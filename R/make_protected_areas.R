#' Create a Raster of Existing Protected Areas Aligned to Planning Units
#'
#' This function generates a raster of protected areas for a given country, aligned to the planning units (`pus`). It optionally downloads the latest protected area data from the [Protected Planet](https://www.protectedplanet.net/) database via the \pkg{wdpar} package or uses an input `sf` object. All geometries are reprojected, dissolved to avoid double-counting overlaps, and converted to individual polygons to improve spatial intersection performance. The raster output represents the fractional area of each planning unit covered by protected areas.
#'
#' @param from_wdpa Logical. If `TRUE`, downloads protected area data using the \pkg{wdpar} package. If `FALSE`, uses `sf_in`.
#' @param iso3 Character. ISO3 code for the country of interest.
#' @param download_path Character. Path where WDPA data will be saved or loaded from.
#' @param sf_in An `sf` object containing protected area geometries. Used only if `from_wdpa = FALSE`.
#' @param status Character vector. Which `STATUS` values to include from the WDPA data. Default is `c("Established", "Inscribed", "Designated")`.
#' @param pa_def Integer or vector of integers. Values for the `PA_DEF` column (1 = Protected Area; 0 = OECM). Only 1 is currently supported. Default is `1`.
#' @param include_mab_designation Logical. If `FALSE`, excludes Man and Biosphere (MAB) reserves.
#' @param buffer_points Logical. If `TRUE`, creates geodesic buffers around `POINT` and `MULTIPOINT` features using area attributes.
#' @param area_column Character. Column name indicating the area (used for buffering). Default is `"REP_AREA"`.
#' @param area_calc_crs Character. CRS to use for buffering operations. Default is `"ESRI:54009"` (World Mollweide).
#' @param nQuadSegs Integer. Number of segments per quarter circle for buffers. Default is `50`.
#' @param return_sf Logical. If `TRUE`, assigns the dissolved individual polygons to `current_protected_area_sf` in the global environment for reuse. Default is `TRUE`.
#' @param force_update Logical. If `TRUE`, forces re-download of WDPA data. Default is `FALSE`.
#' @param pus A `SpatRaster` of planning units to which the protected areas will be aligned.
#' @param output_path Optional. File path where the resulting raster should be saved.
#'
#' @return A `SpatRaster` representing the fractional coverage of protected areas in each planning unit.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'  boundary_in = boundary_dat,
#'  iso3 = "NPL",
#'  iso3_column = "iso3cd"
#'  )
#'
#' pus <- make_planning_units(
#'  boundary_proj = boundary_proj,
#'  pu_size = NULL
#'  )
#'
#' pa_raster <- make_protected_areas(
#'   iso3 = "NPL",
#'   download_path = here::here(),
#'   buffer_points = TRUE,
#'   pus = pus,
#'   force_update = TRUE
#' )
#' }
make_protected_areas <- function(from_wdpa = TRUE,
                                 iso3,
                                 download_path = NULL,
                                 sf_in,
                                 status = c("Established", "Inscribed", "Designated"),
                                 pa_def = 1,
                                 include_mab_designation = FALSE,
                                 buffer_points = TRUE,
                                 area_column = "REP_AREA",
                                 area_calc_crs = "ESRI:54009",
                                 nQuadSegs = 50,
                                 return_sf = TRUE,
                                 force_update = FALSE,
                                 pus,
                                 output_path = NULL) {
  # Ensure WDPA download folder exists
  wdpa_dir <- file.path(download_path, "wdpa_downloads")
  if (!dir.exists(wdpa_dir)) dir.create(wdpa_dir)

  # Load data either from Protected Planet or provided sf object
  if (from_wdpa == TRUE) {
    log_msg("Downloading protected areas using the wdpar package")
    protected_areas <- wdpar::wdpa_fetch(
      iso3,
      wait = TRUE,
      check_version = TRUE,
      force_download = force_update,
      download_dir = wdpa_dir
    )
  } else {
    protected_areas <- sf_in
  }

  # Validate inputs
  assertthat::assert_that(
    all(status %in% c("Designated", "Established", "Inscribed", "Proposed", "Adopted")),
    all(pa_def %in% c(0, 1))
  )

  log_msg(glue::glue("Including {glue::glue_collapse(status, sep = ', ', last = ' and ')} areas only"))

  # Filter by STATUS and PA_DEF
  protected_areas <- protected_areas %>%
    dplyr::filter(.data$STATUS %in% status, .data$PA_DEF %in% pa_def)

  # Remove MAB designated areas if specified
  if (!include_mab_designation) {
    log_msg("Excluding UNESCO Man and Biosphere (MAB) reserve areas")
    protected_areas <- protected_areas %>%
      dplyr::filter(!stringr::str_detect(.data$DESIG, "MAB"))
  }

  # If geometry includes POINT/MULTIPOINT, buffer or filter
  if (any(sf::st_geometry_type(protected_areas) %in% c("POINT", "MULTIPOINT"))) {
    if (buffer_points) {
      log_msg("Creating geodesic buffers around point geometries")
      protected_areas <- convert_points_polygon(
        sf_layer = protected_areas,
        area_crs = area_calc_crs,
        area_attr = area_column,
        nQuadSegs = nQuadSegs
      ) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        dplyr::summarise() %>%
        sf::st_make_valid()
    } else {
      protected_areas <- protected_areas %>%
        dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        sf::st_make_valid()
    }
  } else {
    # If already polygonal, reproject and dissolve
    protected_areas <- protected_areas %>%
      sf::st_transform(sf::st_crs(pus)) %>%
      dplyr::summarise() %>%
      sf::st_make_valid()
  }


  # Rasterize using fractional overlap with planning units
  log_msg("Generating raster from dissolved protected area geometries")
  protected_areas_raster <- exactextractr::coverage_fraction(pus, protected_areas)[[1]] %>%
    terra::mask(pus, maskvalues = 0)

  # Optionally store in global environment for reuse
  if (return_sf) {
    # Always explode to individual polygon features (for faster spatial filtering later)
    protected_areas <- sf::st_cast(protected_areas, "POLYGON")

    log_msg("Current protected areas are available as object `current_protected_area_sf`")
    assign("current_protected_area_sf", protected_areas, envir = .GlobalEnv)
  }

  # Save raster to disk
  if (!is.null(output_path)) {
    terra::writeRaster(
      protected_areas_raster,
      filename = glue::glue("{output_path}/protected_areas_{iso3}.tif"),
      filetype = "COG",
      datatype = "FLT4S",
      gdal = c(
        "COMPRESS=ZSTD",
        "PREDICTOR=3",
        "OVERVIEWS=NONE",
        "NUM_THREADS=ALL_CPUS"
      ),
      overwrite = TRUE
    )
  }

  return(protected_areas_raster)
}

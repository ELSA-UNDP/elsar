#' Create a Binary or Fractional Raster of Protected Areas Aligned to Planning Units
#'
#' This function generates a binary or fractional raster of protected areas for a given country, aligned to the planning units (`pus`). It optionally downloads the latest protected area data from the [Protected Planet](https://www.protectedplanet.net/) database via the \pkg{wdpar} package or uses an input `sf` object. All geometries are reprojected, dissolved to avoid double-counting overlaps, and converted to individual polygons to improve spatial intersection performance. The raster output indicates the presence of protected areas in each planning unit (binary) or the fractional area covered.
#'
#' @param from_wdpa Logical. If `TRUE`, downloads protected area data using the \pkg{wdpar} package. If `FALSE`, uses `sf_in`.
#' @param iso3 Character. ISO3 code for the country of interest.
#' @param download_path Character. Path where WDPA data will be saved or loaded from.
#' @param sf_in An `sf` object containing protected area geometries. Used only if `from_wdpa = FALSE`.
#' @param status Character vector. Which `STATUS` values to include from the WDPA data. Default is `c("Established", "Inscribed", "Designated")`.
#' @param pa_def Integer or vector of integers. Values for the `PA_DEF` column (1 = Protected Area; 0 = OECM). Only `1` is currently supported. Default is `1`.
#' @param include_mab_designation Logical. If `FALSE`, excludes Man and Biosphere (MAB) reserves.
#' @param buffer_points Logical. If `TRUE`, creates geodesic buffers around `POINT` and `MULTIPOINT` features using area attributes.
#' @param area_column Character. Column name indicating the area (used for buffering). Default is `"REP_AREA"`.
#' @param area_calc_crs Character. CRS to use for buffering operations. Default is `"ESRI:54009"` (World Mollweide).
#' @param nQuadSegs Integer. Number of segments per quarter circle for buffers. Default is `50`.
#' @param threshold Numeric. Fractional threshold for considering a planning unit as protected. Default is `0.10`. Used only if `binary = TRUE`.
#' @param binary Logical. If `TRUE`, output will be binary (0/1) based on `threshold`. If `FALSE`, returns fractional coverage raster. Default is `TRUE`.
#' @param return_sf Logical. If `TRUE`, assigns the dissolved individual polygons to `current_protected_area_sf` in the global environment for reuse. Default is `TRUE`.
#' @param force_update Logical. If `TRUE`, forces re-download of WDPA data. Default is `FALSE`.
#' @param pus A `SpatRaster` of planning units to which the protected areas will be aligned.
#' @param output_path Optional. File path where the resulting raster should be saved.
#'
#' @return A `SpatRaster` representing protected areas as binary presence/absence or fractional coverage.
#'
#' @export
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
                                 threshold = 0.10,
                                 binary = TRUE,
                                 return_sf = TRUE,
                                 force_update = FALSE,
                                 pus,
                                 output_path = NULL) {

  # Ensure WDPA download folder exists
  wdpa_dir <- file.path(download_path, "wdpa_downloads")
  if (!dir.exists(wdpa_dir)) dir.create(wdpa_dir)

  # Load data
  if (from_wdpa) {
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

  # Filter data by STATUS and PA_DEF
  protected_areas <- protected_areas %>%
    dplyr::filter(.data$STATUS %in% status, .data$PA_DEF %in% pa_def)

  # Remove MAB designated areas if specified
  if (!include_mab_designation) {
    log_msg("Excluding UNESCO Man and Biosphere (MAB) reserve areas")
    protected_areas <- protected_areas %>%
      dplyr::filter(!stringr::str_detect(.data$DESIG, "MAB"))
  }

  # If geometry includes POINT/MULTIPOINT, buffer or filter
  geom_type <- sf::st_geometry_type(protected_areas)

  if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
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

  # Rasterize with coverage fraction
  log_msg("Generating raster from dissolved protected area geometries")
  protected_areas_raster <- exactextractr::coverage_fraction(pus, protected_areas)[[1]]

  # Binary conversion if requested
  if (binary) {
    threshold <- threshold %||% 0  # Ensure threshold is numeric and not NULL
    protected_areas_raster <- terra::ifel(protected_areas_raster > threshold, 1, 0)
  }

  # Mask to original planning units extent (retain NA where no data)
  protected_areas_raster <- make_normalised_raster(protected_areas_raster, pus = pus, iso3 = iso3)

  # Optionally store in global environment for reuse
  if (return_sf) {
    protected_areas <- sf::st_cast(protected_areas, "POLYGON")
    log_msg("Current protected areas are available as object `current_protected_area_sf`")
    assign("current_protected_area_sf", protected_areas, envir = .GlobalEnv)
  }

  # Save raster
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = protected_areas_raster,
      filename = glue::glue("{output_path}/protected_areas_{iso3}.tif"),
      datatype = if (binary) "INT1U" else "FLT4S"
      )
  }

  return(protected_areas_raster)
}

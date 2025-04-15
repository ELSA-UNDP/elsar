#' Function to create existing protected area raster file.
#'
#' @param from_wdpa logical. If `TRUE`, downloads data from the \pkg{wdpar} package. For more information on the data or package, please consult the [online package documentation](https://prioritizr.github.io/wdpar/).
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param input_path A path that the wdpa data is saved to.
#' @param sf_in An `sf` object and alternative to downloading the data from the \pkg{wdpar} package. Only needed when from_wdpa is `FALSE`.
#' @param status A vector containing which status of protected area should be included. Based on the STATUS field of the wdpa database. Default is `c("Established", "Inscribed", "Designated")`.
#' @param pa_def A value or list of values containing which pa definition (1 = protected area; 0 = OECM (not supported yet)) to include. Default is `1`.
#' @param include_mab_designation logical. If `FALSE`, excludes UNESCO MAB areas.
#' @param buffer_points logical. Only relevant when `"POINT"` or `"MULTIPOINT"` geometries exist in the data. If `TRUE`, creates a circular buffer around `"POINT"` data based on area information data that is then used as polygon data needed for \pkg{exactextractr} calculations.
#' @param area_column A string of the column name with the area information needed for buffer calculations.
#' @param area_calc_crs Character. CRS used for buffering operation (default: `"ESRI:54009"` = World Mollweide).
#' @param nQuadSegs An integer specifying the number of segments to use for buffering. Default is 50.
#' @param return_sf logical. Allows to return `sf`object if needed. Default is FALSE.
#' @param force_update logical. Force downloading an update to the WDPA is a newer version is available. Default is FALSE.
#' @param pus A raster file that contains the reference spatial extent, crs etc.in form of the planning units.
#' @param output_path An optional output path for the created file.
#'
#' @return A `SpatRaster`file that has been aligned to the planning units
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#'
#' current_pas <- make_protected_areas(
#'   iso3 = "NPL",
#'   download_path = here::here(),
#'   buffer_points = TRUE,
#'   pus = pus,
#'   force_update = TRUE
#' )
#' }
make_protected_areas <- function(from_wdpa = TRUE,
                                 iso3,
                                 input_path = NULL,
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
  # Load data from WDPA subdirectory in input_dir (either with wdpar package or locally saved: load outside and then put sf_in here)
  wdpa_dir <- file.path(input_path, "wdpa_downloads")
  if (!dir.exists(wdpa_dir)) dir.create(wdpa_dir)

  if (from_wdpa == TRUE) {
    log_msg("Downloading from Protected Planet using the wdpar package; any existing versions of the WPDA will be updated if a newer version is available if you set `force_update = TRUE`")

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
  # filter for MPAs to be included
  assertthat::assert_that(
    all(status %in% c("Designated", "Established", "Inscribed", "Proposed", "Adopted")),
    all(pa_def %in% c(0, 1))
  )

  log_msg(glue::glue("Including {glue::glue_collapse(status, sep = ', ', last = ' and ')} areas only"))

  protected_areas <- protected_areas %>%
    dplyr::filter(
      .data$STATUS %in% status,
      .data$PA_DEF %in% pa_def
    )

  if (include_mab_designation == FALSE) {
    log_msg("Excuding UNESCO Man and Biosphere (MAB) reserve areas")
    protected_areas <- protected_areas %>%
      dplyr::filter(!stringr::str_detect(.data$DESIG, "MAB")) # Exclude UNESCO MAB areas
  }

  # exactextractr only works with polygon information; need to deal with points
  if (("MULTIPOINT" %in% sf::st_geometry_type(protected_areas)) || ("POINT" %in% sf::st_geometry_type(protected_areas))) {
    if (buffer_points) { # buffer around "POINTS" and make them into polygons
      log_msg("Creating geodesic buffers around any point locations")
      protected_areas <- convert_points_polygon(
        sf_layer = protected_areas,
        area_crs = area_calc_crs,
        area_attr = area_column,
        nQuadSegs = nQuadSegs) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        dplyr::summarise() %>%
        sf::st_make_valid()

    } else { # only keep polygon and multipolygon information
      protected_areas <- protected_areas %>%
        dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        dplyr::summarise() %>%
        sf::st_make_valid()
    }
  } else {
    protected_areas <- protected_areas %>%
      sf::st_transform(sf::st_crs(pus)) %>%
      dplyr::summarise() %>%
      sf::st_make_valid()
    }

  if (return_sf) {
    # Optional: make `current_pas_sf` available globally for reuse
    log_msg("Current protected areas are available as object current_protected_area_sf")
    assign("current_protected_area_sf", protected_areas, envir = .GlobalEnv)
  }

  # get pa values in pus
  protected_areas_raster <- exactextractr::coverage_fraction(pus, protected_areas)[[1]] %>%
    terra::mask(pus, maskvalues = 0)

  # save
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
    )}

  return(protected_areas_raster)
  }

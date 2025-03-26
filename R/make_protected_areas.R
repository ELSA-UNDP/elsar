#' Function to create existing protected area raster file.
#'
#' @param from_wdpa logical. If `TRUE`, downloads data from the \pkg{wdpar} package. For more information on the data or package, please consult the [online package documentation](https://prioritizr.github.io/wdpar/).
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param download_path A path that the wdpa data is saved to.
#' @param sf_in An `sf` object and alternative to downloading the data from the \pkg{wdpar} package. Only needed when from_wdpa is `FALSE`.
#' @param status A vector containing which status of protected area should be included. Based on the STATUS field of the wdpa database. Default is `c("Established", "Inscribed", "Designated")`.
#' @param pa_def A value or list of values containing which pa definition (1 = protected area; 0 = OECM (not supported yet)) to include. Default is `1`.
#' @param designation_mab logical. If `FALSE`, excludes UNESCO MAB areas.
#' @param buffer_points logical. Only relevant when `"POINT"` or `"MULTIPOINT"` geometries exist in the data. If `TRUE`, creates a circular buffer around `"POINT"` data based on area information data that is then used as polygon data needed for \pkg{exactextractr} calculations.
#' @param area_column A string of the column name with the area information needed for buffer calculations.
#' @param nQuadSegs An integer specifying the number of segments to use for buffering. Default is 50.
#' @param return_sf logical. Allows to return `sf`object if needed. Default is FALSE.
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
#'   pus = pus
#' )
#' }
make_protected_areas <- function(from_wdpa = TRUE,
                                 iso3,
                                 download_path = NULL,
                                 sf_in,
                                 status = c("Established", "Inscribed", "Designated"),
                                 pa_def = 1,
                                 designation_mab = FALSE,
                                 buffer_points = TRUE,
                                 area_column = "REP_AREA",
                                 nQuadSegs = 50,
                                 return_sf = FALSE,
                                 pus,
                                 output_path = NULL) {
  # load data (either with wdpar package or locally saved: load outside and then put sf_in here)
  if (from_wdpa == TRUE) {
    pa <- wdpar::wdpa_fetch(iso3,
      wait = TRUE,
      download_dir = download_path
    ) %>%
      sf::st_transform(sf::st_crs(pus))
  } else {
    pa <- sf_in %>%
      sf::st_transform(sf::st_crs(pus))
  }
  # filter for MPAs to be included
  assertthat::assert_that(
    all(status %in% c("Designated", "Established", "Inscribed", "Proposed", "Adopted")),
    all(pa_def %in% c(0, 1))
  )

  pa <- pa %>%
    dplyr::filter(
      .data$STATUS %in% status,
      .data$PA_DEF %in% pa_def
    )

  if (designation_mab == FALSE) {
    pa <- pa %>%
      dplyr::filter(!stringr::str_detect(.data$DESIG, "MAB")) # Exclude UNESCO MAB areas
  }

  # exactextractr only works with polygon information; need to deal with points
  if (("MULTIPOINT" %in% sf::st_geometry_type(pa)) || ("POINT" %in% sf::st_geometry_type(pa))) {
    if (buffer_points) { # buffer around "POINTS" and make them into polygons
      pa <- convert_points_polygon(wdpa_layer = pa,
                                area_crs = sf::st_crs(pa),
                                area_attr = area_column,
                                nQuadSegs = nQuadSegs) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        dplyr::summarise() %>%
        sf::st_make_valid()

    } else { # only keep polygon and multipolygon information
      pa <- pa %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
        dplyr::summarise() %>%
        sf::st_make_valid()
    }
  }

  if (return_sf) {
    return(pa)
  } else {
  # get pa values in pus
  pa_raster <- exactextractr::coverage_fraction(pus, pa)[[1]] %>%
    terra::mask(pus, maskvalues = 0)

  # save
  if (!is.null(output_path)) {
    terra::writeRaster(
      pa_raster,
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

  return(pa_raster)
  }

}

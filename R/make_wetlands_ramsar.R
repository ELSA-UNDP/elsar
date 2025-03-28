#' Create a Standardised Raster for Wetlands and Ramsar Sites
#'
#' This function processes and combines Ramsar and global wetlands data to produce a standardised
#' raster layer aligned to planning units. It supports handling of Ramsar point geometries by buffering
#' them into polygons if required, and allows filtering and normalisation based on user-defined
#' thresholds. Either Ramsar or Wetlands data can be provided, or both.
#'
#' @param ramsar_in An `sf` object containing Ramsar site geometries (optional).
#' @param wetlands_in A `SpatRaster` object representing global wetlands data (optional).
#' @param pus A `SpatRaster` object containing the planning units used for resolution and extent.
#' @param iso3 A character string with the ISO3 country code (e.g., "KEN").
#' @param buffer_points Logical. Only relevant when "POINT" or "MULTIPOINT" geometries exist in `ramsar_in`.
#' If `TRUE`, circular buffers are generated from the area attribute.
#' @param area_column A string indicating the name of the column containing site area (in hectares).
#' Used when buffering point geometries.
#' @param nQuadSegs An integer specifying the number of segments used to approximate circular buffer (default: 50).
#' @param wetland_threshold Numeric. Threshold for binarising wetlands raster (default: 0.25).
#' @param return_all Logical. If `TRUE`, returns a raster stack containing the combined wetlands+ramsar raster,
#' as well as the individual normalised input layers.
#'
#' @return A `SpatRaster` object. If `return_all = TRUE`, returns a raster stack with three layers:
#' combined, ramsar only, and wetlands only.
#' @export
make_wetlands_ramsar <- function(
    ramsar_in = NULL,
    wetlands_in = NULL,
    pus,
    iso3,
    buffer_points = TRUE,
    area_column = "Area (ha)",
    nQuadSegs = 50,
    wetland_threshold = 0.25,
    return_all = FALSE) {
  # To keep consistent with use of iso3 elsewhere, and to avoid issues around filtering
  # on the attribute of the same name in the KBA dataset, we use iso3_filter.
  iso3_filter <- iso3

  if (!is.null(ramsar_in)) {
    log_msg(glue::glue("Filtering Ramsar sites data for iso3 code: {iso3}..."))
    ramsar <- ramsar_in %>%
      dplyr::filter(iso3 == iso3_filter) %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_make_valid()

    if (nrow(ramsar_in) == 0) {
      log_msg("No Ramsar sites in the planning region - returning an empty raster.")
      ramsar <- terra::ifel(pus == 1, 0, NA)
    } else {
      # exactextractr only works with polygon information; need to deal with points
      if (("MULTIPOINT" %in% sf::st_geometry_type(ramsar)) ||
          ("POINT" %in% sf::st_geometry_type(ramsar))) {
        if (buffer_points) {
          # buffer around "POINTS" and make them into polygons
          ramsar <- convert_points_polygon(
            wdpa_layer = ramsar,
            area_crs = sf::st_crs(pus),
            area_attr = area_column,
            nQuadSegs = nQuadSegs,
            area_multiplier = 1e4
          ) %>%
            sf::st_transform(sf::st_crs(pus)) %>%
            dplyr::summarise() %>%
            sf::st_make_valid()
        } else {
          # only keep polygon and multipolygon information

          if (nrow(ramsar %>% dplyr::filter(sf::st_is(., c(
            "POLYGON", "MULTIPOLYGON"
          )))) > 0) {
            ramsar <- ramsar %>%
              sf::st_transform(sf::st_crs(pus)) %>%
              dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
              dplyr::summarise() %>%
              sf::st_make_valid()

            log_msg("Rasterising and normalising Ramsar sites...")
            ramsar <- exactextractr::coverage_fraction(pus, ramsar)[[1]] %>% # problem when point
              elsar::make_normalised_raster(pus = pus, iso3 = iso3)

          } else {
            log_msg(
              "Only 'POINT' or 'MULTIPOINT' geometry type Ramsar sites found in the planning region and 'buffer_points' is set to false."
            )
            log_msg("Returning an empty raster.")

            ramsar <- terra::ifel(pus == 1, 0, NA)
          }
        }
      }
    }
  }

  if (!is.null(wetlands_in)) {
    wetlands <- elsar::make_normalised_raster(
      raster_in = wetlands_in,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(is.na(x), 0, terra::ifel(x > 1, 1, 0)),
      conditional_expression = function(x) terra::ifel(x > wetland_threshold, 1, 0)
      )
    }

  if (!is.null(wetlands) & !is.na(suppressWarnings(terra::minmax(wetlands)[2])) & terra::minmax(wetlands)[2] > 0 & nrow(ramsar_in) > 0) {

    log_msg("Wetlands and Ramsar raster built using Wetlands AND Ramsar data.")
    ramsar_wetlands <- 0.5 * wetlands + ramsar

  } else if (is.null(wetlands_in) & is.na(suppressWarnings(terra::minmax(wetlands)[2])) | terra::minmax(wetlands)[2] == 0 & nrow(ramsar_in) > 0) {

      log_msg("Wetlands and Ramsar raster built using only Ramsar data.")
      ramsar_wetlands <- ramsar

  } else if (!is.null(wetlands_in) & !is.na(suppressWarnings(terra::minmax(wetlands)[2])) & terra::minmax(wetlands)[2] > 0 & nrow(ramsar_in) == 0) {

        log_msg("Wetlands and Ramsar raster built using Wetlands data.")
        ramsar_wetlands <- wetlands

  } else {

    log_msg("Either Wetlands or Ramsar data must be provided.")

  }

  ramsar_wetlands <- ramsar_wetlands %>%
    elsar::make_normalised_raster(
      pus = pus,
      iso3 = iso3
    )

  if ((!is.null(wetlands_in)) & (!is.null(ramsar_in)) & return_all) {

    ramsar_wetlands <- c(
      ramsar_wetlands,
      ramsar %>% terra::subst(NA, 0),
      wetlands %>% terra::subst(NA, 0)
      )

    return(ramsar_wetlands)

    } else {

    return(ramsar_wetlands)

    }
  }

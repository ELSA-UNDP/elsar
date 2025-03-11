#' Function to make standardised Wetlands and Ramsar data
#'
#' Function works with either Ramsar or Wetlands data or both.
#'
#' @param ramsar_in An `sf` file that contains the ramsar data to be put into right format
#' @param wetlands_in  A `SpatRaster` file with the wetlands information
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3_in A string of the iso3 name of the data (country name)
#' @param buffer_points logical. Only relevant when `"POINT"` or `"MULTIPOINT"` geometries exist in the data. If `TRUE`, creates a circular buffer around `"POINT"` data based on area information data that is then used as polygon data needed for \pkg{exactextractr} calculations.
#' @param area_column A string of the column name with the area information needed for buffer calculations.
#' @param nQuadSegs An integer specifying the number of segments to use for buffering. Default is 50.
#' @param return_all Logical. Whether to return a `SpatRaster` with the combined wetlands and raster layer, as well as the individual layers
#'
#' @return A `SpatRaster` file that has been aligned and normalised
#' @export
#'
make_wetlands_ramsar <- function(ramsar_in = NULL,
                                 wetlands_in = NULL,
                                 pus,
                                 iso3_in,
                                 buffer_points = TRUE,
                                 area_column = "Area (ha)",
                                 nQuadSegs = 50,
                                 return_all = FALSE) {
  if (!is.null(ramsar_in)) {
    ramsar_in <- ramsar_in %>%
      dplyr::filter(iso3 == iso3_in) %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_make_valid()

    if (nrow(ramsar_in) == 0) {
      cat("No Ramsar sites in the planning region.")
      raster_ramsar <- pus
    } else {
      # exactextractr only works with polygon information; need to deal with points
      if (("MULTIPOINT" %in% sf::st_geometry_type(ramsar_in)) || ("POINT" %in% sf::st_geometry_type(ramsar_in))) {
        if (buffer_points) { # buffer around "POINTS" and make them into polygons
          ramsar <- convert_points_polygon(
            wdpa_layer = ramsar_in,
            area_crs = sf::st_crs(pus),
            area_attr = area_column,
            nQuadSegs = nQuadSegs,
            area_multiplier = 1e4
          ) %>%
            sf::st_transform(sf::st_crs(pus)) %>%
            dplyr::summarise() %>%
            sf::st_make_valid()
        } else { # only keep polygon and multipolygon information
          ramsar <- ramsar_in %>%
            sf::st_transform(sf::st_crs(ramsar_in)) %>%
            dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
            dplyr::summarise() %>%
            sf::st_make_valid()
        }
      }

      raster_ramsar <- exactextractr::coverage_fraction(pus, ramsar)[[1]] %>% # problem when point
        terra::mask(pus, maskvalues = 0) %>%
        rescale_raster()
    }
  }

  if (!is.null(wetlands_in)) {
    raster_wetlands <- make_normalised_raster(
      raster_in = wetlands_in,
      pus = pus,
      iso3 = iso3,
      conditional_expression = function(r) ifel(r > 1, 1, r)
    )
  }

  if ((!is.null(wetlands_in)) & (!is.null(ramsar_in))) {
    cat("Wetlands and Ramsar calculated on Wetalnds AND Ramsar data.")
    ramsar_wetlands <- (0.5 * raster_wetlands + raster_ramsar) %>%
      terra::app(function(x) ifelse(x > 1, 1, x))
  } else if ((is.null(wetlands_in)) & (!is.null(ramsar_in))) {
    cat("Wetlands and Ramsar only calculated on Ramsar data.")
    ramsar_wetlands <- raster_ramsar
  } else if ((!is.null(wetlands_in)) & (is.null(ramsar_in))) {
    cat("Wetlands and Ramsar only calculated on Wetlands data.")
    ramsar_wetlands <- raster_wetlands
  } else {
    cat("Either Wetlands or Ramsar data must be provided")
  }

  ramsar_wetlands <- ramsar_wetlands %>%
    terra::mask(pus, maskvalues = 0) %>%
    terra::subst(NA, 0)

  if ((!is.null(wetlands_in)) & (!is.null(ramsar_in)) & return_all) {
    ramsar_wetlands <- c(
      ramsar_wetlands, raster_ramsar %>%
        terra::subst(NA, 0),
      raster_wetlands %>%
        terra::subst(NA, 0)
    )
    return(ramsar_wetlands)
  } else {
    return(ramsar_wetlands)
  }
}

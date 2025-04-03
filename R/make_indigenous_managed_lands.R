#' Create Indigenous Managed Lands Raster (LANDMark + ICCA)
#'
#' This function identifies Indigenous- and community-managed lands using two datasets:
#' the LANDMark dataset and the ICCA Registry. It buffers point geometries (if required),
#' combines them, calculates their fractional coverage over a planning unit raster,
#' applies a threshold, and returns a binary raster indicating likely Indigenous management presence.
#'
#' @param sf_landmark sf or NULL. Indigenous and community lands from the LANDMark dataset. Must be subset to the target country.
#' @param sf_icca sf or NULL. ICCA Registry features. Must be subset to the target country.
#' @param iso3 Character. ISO3 country code used for output naming.
#' @param pus SpatRaster. Raster of planning units for alignment and coverage calculations.
#' @param buffer_points Logical. Whether to buffer POINT/MULTIPOINT geometries to create polygon representations. Default is TRUE.
#' @param output_path Character or NULL. Directory to save the final raster as a Cloud Optimized GeoTIFF (COG). If NULL, the file is not written to disk.
#'
#' @return A binary \code{SpatRaster} where:
#' \itemize{
#'   \item \code{1} = area is considered Indigenous-managed (based on coverage)
#'   \item \code{0} = area is not Indigenous-managed
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' indigenous_lands <- make_indigenous_managed_lands(
#'   sf_landmark = landmark_sf,
#'   sf_icca = icca_sf,
#'   iso3 = "COL",
#'   pus = planning_units,
#'   output_path = "outputs/"
#' )
#' terra::plot(indigenous_lands)
#' }
make_indigenous_managed_lands <- function(
    sf_landmark = NULL,
    sf_icca = NULL,
    iso3,
    pus,
    buffer_points = TRUE,
    output_path = NULL
) {
  # Process LANDMark geometries (buffer if points)
  if (!is.null(sf_landmark) && nrow(sf_landmark) > 0) {
    if (any(sf::st_geometry_type(sf_landmark) %in% c("POINT", "MULTIPOINT"))) {
      if (buffer_points) {
        landmark <- convert_points_polygon(
          sf_layer = sf_landmark,
          area_crs = sf::st_crs(pus),
          area_attr = "Area_GIS",
          area_multiplier = 1e4,
          append_original_polygons = TRUE
        )
      } else {
        landmark <- dplyr::filter(sf_landmark, sf::st_is(., c("POLYGON", "MULTIPOLYGON")))
        if (nrow(landmark) == 0) {
          log_msg("Only POINT/MULTIPOINT geometries found in LANDMark and buffering is disabled.")
          log_msg("Returning an empty geometry.")
        }
      }
    } else {
      landmark <- sf_landmark
    }

    landmark <- landmark %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_geometry() %>%
      sf::st_make_valid()
  } else {
    landmark <- sf::st_sfc(crs = sf::st_crs(pus))  # empty geometry
  }

  # Process ICCA geometries (buffer if points)
  if (!is.null(sf_icca) && nrow(sf_icca) > 0) {
    if (any(sf::st_geometry_type(sf_icca) %in% c("POINT", "MULTIPOINT"))) {
      if (buffer_points) {
        icca <- convert_points_polygon(
          sf_layer = sf_icca,
          area_crs = sf::st_crs(pus),
          area_attr = "reported_area",
          area_multiplier = 1e4,
          append_original_polygons = TRUE
        )
      } else {
        icca <- dplyr::filter(sf_icca, sf::st_is(., c("POLYGON", "MULTIPOLYGON")))
        if (nrow(icca) == 0) {
          log_msg("Only POINT/MULTIPOINT geometries found in ICCA and buffering is disabled.")
          log_msg("Returning an empty geometry.")
        }
      }
    } else {
      icca <- sf_icca
    }

    icca <- icca %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_geometry() %>%
      sf::st_make_valid()
  } else {
    icca <- sf::st_sfc(crs = sf::st_crs(pus))  # empty geometry
  }

  # Combine LANDMark and ICCA into a single feature collection
  indigenous_managed_lands <- sf::st_sf(geometry = c(landmark, icca)) %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(.)) %>%
    dplyr::summarise()  # dissolve to one multipolygon

  # Calculate fractional overlap with planning units
  indigenous_managed_lands <- exactextractr::coverage_fraction(pus, indigenous_managed_lands)[[1]] %>%
    elsar::make_normalised_raster(
      pus = pus,
      iso3 = iso3
    )

  # Save to disk if path provided
  if (!is.null(output_path)) {
    output_file <- glue::glue("{output_path}/indigenous_managed_lands_{iso3}.tif")

    terra::writeRaster(
      indigenous_managed_lands,
      filename = output_file,
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

    log_msg(glue::glue("Indigenous managed lands raster created and saved to: {output_file}"))
  }

  return(indigenous_managed_lands)
}

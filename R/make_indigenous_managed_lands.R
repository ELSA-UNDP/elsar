#' Create Indigenous Managed Lands Raster (LANDMark + ICCA)
#'
#' This function identifies Indigenous- and community-managed lands using two datasets:
#' the LANDMark dataset and the ICCA Registry. It buffers point geometries if required,
#' merges both sources, calculates fractional coverage over planning units, applies a threshold,
#' and returns a binary raster indicating areas likely under Indigenous management.
#'
#' @param sf_landmark sf or NULL. LANDMark Indigenous lands dataset, subset to the country of interest.
#' @param sf_icca sf or NULL. ICCA Registry dataset, subset to the country of interest.
#' @param iso3 Character. ISO3 country code for naming.
#' @param pus SpatRaster. Planning unit raster for alignment and coverage calculations.
#' @param buffer_points Logical. Buffer POINT/MULTIPOINT geometries into polygons (default = TRUE).
#' @param output_path Character or NULL. If provided, saves output raster as a COG to this directory.
#'
#' @return A binary \code{SpatRaster} where:
#' \itemize{
#'   \item \code{1} = likely Indigenous-managed
#'   \item \code{0} = not Indigenous-managed
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
  crs_pus <- sf::st_crs(pus)

  # --- Process LANDMark ---
  if (!is.null(sf_landmark) && nrow(sf_landmark) > 0) {
    if (any(sf::st_geometry_type(sf_landmark) %in% c("POINT", "MULTIPOINT"))) {
      if (buffer_points) {
        landmark <- convert_points_polygon(
          sf_layer = sf_landmark,
          area_crs = crs_pus,
          area_attr = "Area_GIS",
          area_multiplier = 1e4,
          append_original_polygons = TRUE
        )
      } else {
        landmark <- sf_landmark %>%
          dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON")))
        if (nrow(landmark) == 0) {
          log_msg("Only POINT/MULTIPOINT geometries found in LANDMark and buffering is disabled.")
        }
      }
    } else {
      landmark <- sf_landmark
    }

    landmark <- landmark %>%
      sf::st_transform(crs = crs_pus) %>%
      sf::st_geometry() %>%
      sf::st_make_valid() %>%
      sf::st_as_sf()
  } else {
    landmark <- sf::st_sfc(crs = crs_pus)  # empty geometry
  }

  # --- Process ICCA ---
  if (!is.null(sf_icca) && nrow(sf_icca) > 0) {
    if (any(sf::st_geometry_type(sf_icca) %in% c("POINT", "MULTIPOINT"))) {
      if (buffer_points) {
        icca <- convert_points_polygon(
          sf_layer = sf_icca,
          area_crs = crs_pus,
          area_attr = "reported_area",
          area_multiplier = 1e4,
          append_original_polygons = TRUE
        )
      } else {
        icca <- sf_icca %>%
          dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON")))
        if (nrow(icca) == 0) {
          log_msg("Only POINT/MULTIPOINT geometries found in ICCA and buffering is disabled.")
        }
      }
    } else {
      icca <- sf_icca
    }

    icca <- icca %>%
      sf::st_transform(crs = crs_pus) %>%
      sf::st_geometry() %>%
      sf::st_make_valid() %>%
      sf::st_as_sf()
  } else {
    icca <- sf::st_sfc(crs = crs_pus)  # empty geometry
  }

  # --- Combine and Calculate Raster ---
  has_landmark <- inherits(landmark, "sf") && nrow(landmark) > 0
  has_icca     <- inherits(icca, "sf") && nrow(icca) > 0

  if (has_landmark || has_icca) {
    combined <- dplyr::bind_rows(
      if (has_landmark) landmark else NULL,
      if (has_icca) icca else NULL
    ) %>%
      sf::st_make_valid() %>%
      dplyr::filter(!sf::st_is_empty(.)) %>%
      dplyr::summarise()

    indigenous_managed_lands <- exactextractr::coverage_fraction(pus, combined)[[1]] %>%
      elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  } else {
    log_msg("No matching LANDMark or ICCA Registry features found in the study region: returning empty raster.")
    indigenous_managed_lands <- terra::ifel(pus == 1, 0, NA)
  }

  # --- Optional: Save Raster ---
  if (!is.null(output_path)) {
    output_file <- glue::glue("{output_path}/indigenous_managed_lands_{iso3}.tif")

    elsar::save_raster(
      raster = indigenous_managed_lands,
      filename = output_file,
      datatype = "FLT4S"
    )
  }

  return(indigenous_managed_lands)
}

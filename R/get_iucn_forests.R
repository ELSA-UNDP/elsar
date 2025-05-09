#' Extract and Rasterise IUCN Forest Ecosystems from Preloaded Layer
#'
#' This function filters IUCN GET polygons (already loaded as `sf`) to include only forest
#' ecosystems, merges them, and calculates proportional coverage across each planning unit.
#' The result is normalized to a 0–1 scale using `elsar::make_normalised_raster()`.
#'
#' Optionally, the result can be written to a Cloud-Optimized GeoTIFF file.
#'
#' @param iucn_get_sf sf object. IUCN GET polygons already loaded and pre-clipped to the boundary.
#' @param iso3 Character. ISO3 country code, used for naming and passed to `make_normalised_raster()`.
#' @param pus SpatRaster. Planning units raster over which forest coverage is computed.
#' @param boundary_layer sf object. Used to spatially clip features if necessary.
#' @param include_minor_occurrence Logical. If FALSE, removes minor occurrence ecosystems.
#' @param iucn_get_prefixes Character vector of layer prefixes to retain (default is forest-related).
#' @param excluded_prefixes Optional character vector of prefixes to exclude (e.g., `"T7.1"`).
#' @param output_path Optional character. Output directory to save raster.
#'
#' @return A normalized `SpatRaster` showing fractional IUCN forest coverage across PUs.
#' @export
#'
#' @examples
#' \dontrun{
#' iucn_data <- get_iucn_ecosystems(...)
#' forest_layer <- get_iucn_forests_from_layer(
#'   iucn_get_sf = iucn_data,
#'   pus = pus,
#'   iso3 = "KEN",
#'   boundary_layer = boundary,
#'   excluded_prefixes = c("T7.1", "T7.2"),
#'   output_path = "outputs"
#' )
#' }

get_iucn_forests <- function(
    iucn_get_sf,
    iso3,
    pus,
    boundary_layer,
    include_minor_occurrence = TRUE,
    iucn_get_prefixes = c("MFT1.2", "T1.1", "T1.2", "T1.3", "T1.4", "T2.1", "T2.2", "T2.3", "T2.4", "T2.5", "T2.6", "TF1.1", "TF1.2"),
    excluded_prefixes = NULL,
    output_path = NULL
) {
  # Input checks
  assertthat::assert_that(inherits(iucn_get_sf, "sf"))
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(boundary_layer, "sf"))

  # Filter by prefixes
  if (!is.null(iucn_get_prefixes)) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, get_id %in% iucn_get_prefixes)
  }
  if (!include_minor_occurrence) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, occurrence != 1)
  }
  if (!is.null(excluded_prefixes)) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, !get_id %in% excluded_prefixes)
  }

  # If no data remains, return a zero raster
  if (nrow(iucn_get_sf) == 0) {
    warning("No forest ecosystems remain after filtering. Returning 0-valued raster.")
    forest_raster <- terra::ifel(pus == 1, 0, NA)
  } else {
    # Dissolve into a single geometry
    iucn_get_sf <- dplyr::summarise(iucn_get_sf)

    # Compute coverage fraction
    log_msg("Calculating forest coverage fractions...")
    forest_raster <- exactextractr::coverage_fraction(pus, iucn_get_sf)[[1]] %>%
      elsar::make_normalised_raster(
        pus = pus,
        iso3 = iso3
      )
  }

  names(forest_raster) <- "iucn_forests"

  # Optional: write to file
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/iucn_get_forests_{iso3}.tif")

    elsar::save_raster(
      raster = forest_raster,
      filename = out_file,
      datatype = "FLT4S"
    )
  }

  return(forest_raster)
}

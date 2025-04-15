#' Create Threatened Ecosystems (for Restoration) Raster Based on Intactness and Degradation
#'
#' This function identifies areas that are both ecologically threatened and degraded,
#' representing priority targets for ecological restoration. It filters the threat values
#' from `threatened_ecosystems_input` to retain only those pixels that overlap with degraded
#' areas indicated in `degradation_input`. The output is a continuous raster with threat values
#' masked to degraded zones.
#'
#' @param iso3 Character. ISO3 country code (e.g., "KEN") used for naming and processing.
#' @param pus SpatRaster. Planning units raster used for resolution and extent.
#' @param threatened_ecosystems_input SpatRaster. Raster from `make_threatened_ecosystems()` containing ecosystem threat values.
#' @param degradation_input SpatRaster. Raster from `make_restore_zone()` indicating degraded areas.
#' @param output_path Character or NULL. Optional output directory to save the resulting raster.
#'
#' @return A SpatRaster containing threat values masked to degraded areas only.
#' @export
#'
#' @examples
#' \dontrun{
#' threatened_restoration <- make_threatened_ecosystems_restoration(
#'   iso3 = "KEN",
#'   pus = planning_units,
#'   threatened_ecosystems_input = rast("outputs/threatened_ecosystems_for_protection_KEN.tif"),
#'   degradation_input = rast("outputs/restore_zones_KEN.tif"),
#'   output_path = "outputs"
#' )
#' }
make_threatened_ecosystems_restoration <- function(
    iso3,
    pus,
    threatened_ecosystems_input = NULL,
    degradation_input = NULL,
    output_path = NULL
) {
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(threatened_ecosystems_input, "SpatRaster"),
                          msg = "'threatened_ecosystems_input' must be a SpatRaster.")
  assertthat::assert_that(inherits(degradation_input, "SpatRaster"),
                          msg = "'degradation_input' must be a SpatRaster.")

  # Retain threat values only where degraded areas exist
  log_msg("Finding overlap of threatened ecosystems and degraded areas...")
  threatened_ecosystems_for_restoration <- threatened_ecosystems_input * degradation_input

  names(threatened_ecosystems_for_restoration) <- "threatened_ecosystems_for_restoration"

  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/threatened_ecosystems_for_restoration_{iso3}.tif")
    log_msg(glue::glue("Writing output to: {out_file}"))

    terra::writeRaster(
      threatened_ecosystems_for_restoration,
      filename = out_file,
      datatype = "FLT4S",
      filetype = "COG",
      gdal = c(
        "COMPRESS=ZSTD",
        "PREDICTOR=3",
        "NUM_THREADS=ALL_CPUS",
        "OVERVIEWS=NONE"
      ),
      overwrite = TRUE
    )
  }

  return(threatened_ecosystems_for_restoration)
}

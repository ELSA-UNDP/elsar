#' Create Indigenous Managed Lands Raster (LANDMark + ICCA)
#'
#' This function identifies Indigenous- and community-managed lands using two inputs:
#' the LANDMark dataset and the ICCA Registry. It calculates the fractional coverage
#' of each input over the planning units (`pus`), applies a threshold, and returns
#' a binary raster indicating where Indigenous management presence is likely.
#'
#' The two input layers must be pre-processed and subset to the target country.
#' Thresholding allows some flexibility for precision.
#'
#' @param landmark_input SpatVector or sf. Indigenous and community lands from the LANDMark dataset.
#' @param icca_input SpatVector or sf. ICCA Registry polygons (post-Feb 2025 update).
#' @param iso3 Character. ISO3 country code used for naming and normalisation.
#' @param pus SpatRaster. Planning units raster used for resolution and extent.
#' @param coverage_threshold Numeric. Fractional area (0–1) threshold to classify as Indigenous managed (default: 0.1).
#' @param output_path Character or NULL. If provided, writes the output raster as a Cloud Optimized GeoTIFF.
#'
#' @return A SpatRaster with values of 1 (Indigenous managed) and 0 (not).
#' @export
#'
#' @examples
#' \dontrun{
#' indigenous_lands <- make_indigenous_managed_lands(
#'   landmark_input = landmark_sf,
#'   icca_input = icca_sf,
#'   iso3 = "COL",
#'   pus = planning_units,
#'   output_path = "outputs"
#' )
#' }
make_indigenous_managed_lands <- function(
    landmark_input = NULL,
    icca_input = NULL,
    iso3,
    pus,
    coverage_threshold = 0.1,
    output_path = NULL
) {

  # #### TO DO:
  # - Add in point buffering for point locations - based on reported area
  # - Options for using the several Landmark datasets, or just a subset
  # - Options for subsetting the ICCA dataset.

  # Calculate fractional coverage of LANDMark data
  landmark <- exactextractr::coverage_fraction(pus, landmark_input)[[1]] |>
    elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  # Calculate fractional coverage of ICCA data
  icca <- exactextractr::coverage_fraction(pus, icca_input)[[1]] |>
    elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  # Classify as Indigenous Managed if coverage in either > threshold
  indigenous_managed_lands <- terra::ifel(
    landmark > coverage_threshold | icca > coverage_threshold,
    1, 0
  )

  names(indigenous_managed_lands) <- "indigenous_managed_lands"

  # Optionally write to GeoTIFF
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

    cat(glue::glue("Indigenous managed lands raster created and saved to: {output_file}\n"))
  }

  return(indigenous_managed_lands)
}

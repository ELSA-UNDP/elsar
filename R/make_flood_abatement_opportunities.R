#' Create a Flood Abatement Opportunities Raster
#'
#' This function generates a raster that identifies flood abatement opportunities
#' by integrating the Global Flood Database (GFD) and NDVI (Normalized Difference Vegetation Index).
#' The function normalizes NDVI and flood risk data, then calculates potential areas for flood
#' abatement based on their relationship.
#'
#' @param gfd_raster A `SpatRaster` representing flood risk data from the Global Flood Database.
#' @param ndvi_raster A `SpatRaster` representing NDVI data.
#' @param pus A `SpatRaster` defining the planning unit (PU) grid.
#' @param iso3 A character string representing the ISO3 country code.
#' @param output_path A character string specifying the output directory for saving the raster (optional).
#' @param threads A logical value indicating whether to use multi-threaded processing where supported (default: `TRUE`).
#'
#' @return A `SpatRaster` representing flood abatement opportunities, rescaled between 0 and 1.
#' If `output_path` is provided, the raster is saved as a Cloud-Optimized GeoTIFF (COG).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flood_abatement <- make_flood_abatement_opportunities(
#'   gfd_raster = terra::rast("gfd_flood_database.tif"),
#'   ndvi_raster = terra::rast("mod13q1_2022_ndvi_wgs84.tif"),
#'   pus = planning_units,
#'   iso3 = "NPL",
#'   output_path = "path/to/output"
#' )
#' }
make_flood_abatement_opportunities <- function(
    gfd_raster,
    ndvi_raster,
    pus,
    iso3,
    output_path = NULL,
    threads = TRUE) {

  # Normalize NDVI
  log_msg("Normalizing NDVI raster and removing negative (e.g., water) values...")
  ndvi <- elsar::make_normalised_raster(
    raster_in = ndvi_raster,
    pus = pus,
    iso3 = iso3,
    conditional_expression = function(x) terra::ifel(x < 0, NA, 1 - x)
  )

  # Normalize Global Flood Database
  log_msg("Normalizing Global Flood Database...")
  gfd_risk <- elsar::make_normalised_raster(
    raster_in = gfd_raster,
    pus = pus,
    iso3 = iso3,
    method_override = "mean"
  )

  # Calculate flood abatement opportunities
  flood_abatement_opportunities <- terra::ifel(ndvi == 0, 0, gfd_risk / ndvi) %>%
    elsar::rescale_raster()

  names(flood_abatement_opportunities) <- "flood_abatement_opportunities"

  # Save output if path is provided
  if (!is.null(output_path)) {
    output_file <- glue::glue("{output_path}/flood_abatement_opportunities_{iso3}.tif")

    terra::writeRaster(
      flood_abatement_opportunities,
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

    log_msg(glue::glue("Flood abatement opportunities raster created and saved to: {output_file}"))
  }

  return(flood_abatement_opportunities)
}

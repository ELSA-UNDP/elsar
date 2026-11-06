#' Create a Flood Abatement Opportunities Raster
#'
#' Identifies areas where natural vegetation can reduce flood risk: vegetated
#' land in flood-prone places. Flood risk (Global Flood Database) and vegetation
#' (NDVI) are each normalised to 0-1, multiplied, and rescaled:
#'
#' `flood_abatement = rescale(flood_risk * vegetation)`
#'
#' Water (NDVI < 0) scores 0. Before 0.4.2 the layer was
#' `flood_risk / (1 - vegetation)`, which pointed the same way but grew without
#' bound for dense vegetation, so a few cells took the whole range.
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

  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Vegetation from NDVI (0-1); water (NDVI < 0) has none
  log_message("Normalizing NDVI raster and removing negative (e.g., water) values...")
  vegetation <- make_normalised_raster(
    raster_in = ndvi_raster,
    pus = pus,
    iso3 = iso3,
    conditional_expression = function(x) terra::ifel(x < 0, NA, x)
  )

  # Normalize Global Flood Database
  log_message("Normalizing Global Flood Database...")
  gfd_risk <- make_normalised_raster(
    raster_in = gfd_raster,
    pus = pus,
    iso3 = iso3,
    method_override = "mean"
  )

  # Calculate flood abatement opportunities
  flood_abatement_opportunities <- rescale_raster(gfd_risk * vegetation)

  names(flood_abatement_opportunities) <- "flood_abatement_opportunities"

  # Save output if path is provided
  if (!is.null(output_path)) {
    output_file <- glue::glue("{output_path}/flood_abatement_opportunities_{iso3}.tif")

    elsar::save_raster(
      raster = flood_abatement_opportunities,
      filename = output_file,
      datatype = "FLT4S"
    )
  }

  return(flood_abatement_opportunities)
}

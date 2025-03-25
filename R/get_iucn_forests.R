#' Extract and Rasterise IUCN GET Forest Ecosystems
#'
#' This function extracts forest ecosystem vector layers from IUCN GET `.gpkg` files
#' (those starting with `"T"`), merges and dissolves them, and calculates the proportional
#' coverage of forests across each planning unit using `exactextractr`. The result is then
#' normalized to a 0–1 scale using `elsar::make_normalised_raster()`.
#'
#' Optionally, the result can be written to a Cloud-Optimized GeoTIFF file.
#'
#' @param iucn_get_directory Character. Path to the directory containing the IUCN `.gpkg` files.
#' @param pus SpatRaster. The planning units raster (terra object) over which to compute forest coverage.
#' @param iso3 Character. ISO3 country code, used for naming and passed to `make_normalised_raster()`.
#' @param include_minor_occurence Logical. Whether to include polygons marked as minor occurrence (default = TRUE).
#' @param output_path Optional character. If provided, the output raster is saved to this directory as a GeoTIFF.
#'
#' @return A normalized `SpatRaster` showing fractional IUCN forest coverage across planning units.
#' @export
#'
#' @examples
#' \dontrun{
#' pus <- terra::rast("data/pus.tif")
#' forest_layer <- get_iucn_forests(
#'   iucn_get_directory = "data/iucn_layers",
#'   pus = pus,
#'   iso3 = "KEN",
#'   output_path = "outputs"
#' )
#' }

get_iucn_forests <- function(
    iucn_get_directory,
    iso3,
    pus,
    iucn_get_prefixes = "T",
    include_minor_occurrence = TRUE,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory))
  assertthat::assert_that(dir.exists(iucn_get_directory))
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a valid ISO3 code, e.g., 'NPL'.")

  # Get forest ecosystems from IUCN GET (prefix "T")
  cat("Collecting IUCN GET forest ecosystems (prefix = 'T')...\n")
  iucn_forests <- elsar::get_iucn_ecosystems(
    iucn_get_directory = iucn_get_directory,
    iso3 = iso3,
    boundary_layer = boundary_layer,
    pus = pus,
    iucn_get_prefixes = iucn_get_prefixes,
    include_minor_occurrence = include_minor_occurrence,
    output_path = NULL  # Don't write intermediate vector layer here
  )

  # If no data returned, exit early
  if (is.null(iucn_forests)) {
    warning("No IUCN forest ecosystems found for the specified area.")
    return(NULL)
  }

  # Dissolve all features into a single geometry
  iucn_forests <- dplyr::summarise(iucn_forests)

  # Compute forest coverage fraction within each planning unit
  cat("Calculating coverage fractions...\n")
  forest_raster <- exactextractr::coverage_fraction(pus, iucn_forests)[[1]] |>
    elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  # Optionally write raster to disk
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/iucn_get_forests_{iso3}.tif")
    cat(glue::glue("Writing output to: {out_file}"), "\n")

    terra::writeRaster(
      forest_raster,
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

  return(forest_raster)
}

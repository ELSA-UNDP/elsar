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
#' @param boundary_layer sf object. Vector polygon used to spatially clip features (usually country boundary).
#' @param include_minor_occurrence Logical. Whether to include polygons marked as minor occurrence (default = TRUE).
#' @param iucn_get_prefixes Character vector of filename prefixes to include all forest
#'    classes (e.g., c("MFT1.2", "T1.1", "T1.2", "T1.3", "T1.4", "T2.1", "T2.2", "T2.3", "T2.4",
#'    "T2.5", "T2.6", "TF1.1", "TF1.2")) or NULL to include all `.gpkg` files.
#' @param output_path Optional character. If provided, the output raster is saved to this directory as a GeoTIFF.
#' @param boundary_layer Boundary of the planning region.
#'
#' @return A normalized `SpatRaster` showing fractional IUCN forest coverage across planning units.
#' @export
#'
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
    boundary_layer,
    include_minor_occurrence = TRUE,
    iucn_get_prefixes = c("MFT1.2", "T1.1", "T1.2", "T1.3", "T1.4", "T2.1", "T2.2", "T2.3", "T2.4", "T2.5", "T2.6", "TF1.1", "TF1.2"),
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory), dir.exists(iucn_get_directory))
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a valid ISO3 code, e.g., 'NPL'.")
  assertthat::assert_that(inherits(boundary_layer, "sf"))

  # Get forest ecosystems from IUCN GET (prefix "T")
  log_msg("Collecting IUCN GET forest ecosystems (prefix = 'T').")
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
    warning("No IUCN forest ecosystems found for the specified area. Returning an empty raster.")
    iucn_forests <- terra::ifel(pus == 1, 0, NA)
  } else {
    # Dissolve all features into a single geometry
    iucn_forests <- dplyr::summarise(iucn_forests)

    # Compute forest coverage fraction within each planning unit
    log_msg("Calculating coverage fractions...")
    iucn_forests <- exactextractr::coverage_fraction(pus, iucn_forests)[[1]] %>%
      elsar::make_normalised_raster(
        pus = pus,
        iso3 = iso3
        )
  }

  names(iucn_forests) <- "iucn_forests"

   # Optionally write raster to disk
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/iucn_get_forests_{iso3}.tif")
    log_msg(glue::glue("Writing output to: {out_file}"))

    terra::writeRaster(
      iucn_forests,
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

  return(iucn_forests)
}

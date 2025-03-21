#' Extract and rasterise IUCN GET forest ecosystems vector layers
#'
#' This function reads and merges all `.gpkg` files in a specified directory
#' that start with `T1`, `T2`, etc. (but not `TF`) — these represent forest classes
#' in the IUCN GET Methodology — and intersect with the extent of a planning units
#' raster. The merged features are used to compute coverage fractions over the
#' planning units using `exactextractr`, and then normalized via
#' `elsar::make_normalised_raster()`. If `output_path` is provided, the result
#' will be written to a GeoTIFF.
#'
#' @param iucn_get_directory Character. Path to the directory containing the IUCN `.gpkg` files.
#' @param pus SpatRaster. The planning units raster (terra object) over which to compute forest coverage.
#' @param iso3 Character. ISO3 country code, passed to `make_normalised_raster()`.
#' @param output_path Optional character. Path to save the output raster as a GeoTIFF. If `NULL` (default), the raster is not written to disk.
#'
#' @return A normalized raster of IUCN forest coverage aligned to the input planning units.
#' @export
#'
#' @import sf
#' @import terra
#' @import assertthat
#' @import exactextractr
#' @importFrom elsar make_normalised_raster
#'
#' @examples
#' pus <- terra::rast("path/to/planning_units.tif")
#' forests <- get_iucn_forests("data/iucn_layers/", pus, iso3 = "KEN", output_path = "forest_cover.tif")

get_iucn_forests <- function(iucn_get_directory, pus, iso3, output_path = NULL) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory))
  assertthat::assert_that(dir.exists(iucn_get_directory))
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a a valid string, e.g., 'NPL'")

  # List relevant .gpkg files
  all_files <- list.files(
    iucn_get_directory,
    pattern = "^T[0-9]+.*\\.gpkg$",
    full.names = TRUE
  )

  # Assert we found at least one valid file
  assertthat::assert_that(length(all_files) > 0,
              msg = paste("No matching '.gpkg' files found in", iucn_get_directory))

  # Convert raster extent to WGS84 sf polygon for filtering
  pus_bbox <- terra::as.polygons(terra::ext(pus)) |>
    sf::st_as_sf()
  sf::st_crs(pus_bbox) <- terra::crs(pus)
  pus_bbox <- sf::st_transform(pus_bbox, crs = "EPSG:4326")

  # Read intersecting layers
  cat("Reading and intersecting IUCN GET Ecosystems forest-type layers...\n")
  iucn_forest_list <- lapply(all_files, function(file) {
    terra::vect(file, extent = pus_bbox)
  })

  # Remove empty layers
  iucn_forest_list <- Filter(NROW, iucn_forest_list)

  if (length(iucn_forest_list) == 0) {
    cat("No intersecting features found in any file.\n")
    return(NULL)
  }

  # Merge features and convert to sf
  iucn_forests <- do.call(rbind, iucn_forest_list) |>
    sf::st_as_sf()

  # Reproject to match `pus` if needed
  if (sf::st_crs(iucn_forests) != terra::crs(pus)) {
    iucn_forests <- sf::st_transform(iucn_forests, terra::crs(pus))
  }

  # Dissolve forest geometries
  iucn_forests <- dplyr::summarise(iucn_forests)

  # Compute coverage fraction and normalize
  cat("Calculating coverage fractions...\n")
  forest_raster <- exactextractr::coverage_fraction(pus, iucn_forests)[[1]] |>
    elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  # Optionally write output to GeoTIFF
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    cat(glue::glue("Writing output to: {output_path}."), "\n")
    terra::writeRaster(
      forest_raster,
      filename = glue::glue("{output_path}/iucn_get_forests_{iso3}.tif"),
      datatype = "FLT4S",
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

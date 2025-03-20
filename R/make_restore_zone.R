#' Generate a Degraded Areas Layer for Restoration Planning
#'
#' This function identifies degraded areas for potential restoration efforts. It integrates multiple input layers, including:
#' - **Productivity degradation data (SDG degradation)**
#' - **Land use/land cover (LULC) for agriculture and built-up areas**
#' - **Human Industrial Footprint Index (HII)**
#' The function applies user-defined thresholds to classify degraded areas and optionally filters small patches to remove noise.
#' Intermediate layers (agriculture, built-up areas, and HII) can also be saved if `output_path` is provided.
#'
#' @param iso3 A character string representing the ISO3 country code (e.g., "CHL" for Chile).
#' @param pus A `SpatRaster` defining the planning units grid.
#' @param sdg_degradation_input A `SpatRaster` representing productivity degradation data (default: `NULL`).
#' @param agri_raster A `SpatRaster` of land use/land cover (LULC) data (default: `NULL`) filtered for agricultural areas. Can be processed or unprocessed (if unprocessed: can be the same input as built_raster)
#' @param built_raster A `SpatRaster` of land use/land cover (LULC) data (default: `NULL`) filtered for built-up areas. Can be processed or unprocessed (if unprocessed: can be the same input as agri_raster)
#' @param hii_input A `SpatRaster` of the Human Industrial Footprint Index (HII) (default: `NULL`).
#' @param sdg_threshold A numeric threshold for productivity degradation classification (default: `0.1`).
#' @param lulc_threshold A numeric threshold for agriculture and built-up areas classification (default: `0.1`).
#' @param hii_threshold A numeric threshold for identifying human impact areas in HII (default: `4`).
#' @param agriculture_lulc_value The LULC classification value representing agricultural areas, based on the ESRI 10m LULC dataset (default: `4`).
#' @param built_area_lulc_value The LULC classification value representing built-up areas, based on the ESRI 10m LULC dataset (default: `7`).
#' @param filter_patch_size Logical; if `TRUE`, small patches below `min_patch_size` are removed (default: `TRUE`).
#' @param min_patch_size The minimum patch size (in pixels) to retain during filtering (default: `10`).
#' @param output_path A character string specifying the directory to save output rasters (default: `NULL`, i.e., not saved).
#' @param threads Logical; whether to enable multi-threaded processing where available (default: `TRUE`).
#'
#' @return A `SpatRaster` object representing degraded areas for restoration, classified as `1` (degraded) and `0` (not degraded).
#' If `output_path` is provided, the raster is saved as a Cloud-Optimized GeoTIFF (COG).
#'
#' @import terra
#' @import glue
#' @import assertthat
#' @import elsar
#'
#' @export
#'
#' @examples
#' \dontrun{
#' restore_zone <- make_restore_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   sdg_degradation_input = sdg_raster,
#'   lulc_raster = landcover_raster,
#'   hii_input = hii_raster,
#'   output_path = "path/to/output"
#' )
#' }
make_restore_zone <- function(
    iso3,
    pus,
    sdg_degradation_input = NULL,
    #lulc_raster = NULL,
    agri_raster = NULL,
    built_raster = NULL,
    hii_input = NULL,
    sdg_threshold = 0.1,
    lulc_threshold = 0.1,
    hii_threshold = 4,
    agriculture_lulc_value = 4,
    built_area_lulc_value = 7,
    filter_patch_size = TRUE,
    min_patch_size = 10,
    output_path = NULL,
    threads = TRUE) {

  # Ensure required inputs are provided
  if (is.null(sdg_degradation_input) || is.null(hii_input) || is.null(agri_raster) || is.null(built_raster)) {
    stop("All required input rasters (sdg_degradation_input, hii_input, lulc_raster) must be provided.")
  }

  # Validate input types
  assertthat::assert_that(inherits(sdg_degradation_input, "SpatRaster"), msg = "'sdg_degradation_input' must be a SpatRaster.")
  assertthat::assert_that(inherits(agri_raster, "SpatRaster"), msg = "'agri_raster' must be a SpatRaster.")
  assertthat::assert_that(inherits(built_raster, "SpatRaster"), msg = "'built_raster' must be a SpatRaster.")
  assertthat::assert_that(inherits(hii_input, "SpatRaster"), msg = "'hii_input' must be a SpatRaster.")

  # Normalize and reclassify SDG degradation layer
  cat("Processing SDG degradation layer...\n")
  sdg_degraded_areas <- elsar::make_normalised_raster(
    raster_in = sdg_degradation_input,
    pus = pus,
    iso3 = iso3,
    method = "bilinear",
    input_raster_conditional_expression = function(x) terra::ifel(x == -1, 1, 0),
    threads = threads
  )

  # Extracting Agricultural Areas from LULC
  cat("Extracting agricultural areas from LULC raster...\n")
  if (terra::minmax(agri_raster)[[2]] <= 1) {
    agricultural_areas <- agri_raster
  } else{
  agricultural_areas <- make_normalised_raster(
    raster_in = agri_raster,
    pus = pus,
    iso3 = iso3,
    method_override = "bilinear",
    input_raster_conditional_expression = function(x) terra::ifel(x == agriculture_lulc_value, 1, 0)
  )
}
  # Extracting Built-Up Areas from LULC
  cat("Extracting built areas from LULC raster...\n")
  if (terra::minmax(built_raster)[[2]] <= 1) {
    built_areas <- built_raster
  } else{
  built_areas <- make_normalised_raster(
    raster_in = built_raster,
    pus = pus,
    iso3 = iso3,
    method_override = "bilinear",
    input_raster_conditional_expression = function(x) terra::ifel(x == built_area_lulc_value, 1, 0)
  )
  }

  # Resample and align HII raster
  cat("Processing Human Industrial Footprint Index (HII)...\n")
  hii_resampled <- make_normalised_raster(
    raster_in = hii_input,
    pus = pus,
    iso3 = iso3,
    rescale = FALSE,
    method_override = "bilinear"
  )

  # Save intermediate outputs
  save_raster <- function(raster, filename, datatype = "FLT4S") {
    # Determine the correct predictor value based on datatype
    predictor_value <- ifelse(datatype == "FLT4S", "3", "1")

    terra::writeRaster(
      raster,
      filename = filename,
      filetype = "COG",
      datatype = datatype,
      gdal = c(
        "COMPRESS=ZSTD",
        glue::glue("PREDICTOR={predictor_value}"),  # Set appropriate predictor
        "NUM_THREADS=ALL_CPUS",
        "OVERVIEWS=NONE"
      ),
      overwrite = TRUE
    )

    cat(glue::glue("Saved: {filename}"), "\n")
  }

  if (!is.null(output_path)) {
    save_raster(agricultural_areas, glue::glue("{output_path}/agriculture_areas_{iso3}.tif"))
    save_raster(built_areas, glue::glue("{output_path}/built_areas_{iso3}.tif"))
    save_raster(hii_resampled, glue::glue("{output_path}/hii_{iso3}.tif"))
  }

  # Create degraded areas mask
  restore_zone <- terra::ifel(
    agricultural_areas > lulc_threshold |
      built_areas > lulc_threshold |
      sdg_degraded_areas > sdg_threshold |
      hii_resampled >= hii_threshold,
    1, 0
  )

  if (filter_patch_size) {
    restore_zone <- terra::sieve(restore_zone, threshold = min_patch_size)
  }

  names(restore_zone) <- "restore_zone"

  # Save final output
  if (!is.null(output_path)) {
    save_raster(
      restore_zone,
      glue::glue("{output_path}/restore_zone_{iso3}.tif"),
      datatype = "INT1U")
  }

  return(restore_zone)
}

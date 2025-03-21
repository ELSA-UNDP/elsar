#' Generate Sustainable Management Zone
#'
#' Manage zone: 0<HFP<10 (middle 60% of whole country), include all managed forests and agricultural areas, NOT include urban areas#####
#'
#' @param iso3 A character string representing the ISO3 country code (e.g., "CHL" for Chile).
#' @param pus A `SpatRaster` defining the planning units grid.
#' @param agri_raster A `SpatRaster` of land use/land cover (LULC) data (default: `NULL`) filtered for agricultural areas. Can be processed or unprocessed (if unprocessed: can be the same input as built_raster)
#' @param built_raster A `SpatRaster` of land use/land cover (LULC) data (default: `NULL`) filtered for built-up areas. Can be processed or unprocessed (if unprocessed: can be the same input as agri_raster)
#' @param hfp_raster A `SpatRaster` of the Human Footprint Index (HFP) (default: `NULL`).
#' @param mf_raster A `SpatRaster` of Managed Forests (default: `NULL`).
#' @param lulc_threshold A numeric threshold for agriculture and built-up areas classification (default: `0.1`).
#' @param hfp_threshold A numeric threshold for identifying human impact areas in HFP (default: `10`).
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
make_manage_zone <- function(
    iso3,
    pus,
    mf_raster = NULL,
    agri_raster = NULL,
    built_raster = NULL,
    hfp_raster = NULL,
    hfp_threshold = 10,
    agriculture_lulc_value = 4,
    built_area_lulc_value = 7,
    filter_patch_size = TRUE,
    min_patch_size = 10,
    output_path = NULL,
    threads = TRUE) {
  # Ensure required inputs are provided
  if (is.null(mf_raster) || is.null(hfp_raster) || is.null(agri_raster) || is.null(built_raster)) {
    stop("All required input rasters (managed forests, HFP, urban areas, agriculture areas) must be provided.")
  }

  # Validate input types
  assertthat::assert_that(inherits(mf_raster, "SpatRaster"), msg = "'mf_raster' must be a SpatRaster.")
  assertthat::assert_that(inherits(agri_raster, "SpatRaster"), msg = "'agri_raster' must be a SpatRaster.")
  assertthat::assert_that(inherits(built_raster, "SpatRaster"), msg = "'built_raster' must be a SpatRaster.")
  assertthat::assert_that(inherits(hfp_raster, "SpatRaster"), msg = "'hfp_raster' must be a SpatRaster.")

  # Extracting Agricultural Areas from LULC
  cat("Extracting agricultural areas from LULC raster...\n")
  if (terra::minmax(agri_raster)[[2]] <= 1) {
    agricultural_areas <- agri_raster
  } else {
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
  } else {
    built_areas <- make_normalised_raster(
      raster_in = built_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "bilinear",
      input_raster_conditional_expression = function(x) terra::ifel(x == built_area_lulc_value, 1, 0)
    )
  }

  # Process HFP data
  hfp <- make_normalised_raster(
    raster_in = hfp_raster,
    pus = pus,
    iso3 = iso3,
    rescale = FALSE,
    method_override = "bilinear",
    conditional_expression = function(x) terra::ifel(x == hfp_threshold, 1, 0)
  )

  hfp <- terra::ifel(hfp > 0, 1, 0) %>%
    terra::mask(pus, maskvalues = 0)

  # Process MF data
  if (terra::minmax(mf_raster)[[2]] <= 1) {
    mf <- mf_raster
  } else {
    cat("requires already processed managed forest file.") # requires too many input arguments to do here. Do this more elegantly later on.
  }

  # Manage zone
  manage_zone <- terra::ifel((hfp + mf) > 0, 1, 0)
  manage_zone <- terra::ifel((manage_zone - built_areas) < 0, 0, manage_zone) %>%
    terra::mask(pus, maskvalues = 0) %>%
    terra::subst(NA, 0)

  if (filter_patch_size) {
    manage_zone <- terra::sieve(manage_zone, threshold = min_patch_size)
  }

  # Save final output
  if (!is.null(output_path)) {
    save_raster(
      manage_zone,
      glue::glue("{output_path}/manage_zone_{iso3}.tif"),
      datatype = "INT1U"
    )
  }

  return(manage_zone)
}

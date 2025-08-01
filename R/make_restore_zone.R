#' Generate a Degraded Areas Layer for Restoration Planning
#'
#' This function identifies degraded areas for potential restoration based on a combination of:
#' - **SDG productivity degradation data**
#' - **Agricultural and built-up areas** (either provided or derived from a LULC raster)
#' - **Human Industrial Footprint Index (HII)**
#' - **Forest ecosystems coverage** (optional, used to create `restore_zone_v2`)
#'
#' The function applies user-defined thresholds to generate one or two outputs:
#' - `restore_zone_v1`: Based on SDG degradation, agriculture, built-up, and HII thresholds
#' - `restore_zone_v2`: Same as `v1` but masked by forest extent (only created if forest layer provided)
#'
#' If `output_path` is provided, the intermediate layers and final output are saved as Cloud Optimized GeoTIFFs.
#'
#' @param iso3 Character. ISO3 country code (e.g., "CHL").
#' @param pus SpatRaster. Planning units raster used to define resolution and extent.
#' @param sdg_degradation_input SpatRaster. SDG degradation raster input.
#' @param agricultural_areas_input SpatRaster or NULL. Optional input raster for agricultural areas. If NULL, `lulc_raster` must be provided.
#' @param built_areas_input SpatRaster or NULL. Optional input raster for built-up areas. If NULL, `lulc_raster` must be provided.
#' @param lulc_raster SpatRaster or NULL. LULC raster used to derive agriculture/built areas if not directly provided. Assumes using the ESRI 10m LULC dataset.
#' @param hii_input SpatRaster. Human Industrial Footprint Index (HII) raster.
#' @param forest_cover_input SpatRaster or NULL. Optional forest cover raster used to create `restore_zone_v2`. If NULL, only `restore_zone_v1` is created.
#' @param sdg_threshold Numeric. Threshold for SDG degradation to classify as degraded (default: 0.1).
#' @param lulc_threshold Numeric. Threshold for agri/built classification (default: 0.1).
#' @param hii_threshold Numeric. HII threshold for defining high human pressure (default: 4).
#' @param forest_threshold Numeric. Minimum forest cover value to retain in restore zone 2 (default: 0.1).
#' @param agriculture_lulc_value Integer. LULC value representing agriculture if derived from `lulc_raster` (default: 4).
#' @param built_area_lulc_value Integer. LULC value representing built-up areas if derived from `lulc_raster` (default: 7).
#' @param filter_patch_size Logical. Whether to remove small isolated patches (default: TRUE).
#' @param min_patch_size Integer. Minimum number of connected pixels to retain (default: 10).
#' @param output_path Character or NULL. Directory to save output rasters. If NULL, outputs are returned but not saved (default: NULL).
#'
#' @return A `SpatRaster` with one or two layers:
#' - `restore_zone_v1`: Degraded areas based on SDG, LULC, and HII thresholds
#' - `restore_zone_v2`: `v1` masked by forest coverage (only if `forest_cover_input` is provided)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: With forest cover layer (creates both v1 and v2)
#' restore_zone <- make_restore_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   sdg_degradation_input = sdg_raster,
#'   agricultural_areas_input = NULL,
#'   built_areas_input = NULL,
#'   lulc_raster = lulc_input,
#'   hii_input = hii_raster,
#'   forest_cover_input = forest_raster,
#'   output_path = "outputs/"
#' )
#'
#' # Example 2: Without forest cover layer (creates only v1)
#' restore_zone <- make_restore_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   sdg_degradation_input = sdg_raster,
#'   agricultural_areas_input = ag_areas_input,
#'   built_areas_input = built_areas_input,
#'   lulc_raster = NULL,
#'   hii_input = hii_raster,
#'   forest_cover_input = NULL,
#'   output_path = "outputs/"
#' )
#'
#' # Example 3: Using pre-computed agricultural and built areas
#' restore_zone <- make_restore_zone(
#'   iso3 = "BRA",
#'   pus = planning_units,
#'   sdg_degradation_input = sdg_raster,
#'   agricultural_areas_input = ag_raster,
#'   built_areas_input = built_raster,
#'   hii_input = hii_raster,
#'   forest_cover_input = forest_raster
#' )
#' }

make_restore_zone <- function(
    iso3,
    pus,
    sdg_degradation_input = NULL,
    agricultural_areas_input = NULL,
    built_areas_input = NULL,
    lulc_raster = NULL,
    hii_input = NULL,
    forest_cover_input = NULL,
    sdg_threshold = 0.1,
    lulc_threshold = 0.1,
    hii_threshold = 4,
    forest_threshold = 0.1,
    agriculture_lulc_value = 4,
    built_area_lulc_value = 7,
    filter_patch_size = TRUE,
    min_patch_size = 10,
    output_path = NULL
) {
  # Input validation
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(inherits(sdg_degradation_input, "SpatRaster"))
  assertthat::assert_that(inherits(hii_input, "SpatRaster"))

  # Validate forest cover input if provided
  if (!is.null(forest_cover_input)) {
    assertthat::assert_that(inherits(forest_cover_input, "SpatRaster"))
  }

  # Ensure at least one valid input source for agricultural/built areas exists
  assertthat::assert_that(
    !(is.null(agricultural_areas_input) && is.null(built_areas_input) && is.null(lulc_raster)),
    msg = "Either 'agricultural_areas_input' and/or 'built_areas_input' must be provided, or 'lulc_raster' must be supplied to derive them."
  )

  # SDG degradation layer
  log_msg("Processing SDG degradation layer...")
  sdg_degraded_areas <- elsar::make_normalised_raster(
    raster_in = sdg_degradation_input,
    pus = pus,
    iso3 = iso3,
    method = "mean",
    input_raster_conditional_expression = function(x) terra::ifel(x == -1, 1, 0)
  )

  # Agricultural areas
  log_msg("Processing agricultural areas...")
  if (!is.null(agricultural_areas_input)) {
    log_msg("Using provided agricultural areas raster...")
    agricultural_areas <- agricultural_areas_input
  } else {
    assertthat::assert_that(!is.null(lulc_raster),
                            msg = "When 'agricultural_areas_input' is NULL, 'lulc_raster' must be provided.")
    log_msg("Extracting agricultural areas from LULC raster...")
    agricultural_areas <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(x == agriculture_lulc_value, 1, 0)
    )
  }

  # Built-up areas
  log_msg("Processing built-up areas...")
  if (!is.null(built_areas_input)) {
    log_msg("Using provided built-up areas raster...")
    built_areas <- built_areas_input
  } else {
    assertthat::assert_that(!is.null(lulc_raster),
                            msg = "When 'built_areas_input' is NULL, 'lulc_raster' must be provided.")
    log_msg("Extracting built areas from LULC raster...")
    built_areas <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(x == built_area_lulc_value, 1, 0)
    )
  }

  # Human Industrial Footprint Index (HII) layer
  log_msg("Processing HII layer...")
  hii_resampled <- elsar::make_normalised_raster(
    raster_in = hii_input,
    pus = pus,
    iso3 = iso3,
    rescale = FALSE,
    method_override = "mean"
  )

  # Optional output of intermediate layers
  if (!is.null(output_path)) {
    log_msg("Saving intermediate layers...")
    if (!is.null(agricultural_areas_input)) {
      elsar::save_raster(agricultural_areas, glue::glue("{output_path}/agriculture_areas_{iso3}.tif"))
    }
    if (!is.null(built_areas_input)) {
      elsar::save_raster(built_areas, glue::glue("{output_path}/built_areas_{iso3}.tif"))
    }
    elsar::save_raster(hii_resampled, glue::glue("{output_path}/hii_{iso3}.tif"))
  }

  # Create restore zone v1 (Default Restore Zone)
  log_msg("Creating restore zone v1...")
  restore_zone_v1 <- terra::ifel(
    agricultural_areas > lulc_threshold |
      built_areas > lulc_threshold |
      sdg_degraded_areas > sdg_threshold |
      hii_resampled >= hii_threshold,
    1, 0
  ) %>%
    elsar::make_normalised_raster(
      pus = pus,
      iso3 = iso3
    )

  # Initialize output list
  restore_zones <- list(restore_zone_v1 = restore_zone_v1)

  # Create restore zone v2 only if forest cover input is provided
  if (!is.null(forest_cover_input)) {
    log_msg("Creating restore zone v2 (forest-masked)...")

    # Process forest cover layer
    forest_cover_resampled <- elsar::make_normalised_raster(
      raster_in = forest_cover_input,
      pus = pus,
      iso3 = iso3,
      method_override = "mean"
    )

    # Create forest-masked restore zone
    restore_zone_v2 <- terra::ifel(
      restore_zone_v1 == 1 & forest_cover_resampled > forest_threshold,
      1, 0
    ) %>%
      elsar::make_normalised_raster(
        pus = pus,
        iso3 = iso3
      )

    restore_zones$restore_zone_v2 <- restore_zone_v2

    # Save forest layer if output path provided
    if (!is.null(output_path)) {
      elsar::save_raster(forest_cover_resampled, glue::glue("{output_path}/forest_cover_{iso3}.tif"))
    }
  } else {
    log_msg("Forest cover input not provided - creating only restore zone v1")
  }

  # Convert list to SpatRaster
  restore_zones <- terra::rast(restore_zones)

  # Optionally remove small patches (default behaviour)
  if (filter_patch_size) {
    log_msg(glue::glue("Filtering patches smaller than {min_patch_size} pixels..."))
    restore_zones[[1]] <- terra::sieve(restore_zones[[1]], threshold = min_patch_size)

    if (terra::nlyr(restore_zones) > 1) {
      restore_zones[[2]] <- terra::sieve(restore_zones[[2]], threshold = min_patch_size)
    }
  }

  # Save final output
  if (!is.null(output_path)) {
    log_msg("Saving final restore zones...")
    elsar::save_raster(
      raster = restore_zones,
      filename = glue::glue("{output_path}/restore_zones_{iso3}.tif"),
      datatype = "INT1U"
    )
  }

  log_msg(glue::glue("Successfully created {terra::nlyr(restore_zones)} restore zone layer(s)"))
  return(restore_zones)
}

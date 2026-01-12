#' Generate Restore Zones for Spatial Planning
#'
#' Identifies potential restoration areas by combining degradation indicators
#' while excluding areas unsuitable for restoration (e.g., active farmland, urban areas).
#'
#' The function generates two restore zone outputs:
#' - `restore_zone_v1`: All degraded areas excluding agriculture and built-up land
#' - `restore_zone_v2`: Same as v1, but further masked to forest ecosystems only
#'
#' Areas are classified as degraded based on either:
#' - Land degradation indicators (e.g., productivity decline)
#' - Human pressure exceeding a threshold
#'
#' Agricultural and built-up areas can be provided directly as rasters, or derived
#' from a land use/land cover (LULC) raster by specifying class values.
#'
#' @param iso3 Character. ISO3 country code (e.g., "CHL").
#' @param pus SpatRaster. Planning units raster defining the output resolution and extent.
#' @param degradation SpatRaster. Raster indicating land degradation. Values of -1 are
#'   treated as degraded (e.g., SDG 15.3.1 productivity degradation layer).
#' @param human_pressure SpatRaster. Raster of human pressure/impact values
#'   (e.g., Human Footprint Index). Higher values indicate greater pressure.
#' @param forest_mask SpatRaster. Raster indicating forest ecosystem extent, used
#'   to create the forest-only restore zone (v2). Values represent forest cover proportion.
#' @param lulc_proportions A multi-band `SpatRaster` from [download_lulc_proportions()] containing
#'   pre-computed class proportions. If provided, bands named "agriculture" and "built_area" will
#'   be used automatically, overriding `agricultural_areas` and `built_areas`.
#' @param agricultural_areas SpatRaster or NULL. Pre-computed raster of agricultural
#'   area proportions (0-1). If NULL, derived from `lulc`.
#' @param built_areas SpatRaster or NULL. Pre-computed raster of built-up/urban area
#'   proportions (0-1). If NULL, derived from `lulc`.
#' @param lulc SpatRaster or NULL. Land use/land cover raster used to derive
#'   agriculture and built-up areas when not provided directly.
#' @param degradation_threshold Numeric. Proportion threshold above which an area
#'   is considered degraded (default: 0.1).
#' @param human_pressure_threshold Numeric. Human pressure value at or above which
#'   an area is considered degraded (default: 4).
#' @param agriculture_threshold Numeric. Proportion threshold above which an area
#'   is excluded as agricultural land (default: 0.1).
#' @param built_area_threshold Numeric. Proportion threshold above which an area
#'   is excluded as built-up land (default: 0.1).
#' @param forest_threshold Numeric. Minimum forest cover proportion to include
#'   in restore_zone_v2 (default: 0.1).
#' @param lulc_product Character. LULC product used for class value lookups: "esri_10m" (default),
#'   "dynamic_world", "esa_worldcover", or "local". When "local", explicit class values must be provided.
#' @param agriculture_lulc_value Integer. Class value in `lulc` representing
#'   agricultural land (default: 5, for ESRI 10m LULC).
#' @param built_area_lulc_value Integer. Class value in `lulc` representing built-up
#'   areas (default: 7, for ESRI 10m LULC).
#' @param filter_small_patches Logical. Whether to remove small isolated patches
#'   from the output (default: TRUE).
#' @param min_patch_size Integer. Minimum number of connected pixels to retain
#'   when filtering patches (default: 10).
#' @param output_path Character or NULL. Directory to save output rasters as
#'   Cloud Optimized GeoTIFFs. If NULL, outputs are returned but not saved.
#'
#' @return A SpatRaster with two layers:
#' \describe{
#'   \item{restore_zone_v1}{Degraded areas excluding agriculture and built-up land}
#'   \item{restore_zone_v2}{restore_zone_v1 masked to forest ecosystems only}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using LULC raster to derive agriculture/built areas
#' restore_zones <- make_restore_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   degradation = degradation_raster,
#'   human_pressure = hfi_raster,
#'   forest_mask = forest_raster,
#'   lulc = lulc_raster,
#'   output_path = "outputs/"
#' )
#'
#' # Using pre-computed agriculture/built area rasters
#' restore_zones <- make_restore_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   degradation = degradation_raster,
#'   human_pressure = hfi_raster,
#'   forest_mask = forest_raster,
#'   agricultural_areas = ag_raster,
#'   built_areas = urban_raster,
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
    degradation,
    human_pressure,
    forest_mask,
    lulc_proportions = NULL,
    agricultural_areas = NULL,
    built_areas = NULL,
    lulc = NULL,
    degradation_threshold = 0.1,
    human_pressure_threshold = 4,
    agriculture_threshold = 0.1,
    built_area_threshold = 0.1,
    forest_threshold = 0.1,
    lulc_product = c("esri_10m", "dynamic_world", "esa_worldcover", "local"),
    agriculture_lulc_value = NULL,
    built_area_lulc_value = NULL,
    filter_small_patches = TRUE,
    min_patch_size = 10,
    output_path = NULL
) {
  lulc_product <- match.arg(lulc_product)

  # If lulc_proportions provided, extract agriculture and built_area
  # Supports both list format (new) and SpatRaster format (legacy)
  if (!is.null(lulc_proportions)) {
    if (inherits(lulc_proportions, "list")) {
      log_message("Using LULC proportions (list format): {paste(names(lulc_proportions), collapse=', ')}")
      if ("agriculture" %in% names(lulc_proportions) && is.null(agricultural_areas)) {
        agricultural_areas <- lulc_proportions[["agriculture"]]
        log_message("Using 'agriculture' proportion raster")
      }
      if ("built_area" %in% names(lulc_proportions) && is.null(built_areas)) {
        built_areas <- lulc_proportions[["built_area"]]
        log_message("Using 'built_area' proportion raster")
      }
    } else if (inherits(lulc_proportions, "SpatRaster")) {
      band_names <- names(lulc_proportions)
      log_message("Using LULC proportions (SpatRaster): {paste(band_names, collapse=', ')}")
      if ("agriculture" %in% band_names && is.null(agricultural_areas)) {
        agricultural_areas <- lulc_proportions[["agriculture"]]
        log_message("Extracted 'agriculture' band")
      }
      if ("built_area" %in% band_names && is.null(built_areas)) {
        built_areas <- lulc_proportions[["built_area"]]
        log_message("Extracted 'built_area' band")
      }
    } else {
      stop("'lulc_proportions' must be a list or SpatRaster object.", call. = FALSE)
    }
  }

  # Resolve LULC class values from product if not explicitly provided
  # (only needed if using lulc fallback)
  if (is.null(agricultural_areas) || is.null(built_areas)) {
    if (is.null(agriculture_lulc_value)) {
      if (lulc_product == "local") {
        stop("When lulc_product = 'local', agriculture_lulc_value must be explicitly provided.", call. = FALSE)
      }
      agriculture_lulc_value <- get_lulc_class_value(lulc_product, "agriculture")
    }
    if (is.null(built_area_lulc_value)) {
      if (lulc_product == "local") {
        stop("When lulc_product = 'local', built_area_lulc_value must be explicitly provided.", call. = FALSE)
      }
      built_area_lulc_value <- get_lulc_class_value(lulc_product, "built_area")
    }

    log_message("Using LULC product: {lulc_product}")
    log_message("Agriculture class value(s): {paste(agriculture_lulc_value, collapse=', ')}")
    log_message("Built area class value(s): {paste(built_area_lulc_value, collapse=', ')}")
  }

  # Input validation
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(inherits(degradation, "SpatRaster"))
  assertthat::assert_that(inherits(human_pressure, "SpatRaster"))
  assertthat::assert_that(inherits(forest_mask, "SpatRaster"))

  # Ensure at least one valid input source for agricultural/built areas exists
  assertthat::assert_that(
    !(is.null(agricultural_areas) && is.null(built_areas) && is.null(lulc)),
    msg = "Either 'agricultural_areas' and/or 'built_areas' must be provided, or 'lulc' must be supplied to derive them."
  )
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Land degradation layer
  log_message("Processing degradation layer...")
  degradation_processed <- elsar::make_normalised_raster(
    raster_in = degradation,
    pus = pus,
    iso3 = iso3,
    method = "mean",
    input_raster_conditional_expression = function(x) terra::ifel(x == -1, 1, 0)
  )

  # Agricultural areas
  log_message("Processing agricultural areas...")
  if (!is.null(agricultural_areas)) {
    log_message("Aligning provided agricultural areas raster to planning units...")
    agricultural_areas_processed <- elsar::make_normalised_raster(
      raster_in = agricultural_areas,
      pus = pus,
      iso3 = iso3,
      rescaled = FALSE,
      method_override = "mean"
    )
  } else {
    assertthat::assert_that(!is.null(lulc), msg = "When 'agricultural_areas' is NULL, 'lulc' must be provided.")
    log_message("Extracting agricultural areas from LULC raster...")
    agricultural_areas_processed <- elsar::make_normalised_raster(
      raster_in = lulc,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(x == agriculture_lulc_value, 1, 0)
    )
  }

  # Built-up areas
  log_message("Processing built-up areas...")
  if (!is.null(built_areas)) {
    log_message("Aligning provided built areas raster to planning units...")
    built_areas_processed <- elsar::make_normalised_raster(
      raster_in = built_areas,
      pus = pus,
      iso3 = iso3,
      rescaled = FALSE,
      method_override = "mean"
    )
  } else {
    assertthat::assert_that(!is.null(lulc), msg = "When 'built_areas' is NULL, 'lulc' must be provided.")
    log_message("Extracting built areas from LULC raster...")
    built_areas_processed <- elsar::make_normalised_raster(
      raster_in = lulc,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(x == built_area_lulc_value, 1, 0)
    )
  }

  # Human pressure layer
  log_message("Processing human pressure layer...")
  human_pressure_processed <- elsar::make_normalised_raster(
    raster_in = human_pressure,
    pus = pus,
    iso3 = iso3,
    rescaled = FALSE,
    method_override = "mean"
  )

  # Forest mask - align to planning units
  log_message("Aligning forest mask to planning units...")
  forest_mask_processed <- elsar::make_normalised_raster(
    raster_in = forest_mask,
    pus = pus,
    iso3 = iso3,
    rescaled = FALSE,
    method_override = "mean"
  )

  # Optional output of intermediate layers
  if (!is.null(output_path)) {
    if (!is.null(agricultural_areas)) {
      save_raster(agricultural_areas_processed, glue::glue("{output_path}/agriculture_areas_{iso3}.tif"))
    }
    if (!is.null(built_areas)) {
      save_raster(built_areas_processed, glue::glue("{output_path}/built_areas_{iso3}.tif"))
    }
    save_raster(human_pressure_processed, glue::glue("{output_path}/human_pressure_{iso3}.tif"))
  }

  # Combine degradation indicators into restore zone v1 (the Default Restore Zone)
  # Include areas that are degraded but exclude farms and urban areas
  restore_zone <- terra::ifel(
    (degradation_processed > degradation_threshold | human_pressure_processed >= human_pressure_threshold) &
      agricultural_areas_processed <= agriculture_threshold &
      built_areas_processed <= built_area_threshold,
    1, 0
  ) %>%
    elsar::make_normalised_raster(
      pus = pus,
      iso3 = iso3
    )

  # Create Restore Zone v2 (the alternative Restore Zone) as only degraded forest areas
  restore_zone_v2 <- terra::ifel(
    restore_zone == 1 & forest_mask_processed > forest_threshold,
    1, 0
  ) %>%
    make_normalised_raster(
      pus = pus,
      iso3 = iso3,
      method_override = "mean"
    )

  # Combine into multi-layer raster
  restore_zones <- c(restore_zone, restore_zone_v2)
  names(restore_zones) <- c("restore_zone_v1", "restore_zone_v2")

  # Optionally remove small patches (default behaviour)
  if (filter_small_patches) {
    restore_zones[[1]] <- terra::sieve(restore_zones[[1]], threshold = min_patch_size)

    if (terra::nlyr(restore_zones) > 1) {
      restore_zones[[2]] <- terra::sieve(restore_zones[[2]], threshold = min_patch_size)
    }
  }

  # Save final output
  if (!is.null(output_path)) {
    log_message("Saving final restore zones...")
    elsar::save_raster(
      raster = restore_zones,
      filename = glue::glue("{output_path}/restore_zones_{iso3}.tif"),
      datatype = "INT1U"
    )
  }

  log_message("Successfully created {terra::nlyr(restore_zones)} restore zone layer(s)")
  return(restore_zones)
}

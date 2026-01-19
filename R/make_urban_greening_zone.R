#' Generate Urban Greening Zone Raster for ELSA Analysis
#'
#' This function creates a `SpatRaster` layer representing planning units where urban
#' greening interventions are applicable in the ELSA framework. The Urban Greening Zone
#' identifies built/urban areas where greening actions (e.g., tree planting, green
#' infrastructure) can be implemented.
#'
#' This zone is simpler than Protect, Restore, or Manage zones as it has a single
#' primary input (built/urban areas). It essentially transforms the built area raster
#' into a formal zone layer for consistency with the ELSA framework.
#'
#' @param iso3 ISO3 country code (e.g., "NPL").
#' @param pus A `SpatRaster` defining the planning units.
#' @param built_areas_input A `SpatRaster` representing binary or probabilistic built-up areas (optional).
#'   If provided, this takes priority over extraction from other inputs.
#' @param lulc_proportions A multi-band `SpatRaster` or list from [download_lulc_proportions()] containing
#'   pre-computed class proportions. If provided and contains "built_area", will be used
#'   if `built_areas_input` is NULL.
#' @param lulc_raster A `SpatRaster` LULC map used to extract built-up areas if not already provided (default: NULL).
#' @param lulc_product Character. LULC product used for class value lookups: "esri_10m" (default),
#'   "dynamic_world", "esa_worldcover", or "local". When "local", `built_area_lulc_value` must be provided.
#' @param built_area_lulc_value Integer or NULL. LULC value representing built-up areas. If NULL,
#'   automatically determined from `lulc_product`. Default is NULL.
#' @param built_area_threshold Minimum fraction for built-up area to be included (default: 0).
#'   A threshold of 0 includes all urban areas, which is recommended since urban areas
#'   are often small and should be preserved.
#' @param filter_patch_size Logical. Whether to remove small isolated patches (default: FALSE).
#'   Defaults to FALSE because urban areas are often small and should be preserved.
#' @param min_patch_size Integer. Minimum patch size to retain (in raster cells; default: 10).
#'   Only used if `filter_patch_size = TRUE`.
#' @param output_path Optional directory to write the result as a COG.
#'
#' @return A `SpatRaster` with values 1 (urban greening zone) and 0 (not urban).
#' @export
#'
#' @examples
#' \dontrun{
#' urban_greening_zone <- make_urban_greening_zone(
#'   pus = planning_units,
#'   iso3 = "NPL",
#'   built_areas_input = built_raster,
#'   output_path = "outputs/"
#' )
#' }

make_urban_greening_zone <- function(
    iso3,
    pus,
    built_areas_input = NULL,
    lulc_proportions = NULL,
    lulc_raster = NULL,
    lulc_product = c("esri_10m", "dynamic_world", "esa_worldcover", "local"),
    built_area_lulc_value = NULL,
    built_area_threshold = 0,
    filter_patch_size = FALSE,
    min_patch_size = 10,
    output_path = NULL
) {
  lulc_product <- match.arg(lulc_product)

  # If lulc_proportions provided, extract built_area
  # Supports both list format (new) and SpatRaster format (legacy)
  if (!is.null(lulc_proportions) && is.null(built_areas_input)) {
    if (inherits(lulc_proportions, "list")) {
      # New format: named list of SpatRasters
      log_message("Using LULC proportions (list format): {paste(names(lulc_proportions), collapse=', ')}")
      if ("built_area" %in% names(lulc_proportions)) {
        built_areas_input <- lulc_proportions[["built_area"]]
        log_message("Using 'built_area' proportion raster")
      }
    } else if (inherits(lulc_proportions, "SpatRaster")) {
      # Legacy format: multi-band SpatRaster
      band_names <- names(lulc_proportions)
      log_message("Using LULC proportions (SpatRaster): {paste(band_names, collapse=', ')}")
      if ("built_area" %in% band_names) {
        built_areas_input <- lulc_proportions[["built_area"]]
        log_message("Extracted 'built_area' band")
      }
    } else {
      stop("'lulc_proportions' must be a list or SpatRaster object.", call. = FALSE)
    }
  }

  # Resolve LULC class value from product if not explicitly provided
  # (only needed if using lulc_raster fallback)
  if (is.null(built_areas_input) && is.null(built_area_lulc_value)) {
    if (lulc_product == "local") {
      stop("When lulc_product = 'local', built_area_lulc_value must be explicitly provided.", call. = FALSE)
    }
    built_area_lulc_value <- get_lulc_class_value(lulc_product, "built_area")
    log_message("Using LULC product: {lulc_product}")
    log_message("Built area class value(s): {paste(built_area_lulc_value, collapse=', ')}")
  }

  # Validate output path if provided
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Process built-up areas
  log_message("Processing built-up areas for Urban Greening Zone...")
  if (!is.null(built_areas_input)) {
    log_message("Aligning provided built areas raster to planning units...")
    built_areas <- elsar::make_normalised_raster(
      raster_in = built_areas_input,
      pus = pus,
      iso3 = iso3,
      rescaled = FALSE,
      method_override = "mean"
    )
  } else {
    assertthat::assert_that(!is.null(lulc_raster),
      msg = "One of 'built_areas_input', 'lulc_proportions' (with built_area), or 'lulc_raster' must be provided.")
    log_message("Extracting built areas from LULC raster...")
    built_areas <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == built_area_lulc_value, 1, 0)
    )
  }

  # Apply threshold to create binary zone
  log_message("Applying built area threshold: {built_area_threshold}")
  urban_greening_zone <- terra::ifel(built_areas > built_area_threshold, 1, 0)

  # Optionally filter out small patches
  if (filter_patch_size) {
    log_message("Sieving out patch sizes smaller than {min_patch_size} planning units...")
    urban_greening_zone <- terra::sieve(urban_greening_zone, threshold = min_patch_size)
  }

  names(urban_greening_zone) <- "urban_greening_zone"

  # Save to disk if needed
  if (!is.null(output_path)) {
    filename <- glue::glue("{output_path}/urban_greening_zone_{iso3}.tif")

    elsar::save_raster(
      urban_greening_zone,
      filename = filename,
      datatype = "INT1U"
    )
    log_message("Saved Urban Greening Zone to: {filename}")
  }

  return(urban_greening_zone)
}

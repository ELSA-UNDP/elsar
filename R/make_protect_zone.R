#' Generate Protect Zone Raster for ELSA Analysis
#'
#' This function creates a `SpatRaster` layer representing planning units eligible for protect
#' actions in the ELSA framework. Areas with high human footprint, agriculture, or built-up land cover
#' are excluded. Optionally, the result can be inverted to serve as a locked-out constraint
#' for spatial prioritization tools such as `prioritizr`.
#'
#' The Human Footprint Index (HII) threshold can be defined either as a fixed value (`hii_threshold`)
#' or estimated from the upper 95% quantile (`hii_quantile`) of values within protected areas.
#'
#' @param iso3 ISO3 country code (e.g., "NPL").
#' @param pus A `SpatRaster` defining the planning units.
#' @param current_protected_areas a `sf` or `SpatVector` object of current protected areas.
#'        If NULL, `elsar::make_protected_areas()` will be used.
#' @param agricultural_areas_input A `SpatRaster` representing binary or probabilistic agricultural areas (optional).
#' @param built_areas_input A `SpatRaster` representing binary or probabilistic built-up areas (optional).
#' @param lulc_raster A `SpatRaster` LULC map used to extract agriculture/built-up areas if not already provided (default: NULL).
#' @param hii_input A `SpatRaster` of the Human Footprint Index.
#' @param hii_threshold A fixed numeric HII threshold. If NULL, `hii_quantile` is used to estimate it.
#' @param hii_quantile A quantile threshold (e.g., 0.95) used to calculate the HII threshold within protected areas if `hii_threshold` is NULL.
#' @param agriculture_lulc_value LULC value representing agriculture (default: 4).
#' @param built_area_lulc_value LULC value representing built-up areas (default: 7).
#' @param agriculture_threshold Minimum fraction for agriculture to exclude a cell (default: 0.1).
#' @param built_areas_threshold Minimum fraction for built-up area to exclude a cell (default: 0.1).
#' @param filter_patch_size Logical. Whether to remove small isolated patches (default: TRUE).
#' @param min_patch_size Integer. Minimum patch size to retain (in raster cells; default: 20).
#' @param make_locked_out Logical. If TRUE, invert the raster to produce a locked-out constraint (default: FALSE).
#' @param output_path Optional directory to write the result as a COG.
#'
#' @return A `SpatRaster` with values 1 (eligible) and 0 (excluded), or the inverse if `make_locked_out = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' protect_zone <- make_protect_zone(
#'   pus = planning_units,
#'   iso3 = "NPL",
#'   hii_input = hii_raster,
#'   agricultural_areas_input = crop_raster,
#'   built_areas_input = built_raster,
#'   lulc_raster = NULL,
#'   hii_quantile = 0.95,
#'   output_path = "outputs/"
#' )
#' }

make_protect_zone <- function(
    iso3,
    pus,
    current_protected_areas,
    agricultural_areas_input = NULL,
    built_areas_input = NULL,
    lulc_raster = NULL,
    hii_input,
    hii_threshold = NULL,
    hii_quantile = 0.95,
    agriculture_lulc_value = 5,
    built_area_lulc_value = 7,
    agriculture_threshold = 0.1,
    built_areas_threshold = 0.1,
    filter_patch_size = TRUE,
    min_patch_size = 20,
    make_locked_out = FALSE,
    output_path = NULL
) {
  # Validate protected areas input or load default
  assertthat::assert_that(
      inherits(current_protected_areas, "sf") || inherits(current_protected_areas, "SpatVector"),
      msg = "'current_protected_areas' must be an 'sf' or 'SpatVector' object.")

  if (nrow(current_protected_areas) > 1){
    current_protected_areas <- current_protected_areas %>%
      dplyr::summarise() %>%
      sf::st_make_valid()
    }

  # Process agriculture
  log_msg("Processing agricultural areas...")
  if (!is.null(agricultural_areas_input)) {
    agricultural_areas <- agricultural_areas_input
  } else {
    assertthat::assert_that(!is.null(lulc_raster), msg = "If 'agricultural_areas_input' is NULL, 'lulc_raster' must be provided.")
    log_msg("Extracting agricultural areas from LULC raster...")
    agricultural_areas <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == agriculture_lulc_value, 1, 0)
      )
    }

  # Process built-up areas
  log_msg("Processing built-up areas...")
  if (!is.null(built_areas_input)) {
    built_areas <- built_areas_input
  } else {
    assertthat::assert_that(!is.null(lulc_raster), msg = "If 'built_areas_input' is NULL, 'lulc_raster' must be provided.")
    log_msg("Extracting built areas from LULC raster...")
    built_areas <-  elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == built_area_lulc_value, 1, 0)
      )
    }

  # Normalize HII
  log_msg("Processing HII raster...")
  hii_resampled <- elsar::make_normalised_raster(
    raster_in = hii_input,
    pus = pus,
    iso3 = iso3,
    rescale = FALSE,
    method_override = "mean"
    )

  # Determine threshold from fixed value or quantile
  if (!is.null(hii_threshold)) {
    breaks <- hii_threshold
    log_msg(glue::glue("Using fixed HII threshold: {breaks}"))
  } else {
    breaks <- exactextractr::exact_extract(
      x = hii_resampled,
      y = current_protected_areas,
      fun = "quantile",
      quantiles = hii_quantile
    )
    log_msg(glue::glue("HII threshold calculated from quantile {hii_quantile}: {breaks}"))
  }

  # Base zone: where HII is low
  log_msg("Creating initial protect zone based on HII values...")
  protect_zone <- terra::ifel(hii_resampled < breaks, 1, 0)

  # Exclude agriculture and built-up areas
  log_msg("Removing built and agricultural areas...")
  if (!is.null(built_areas) && is.null(agricultural_areas)) {
    protect_zone <- terra::ifel(built_areas > built_areas_threshold, 0, protect_zone)
  } else if (is.null(built_areas) && !is.null(agricultural_areas)) {
    protect_zone <- terra::ifel(agricultural_areas > agriculture_threshold, 0, protect_zone)
  } else if (!is.null(built_areas) && !is.null(agricultural_areas)) {
    protect_zone <- terra::ifel(built_areas > built_areas_threshold | agricultural_areas > agriculture_threshold, 0, protect_zone)
  } else {
    log_msg("No building or agricultural area found - Protection zone based on HII only.")
  }

  # Optionally filter out small patches
  if (filter_patch_size) {
    log_msg(glue::glue("Sieving out patch sizes smaller than {min_patch_size} planning units..."))
    protect_zone <- terra::sieve(protect_zone, threshold = min_patch_size)
  }

  # Invert if locked-out zone required
  if (make_locked_out) {
    protect_zone <- 1 - protect_zone
  }

  names(protect_zone) <- "protect_zone"

  # Save to disk if needed
  if (!is.null(output_path)) {
    filename <- glue::glue("{output_path}/protect_zone_{iso3}.tif")

    elsar::save_raster(
      protect_zone,
      filename = filename,
      datatype = "INT1U"
    )
  }

  return(protect_zone)
}

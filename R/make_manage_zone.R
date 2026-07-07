#' Generate Sustainable Management Zones for ELSA Analysis
#'
#' This function identifies planning units eligible for the "Manage" zone under the ELSA framework.
#' The zone (default) includes areas with a moderate level of human influence (HII between
#' 20th and 80th percentiles), all managed forest types, and agricultural and pastureland areas, but
#' explicitly excludes built-up areas. An alternative zone includes is limited to all
#' agricultural areas and pasturelands.
#'
#' Two layers are returned:
#' - `manage_zone_v1`: Based on HII quantile range, agriculture, and managed forests (excluding built-up areas)
#' - `manage_zone_v2`: Simplified zone based on agriculture only
#'
#' If `output_path` is provided, both layers are saved as a (multiband) Cloud Optimized GeoTIFF (COG).
#'
#' @param iso3 Character. ISO3 country code (e.g., "CHL").
#' @param pus SpatRaster. Planning units raster to which all inputs are aligned.
#' @param managed_forests SpatRaster. A preprocssed raster of managed forest extent.
#' @param raster_mf SpatRaster A raster of forest classes to identify managed forests.
#' @param lulc_proportions A multi-band `SpatRaster` from [download_lulc_proportions()] containing
#'   pre-computed class proportions. If provided, bands named "agriculture" and "built_area" will
#'   be used automatically, overriding `agricultural_areas` and `built_areas`.
#' @param agricultural_areas SpatRaster. A binary or categorical raster representing agricultural areas.
#' @param pasturelands SpatRaster. A binary or categorical raster representing (actively managed/improved)pastures.
#' @param built_areas SpatRaster. A binary or categorical raster representing built-up/urban areas.
#' @param lulc_raster SpatRaster or NULL. Optional raw LULC input used to extract agriculture and built-up layers if those inputs are categorical.
#' @param human_pressure SpatRaster. Human Footprint Index raster.
#' @param pasturelands_threshold Numeric. Probability threshold above which cells are considered pasturelands (default = 0.35).
#' @param lulc_product Character. LULC product used for class value lookups: "esri_10m" (default),
#'   "dynamic_world", "esa_worldcover", or "local". When "local", explicit class values must be provided.
#' @param agriculture_lulc_value Integer or NULL. LULC value for agriculture. If NULL, automatically
#'   determined from `lulc_product`. Default is NULL.
#' @param built_area_lulc_value Integer or NULL. LULC value for built-up areas. If NULL, automatically
#'   determined from `lulc_product`. Default is NULL.
#' @param forest_classes Integer vector or NULL. LULC values representing managed forests. If NULL,
#'   automatically determined from `lulc_product`. Default is NULL.
#' @param forest_class_threshold Numeric. Minimum proportion of managed forest in a cell to include (default = 0.1).
#' @param agriculture_threshold Numeric. Minimum proportion of agriculture in a cell to include (default = 0.1).
#' @param built_areas_threshold Numeric. Threshold above which cells are considered built-up and excluded (default = 0.1).
#' @param filter_patch_size Logical. Whether to remove small patches (default = TRUE).
#' @param min_patch_size Integer. Minimum size of patches to retain (in cells; default = 10).
#' @param output_path Character or NULL. When provided, the final zone is saved
#'   here as a COG (`manage_zone_{iso3}.tif`), together with the region-aligned
#'   intermediate inputs used to build it - `agriculture_areas_`, `pasturelands_`,
#'   `built_areas_`, `hii_mid60pct_` and `managed_forests_` `{iso3}.tif` - each
#'   masked to the planning units (NoData outside the study area). Default NULL.
#'
#' @return A SpatRaster with two layers: `manage_zone_v1` and `manage_zone_v2`.
#'
#' @seealso [get_lulc_classes()], [get_lulc_class_value()], [download_lulc_data()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' manage_zone <- make_manage_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   managed_forests = forest_raster,
#'   agricultural_areas = agri_raster,
#'   pasturelands = pastures_raster,
#'   built_areas = built_raster,
#'   lulc_raster = landcover_raster,
#'   human_pressure = hfp_raster,
#'   output_path = "outputs/"
#' )
#' }

make_manage_zone <- function(
    iso3,
    pus,
    managed_forests,
    raster_mf = NULL,
    lulc_proportions = NULL,
    agricultural_areas = NULL,
    pasturelands = NULL,
    built_areas = NULL,
    lulc_raster = NULL,
    human_pressure = NULL,
    pasturelands_threshold = 0.35,
    lulc_product = c("esri_10m", "dynamic_world", "esa_worldcover", "local"),
    agriculture_lulc_value = NULL,
    built_area_lulc_value = NULL,
    forest_classes = NULL,
    forest_class_threshold = 0.1,
    agriculture_threshold = 0.1,
    built_areas_threshold = 0.1,
    filter_patch_size = TRUE,
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
  # (only needed if using lulc_raster fallback)
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

  if (is.null(forest_classes)) {
    if (lulc_product == "local") {
      stop("When lulc_product = 'local', forest_classes must be explicitly provided.", call. = FALSE)
    }
    forest_classes <- get_lulc_class_value(lulc_product, "forest_managed")
  }
  log_message("Forest class value(s): {paste(forest_classes, collapse=', ')}")

  # Input validation
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a character string.")
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")

  # Ensure agricultural/built data is supplied or can be derived
  assertthat::assert_that(
    !(is.null(agricultural_areas) && is.null(built_areas) && is.null(lulc_raster)),
    msg = "Either 'agricultural_areas' and/or 'built_areas' must be provided, or 'lulc_raster' must be supplied to derive them."
  )

  # Check required raster inputs
  assertthat::assert_that(inherits(human_pressure, "SpatRaster"), msg = "'human_pressure' must be a SpatRaster.")
  assertthat::assert_that(inherits(managed_forests, "SpatRaster"), msg = "'managed_forests' must be a SpatRaster.")

  # If provided, validate optional rasters
  if (!is.null(agricultural_areas)) {
    assertthat::assert_that(inherits(agricultural_areas, "SpatRaster"), msg = "'agricultural_areas' must be a SpatRaster.")
  }
  if (!is.null(pasturelands)) {
    assertthat::assert_that(inherits(pasturelands, "SpatRaster"), msg = "'pasturelands' must be a SpatRaster.")
  }
  if (!is.null(built_areas)) {
    assertthat::assert_that(inherits(built_areas, "SpatRaster"), msg = "'built_areas' must be a SpatRaster.")
  }
  if (!is.null(lulc_raster)) {
    assertthat::assert_that(inherits(lulc_raster, "SpatRaster"), msg = "'lulc_raster' must be a SpatRaster.")
  }
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Process agricultural areas
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
    agricultural_areas_processed <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == agriculture_lulc_value, 1, 0)
      )
  }

  # Process pasturelands
  log_message("Processing pasturelands...")
  pasturelands_processed <- elsar::make_normalised_raster(
    raster_in = pasturelands,
    pus = pus,
    iso3 = iso3,
    method_override = "mean"
    )

  # Process built-up areas
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
    built_areas_processed <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == built_area_lulc_value, 1, 0)
      )
    }

  # Normalize HFP and extract middle 60%
  log_message("Processing HII layer and extracting middle 60% quantile range...")
  hii_resampled <- elsar::make_normalised_raster(
    raster_in = human_pressure,
    pus = pus,
    iso3 = iso3,
    rescaled = FALSE,
    method_override = "mean"
    )

  breaks <- terra::global(hii_resampled, fun = quantile, probs = c(0.2, 0.8), na.rm = TRUE)
  hii_middle_60_pct <- terra::ifel(hii_resampled >= breaks[,1] & hii_resampled <= breaks[,2], 1, 0)

  log_message("The middle 60% threshold of HII is is between values of {breaks[1]} to {breaks[2]}.")

  # Process managed forests
  log_message("Processing managed forests...")
  if (!is.null(managed_forests)) {
    log_message("Aligning provided managed forests raster to planning units...")
    managed_forests_processed <- elsar::make_normalised_raster(
      raster_in = managed_forests,
      pus = pus,
      iso3 = iso3,
      rescaled = FALSE,
      method_override = "mean"
    )
  } else {
  # Normalise and reclass
    managed_forests_processed <- elsar::make_managed_forests(
      raster_in = raster_mf,
      pus = pus,
      iso3 = iso3,
      make_productive = FALSE)

  managed_forests_processed <- managed_forests_processed[[1]]
  }

  # Main management zone: moderate HFP, OR managed forests, OR ag areas — minus built-up
  log_message("Creating the default manage zone using middle 60% of HII value, managed forests, agricultural areas, and pasturelands...")
  manage_zone <- terra::ifel(
    hii_middle_60_pct == 1 |
      managed_forests_processed > forest_class_threshold |
      agricultural_areas_processed > agriculture_threshold |
      pasturelands_processed > pasturelands_threshold,
    1, 0
  ) %>% make_normalised_raster(
    pus = pus,
    iso3 = iso3)

  # Exclude built-up areas
  log_message("Excluding built areas from the manage zone...")
  manage_zone <- terra::ifel(built_areas_processed > built_areas_threshold, 0, manage_zone) %>%
    make_normalised_raster(pus = pus, iso3 = iso3)

  # Secondary zone (agriculture and pastureland only)
  log_message("Creating the alternative manage zone using agricultural areas and pasturelands only...")
  manage_zone_alt <- terra::ifel(
    agricultural_areas_processed > agriculture_threshold |
      pasturelands_processed > pasturelands_threshold,
    1, 0
    ) %>%
    make_normalised_raster(pus = pus, iso3 = iso3)

  # Combine into multi-layer SpatRaster
  manage_zones <- c(manage_zone, manage_zone_alt)
  names(manage_zones) <- c("manage_zone_v1", "manage_zone_v2")

  # Filter out small patches
  if (filter_patch_size) {
    log_message("Sieving out patch sizes smaller than {min_patch_size} planning units...")
    manage_zones[[1]] <- terra::sieve(manage_zones[[1]], threshold = min_patch_size)
    manage_zones[[2]] <- terra::sieve(manage_zones[[2]], threshold = min_patch_size)
  }

  # Save to disk if needed
  if (!is.null(output_path)) {
    filename <- glue::glue("{output_path}/manage_zone_{iso3}.tif")

    elsar::save_raster(
      manage_zones,
      filename = filename,
      datatype = "INT1U"
    )

    # Also save the region-aligned intermediate inputs for inspection (mirrors
    # make_degraded_areas). Each is guarded because some inputs are optional, and
    # masked to the planning units so cells outside the study area are NoData.
    pu_mask <- function(r) terra::mask(r, pus)
    if (exists("agricultural_areas_processed", inherits = FALSE) && inherits(agricultural_areas_processed, "SpatRaster")) {
      elsar::save_raster(pu_mask(agricultural_areas_processed), glue::glue("{output_path}/agriculture_areas_{iso3}.tif"), datatype = "FLT4S")
    }
    if (exists("pasturelands_processed", inherits = FALSE) && inherits(pasturelands_processed, "SpatRaster")) {
      elsar::save_raster(pu_mask(pasturelands_processed), glue::glue("{output_path}/pasturelands_{iso3}.tif"), datatype = "FLT4S")
    }
    if (exists("built_areas_processed", inherits = FALSE) && inherits(built_areas_processed, "SpatRaster")) {
      elsar::save_raster(pu_mask(built_areas_processed), glue::glue("{output_path}/built_areas_{iso3}.tif"), datatype = "FLT4S")
    }
    if (exists("hii_middle_60_pct", inherits = FALSE) && inherits(hii_middle_60_pct, "SpatRaster")) {
      elsar::save_raster(pu_mask(hii_middle_60_pct), glue::glue("{output_path}/hii_mid60pct_{iso3}.tif"), datatype = "INT1U")
    }
    if (exists("managed_forests_processed", inherits = FALSE) && inherits(managed_forests_processed, "SpatRaster")) {
      elsar::save_raster(pu_mask(managed_forests_processed), glue::glue("{output_path}/managed_forests_{iso3}.tif"), datatype = "FLT4S")
    }
  }

  return(manage_zones)
}

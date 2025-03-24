#' Generate Sustainable Management Zones for ELSA Analysis
#'
#' This function identifies planning units eligible for the "Manage" zone under the ELSA framework.
#' The zone (default) includes areas with a moderate level of human influence (HII between
#' 20th and 80th percentiles), all managed forest types, and agricultural areas, but
#' explicitly excludes built-up areas. An alternative zone includes is limtied to all
#' agricultural areas.
#'
#' Two layers are returned:
#' - `manage_zone_v1`: Based on HII quantile range, agriculture, and managed forests (excluding built-up areas)
#' - `manage_zone_v2`: Simplified zone based on agriculture only
#'
#' If `output_path` is provided, both layers are saved as a (multiband) Cloud Optimized GeoTIFF (COG).
#'
#' @param iso3 Character. ISO3 country code (e.g., "CHL").
#' @param pus SpatRaster. Planning units raster to which all inputs are aligned.
#' @param managed_forests_input SpatRaster. A raster of forest classes to identify managed forests.
#' @param agricultural_areas_input SpatRaster. A binary or categorical raster representing agricultural areas.
#' @param built_areas_input SpatRaster. A binary or categorical raster representing built-up/urban areas.
#' @param lulc_raster SpatRaster or NULL. Optional raw LULC input used to extract agriculture and built-up layers if those inputs are categorical.
#' @param hii_input SpatRaster. Human Footprint Index raster.
#' @param hii_threshold Numeric. Threshold to define "middle" HFP range. If 0.2–0.8 quantiles are desired, this is ignored.
#' @param agriculture_lulc_value Integer. LULC value for agriculture (default = 4).
#' @param built_area_lulc_value Integer. LULC value for built-up areas (default = 5).
#' @param forest_classes Integer vector. LULC values representing managed forests (default = c(20, 31, 32, 40, 53)).
#' @param forest_class_threshold Numeric. Minimum proportion of managed forest in a cell to include (default = 0.1).
#' @param agriculture_threshold Numeric. Minimum proportion of agriculture in a cell to include (default = 0.1).
#' @param built_areas_threshold Numeric. Threshold above which cells are considered built-up and excluded (default = 0.1).
#' @param filter_patch_size Logical. Whether to remove small patches (default = TRUE).
#' @param min_patch_size Integer. Minimum size of patches to retain (in cells; default = 10).
#' @param output_path Character or NULL. If provided, save result to this path as a COG (default = NULL).
#'
#' @return A SpatRaster with two layers: `manage_zone_v1` and `manage_zone_v2`.
#' @export
#'
#' @import terra
#' @import glue
#' @import assertthat
#' @import elsar
#'
#' @examples
#' \dontrun{
#' manage_zone <- make_manage_zone(
#'   iso3 = "CHL",
#'   pus = planning_units,
#'   managed_forests_input = forest_raster,
#'   agricultural_areas_input = agri_raster,
#'   built_areas_input = built_raster,
#'   lulc_raster = landcover_raster,
#'   hii_input = hfp_raster,
#'   output_path = "outputs/"
#' )
#' }

make_manage_zone <- function(
    iso3,
    pus,
    managed_forests_input,
    agricultural_areas_input = NULL,
    built_areas_input = NULL,
    lulc_raster = NULL,
    hii_input = NULL,
    hii_threshold = 10,
    agriculture_lulc_value = 5,
    built_area_lulc_value = 7,
    forest_classes = c(20, 31, 32, 40, 53),
    forest_class_threshold = 0.1,
    agriculture_threshold = 0.1,
    built_areas_threshold = 0.1,
    filter_patch_size = TRUE,
    min_patch_size = 10,
    output_path = NULL
) {

  # Input validation
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a character string.")
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")

  # Ensure agricultural/built data is supplied or can be derived
  assertthat::assert_that(
    !(is.null(agricultural_areas_input) && is.null(built_areas_input) && is.null(lulc_raster)),
    msg = "Either 'agricultural_areas_input' and/or 'built_areas_input' must be provided, or 'lulc_raster' must be supplied to derive them."
  )

  # Check required raster inputs
  assertthat::assert_that(inherits(hii_input, "SpatRaster"), msg = "'hii_input' must be a SpatRaster.")
  assertthat::assert_that(inherits(managed_forests_input, "SpatRaster"), msg = "'managed_forests_input' must be a SpatRaster.")

  # If provided, validate optional rasters
  if (!is.null(agricultural_areas_input)) {
    assertthat::assert_that(inherits(agricultural_areas_input, "SpatRaster"), msg = "'agricultural_areas_input' must be a SpatRaster.")
  }
  if (!is.null(built_areas_input)) {
    assertthat::assert_that(inherits(built_areas_input, "SpatRaster"), msg = "'built_areas_input' must be a SpatRaster.")
  }
  if (!is.null(lulc_raster)) {
    assertthat::assert_that(inherits(lulc_raster, "SpatRaster"), msg = "'lulc_raster' must be a SpatRaster.")
  }

  # Process agricultural areas
  cat("Processing agricultural areas...\n")
  if (!is.null(agricultural_areas_input)) {
    agricultural_areas <- agricultural_areas_input
  } else {
    agricultural_areas <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "bilinear",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == agriculture_lulc_value, 1, 0)
    )
  }

  # Process built-up areas
  cat("Processing built-up areas...\n")
  if (!is.null(built_areas_input)) {
    built_areas <- built_areas_input
  } else {
    built_areas <- elsar::make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "bilinear",
      input_raster_conditional_expression = function(x)
        terra::ifel(x == built_area_lulc_value, 1, 0)
    )
  }

  # Normalize HFP and extract middle 60%
  cat("Processing HII layer and extracting middle 60% quantile range...\n")
  hii_resampled <- elsar::make_normalised_raster(
    raster_in = hii_input,
    pus = pus,
    iso3 = iso3,
    rescale = FALSE,
    method_override = "bilinear"
  )

  breaks <- terra::global(hii_resampled, fun = quantile, probs = c(0.2, 0.8), na.rm = TRUE)
  hii_middle_60_pct <- terra::ifel(hii_resampled >= breaks[,1] & hii_resampled <= breaks[,2], 1, 0)

  # Process managed forests
  cat("Processing managed forests...\n")
  # Process managed forests
  # First: crop the raster at native resolution to the extent of PUs - this will speed
  # up processing of the global raster significantly.
  managed_forests_cropped <- terra::crop(
    managed_forests_input,
    terra::project(
      pus,
      y = terra::crs(
        managed_forests_input,
        proj = TRUE)
      )
    )

  # Normalise and reclass
  managed_forests <- elsar::make_normalised_raster(
    raster_in = managed_forests_cropped,
    pus = pus,
    iso3 = iso3,
    method_override = "bilinear",
    input_raster_conditional_expression = function(x)
      terra::classify(x, rcl = matrix(c(
        forest_classes, rep(1, length(forest_classes))
      ), ncol = 2), others = 0)
  )

  # Main management zone: moderate HFP, OR managed forests, OR ag areas — minus built-up
  cat("Creating the default manage zone using middle 60% of HII value, managed forests, and agricultural areas...\n")
  manage_zone <- terra::ifel(
    hii_middle_60_pct == 1 |
      managed_forests > forest_class_threshold |
      agricultural_areas > agriculture_threshold,
    1, 0
  )

  # Exclude built-up areas
  cat("Excluding built areas from the manage zone...\n")
  manage_zone <- terra::ifel(built_areas > built_areas_threshold, 0, manage_zone)

  # Secondary zone (agriculture only)
  cat("Creating the alternative manage zone using agricultural areas only...\n")
  manage_zone_alt <- terra::ifel(agricultural_areas > agriculture_threshold, 1, 0)

  # Combine into multi-layer SpatRaster
  manage_zones <- c(manage_zone, manage_zone_alt)
  names(manage_zones) <- c("manage_zone_v1", "manage_zone_v2")

  # Filter out small patches
  if (filter_patch_size) {
    cat(glue::glue("Sieving out patch sizes smaller than {min_patch_size} planning units..."), "\n")
    manage_zones[[1]] <- terra::sieve(manage_zones[[1]], threshold = min_patch_size)
    manage_zones[[2]] <- terra::sieve(manage_zones[[2]], threshold = min_patch_size)
  }

  # Save to disk if needed
  if (!is.null(output_path)) {
    filename <- glue::glue("{output_path}/manage_zone_{iso3}.tif")
    cat(glue::glue("Saving manage zone rasters to: {filename}..."), "\n")
    terra::writeRaster(
      manage_zones,
      filename = filename,
      filetype = "COG",
      datatype = "INT1U",
      gdal = c(
        "COMPRESS=ZSTD",
        "PREDICTOR=1",
        "NUM_THREADS=ALL_CPUS",
        "OVERVIEWS=NONE"
      ),
      overwrite = TRUE
    )
  }

  return(manage_zones)
}

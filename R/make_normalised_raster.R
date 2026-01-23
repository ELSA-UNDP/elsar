#' Create a Normalised Raster Aligned with and masked by Planning Units
#'
#' This function takes an input raster, aligns it with specified planning units (PUs),
#' optionally inverts, rescales, applies conditional expressions, and saves the processed
#' raster to a specified output path.
#'
#' @param raster_in `SpatRaster` The input raster to be processed.
#' @param pus `SpatVector` The planning units (PUs) to align the raster to.
#' @param iso3 `character` ISO3 country code used for naming the output file.
#' @param invert `logical` If `TRUE`, inverts the raster values (default: `FALSE`).
#' @param rescaled `logical` If `TRUE`, rescales the raster using `rescale_raster()` (default: `TRUE`).
#' @param method_override `character` Optional method for `terra::project()`, overriding
#'        the default (default: `NULL`).
#' @param crop_global_input  `logical` If true the input (large global) raster is cropped
#'        to the PU extent before applying an `input_raster_conditional_expression`, to reduce
#'        the are of processing (default: `TRUE`).
#' @param input_raster_conditional_expression `function` Optional method to apply a
#'        function to the raster before resampling to the PU layer (default: `NULL`).
#' @param conditional_expression `function` Optional method to apply a function to the
#'        raster after resampling to the PU layer (default: `NULL`).
#' @param fill_na `numeric` or `NA` The fill value to use to fill in `NA` values before
#'        masking (default: 0).
#' @param name_out `character` The name of the output raster file (without the extension).
#' @param output_path `character` The directory path to save the output raster (default: `NULL`, i.e., not saved).
#' @param threads Optional method to use multi-core processing - to speed on some `terra`
#'        functions (default: `TRUE`).
#'
#' @details This function reprojects the input raster (`raster_in`) to match the CRS and resolution
#' of the planning units (`pus`). The method for reprojection can be overridden using `method_override`.
#' If `input_raster_conditional_expression` is provided, it is applied before any reprojection. Applying a
#' `input_raster_conditional_expression` can also make processing times significantly longer
#' for high resolution input rasters.The function can optionally rescale (0-1) and invert the raster values.
#'
#' @return Returns a `SpatRaster` object that has been reprojected and processed.
#' If `output_path` is specified, saves the raster as a COG (Cloud Optimized GeoTIFF).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' raster_out <- make_normalised_raster(
#'   raster_in = my_raster,
#'   pus = my_pus,
#'   iso3 = "USA",
#'   invert = TRUE,
#'   rescaled = FALSE,
#'   name_out = "aligned_raster",
#'   output_path = "/path/to/output"
#' )
#'
#' # Applies a conditional expression to the layer that hase been projected to the
#'   planning unit layer.
#' inverted_ndvi <- make_normalised_raster(
#'   raster_in = ndvi_raster,
#'   pus = pus,
#'   iso3 = iso3,
#'   invert = TRUE,
#'   conditional_expression = function(x) terra::ifel(x < 0, NA, 1 - x)
#'   )
#'
#' # Convert a landcover classifgiation layer into a binary - applied a conditional
#'   function to the input layer, not the layer that has been projected to the planning
#'   units.
#' raster_out <- make_normalised_raster(
#'   raster_in = land_cover_raster,
#'   pus = my_pus,
#'   iso3 = "USA",
#'   crop_global_input = FALSE # ESRI LULC rasters are already export from at the PU extent
#'   input_raster_conditional_expression = function(r) ifel(r %in% c(1:4, 7, 9), 1, 0)
#'   )
#'
#' # For high resolution rasters (10m Sentinel based LULC for example) that cover a large
#'   area, it may be more efficient and much faster to pre-create binary class rasters
#'   (e.g., built areas, agriculture areas) using tools like gdal_calc or GEE.
#'
#' built_areas_raster <- terra::rast("built_areas_brazil.tif")
#'
#' urban_areas <- make_normalised_raster(
#'   raster_in = built_areas_raster,
#'   pus = my_pus,
#'   iso3 = "BRA"
#'   )
#' }
#'
make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   invert = FALSE,
                                   rescaled = TRUE,
                                   method_override = NULL,
                                   crop_global_input = TRUE,
                                   input_raster_conditional_expression = NULL,
                                   conditional_expression = NULL,
                                   fill_na = 0,
                                   name_out,
                                   output_path = NULL,
                                   threads = TRUE) {
  # Valid methods for exactextractr::exact_resample
  valid_resampling_methods <- c(
    "mean",     # Area-weighted mean
    "sum",      # Area-weighted sum
    "min",      # Minimum value
    "max",      # Maximum value
    "mode",     # Most frequent value
    "majority", # Same as mode (alias)
    "minority", # Least frequent value
    "variety",  # Number of unique values
    "count",    # Count of raster cells
    "stdev",    # Standard deviation
    "coefficient_of_variation" # stdev / mean
  )

  if (!is.null(method_override)) {
    if (!(method_override %in% valid_resampling_methods)) {
      stop("Invalid method_override. Choose one of: ", paste(valid_resampling_methods, collapse = ", "))
    }
    method <- method_override
  } else {
    method <- "mean"
  }

  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Check if raster was pre-aggregated in GEE (has pre_aggregated attribute)
  is_pre_aggregated <- isTRUE(attr(raster_in, "pre_aggregated"))

  if (is_pre_aggregated) {
    # Skip expensive resampling - data is already at PU resolution
    log_message("Input raster was pre-aggregated in GEE. Skipping local resampling.")

    # Just align and mask to PUs
    dat_aligned <- terra::project(raster_in, y = pus, threads = threads)

    max_val <- suppressWarnings(terra::minmax(dat_aligned)[2])
    if (is.null(dat_aligned) || isTRUE(is.na(max_val)) || isTRUE(max_val == 0)) {
      log_message("No valid raster values found: returning empty raster.")
      return(terra::ifel(pus == 1, 0, NA))
    }

    # Apply input conditional expression if provided (for class extraction)
    if (!is.null(input_raster_conditional_expression)) {
      # Warn if applying class extraction to mode-aggregated categorical data
      aggregation_reducer <- attr(raster_in, "aggregation_reducer")
      if (!is.null(aggregation_reducer) && aggregation_reducer == "mode") {
        warning(
          "Applying conditional expression to pre-aggregated categorical data (mode reducer). ",
          "This will produce binary (0/1) values per PU, not class proportions. ",
          "For proper class proportions, use download_lulc_class_proportion() or pass ",
          "pre-computed class proportion rasters via agricultural_areas_input/built_areas_input parameters.",
          call. = FALSE
        )
      }
      log_message("Applying conditional expression to pre-aggregated raster...")
      dat_aligned <- input_raster_conditional_expression(dat_aligned)
    }

  } else {
    # Standard processing path with full resampling

    # Crop before reprojection if a conditional expression is to be applied and the raw input is a large global raster
    if (!is.null(input_raster_conditional_expression)) {
      if (crop_global_input) {
        log_message("Cropping input raster before applying conditional expression...")
        cropped <- crop_global_raster(raster_in, pus)
        log_message("Applying conditional expression to cropped input raster...")
        raster_in <- input_raster_conditional_expression(cropped)
      } else {
        log_message("Applying conditional expression to input raster...")
        raster_in <- input_raster_conditional_expression(raster_in)
      }
    }

    # Reproject and align raster to match PUs
    dat_aligned <- terra::project(raster_in, y = pus, threads = threads)

    max_val <- suppressWarnings(terra::minmax(dat_aligned)[2])
    if (is.null(dat_aligned) || isTRUE(is.na(max_val)) || isTRUE(max_val == 0)) {
      log_message("No valid raster values found: returning empty raster.")
      return(terra::ifel(pus == 1, 0, NA))
    }

    # Resample using exactextractr to match PU resolution
    dat_aligned <- exactextractr::exact_resample(x = dat_aligned, y = pus, fun = method)
  }

  if (!is.null(conditional_expression)) {
    dat_aligned <- conditional_expression(dat_aligned)
  }

  if (!is.null(fill_na)) {
    dat_aligned[is.na(dat_aligned)] <- fill_na
    dat_aligned <- terra::mask(dat_aligned, pus, maskvalues = NA)
  }

  if (invert) {
    dat_aligned <- -dat_aligned
  }

  if (rescaled) {
    dat_aligned <- elsar::rescale_raster(dat_aligned)
  } else {
    warning("NOTE: Raster values are NOT rescaled.")
  }

  if (!is.null(output_path)) {
    data_type <- if (terra::is.int(dat_aligned)) "INT1S" else "FLT4S"
    na_value <- if (terra::is.int(dat_aligned)) 255 else -9999

    gdal_options <- c(
      "COMPRESS=ZSTD",
      "NUM_THREADS=ALL_CPUS",
      "OVERVIEWS=NONE",
      if (data_type == "INT1S") "PREDICTOR=2" else if (data_type == "FLT4S") "PREDICTOR=3"
    )

    gdal_options <- gdal_options[!is.na(gdal_options)]

    terra::writeRaster(
      dat_aligned,
      glue::glue("{output_path}/{name_out}_{iso3}.tif"),
      gdal = gdal_options,
      datatype = data_type,
      NAflag = na_value,
      overwrite = TRUE,
      filetype = "COG"
    )
  }

  return(dat_aligned)
}

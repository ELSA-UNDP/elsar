#' Create a Normalized Raster Aligned with and masked by Planning Units
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
#' @param method_override `character` Optional method for `terra::project()`, overriding the default (default: `NULL`).
#' @param input_raster_conditional_expression `function` Optional method to apply a function to the raster before resampling to the PU layer (default: `NULL`).
#' @param conditional_expression `function` Optional method to apply a function to the raster after resampling to the PU layer (default: `NULL`).
#' @param fill_na `logical` If `TRUE`, fills `NA` values with 0 before masking (default: `TRUE`).
#' @param name_out `character` The name of the output raster file (without the extension).
#' @param output_path `character` The directory path to save the output raster (default: `NULL`, i.e., not saved).
#' @param threads Optional method to use multi-core processing - to speed on some `terra` functions (default: `TRUE`).
#'
#' @details This function reprojects the input raster (`raster_in`) to match the CRS and resolution
#' of the planning units (`pus`). The method for reprojection can be overridden using `method_override`.
#' If `input_raster_conditional_expression` is provided, it is applied before any reprojection. The function can
#' optionally rescale (0-1) and invert the raster values. It can also make processing times
#' significantly longer for high resolution input rasters.
#'
#' @return Returns a [SpatRaster] object that has been reprojected and processed.
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
#'   input_raster_conditional_expression = function(r) ifel(r %in% c(1:4, 7, 9), 1, 0)
#' )
#' }
#'
make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   invert = FALSE,
                                   rescaled = TRUE,
                                   method_override = NULL,
                                   input_raster_conditional_expression = NULL,
                                   conditional_expression = NULL,
                                   fill_na = TRUE,
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

  # then crop (make data a lot smaller)
  # dat_aligned <- terra::crop(raster_in, pus_reproject) %>%
  #  terra::subst(., NA, 0)

  # Temporarily reproject pus to the CRS of raster_in for resolution comparison
  pus_temp <- terra::project(pus, terra::crs(raster_in), threads = threads)

  # Calculate resolutions of both rasters in the same CRS
  raster_res <- min(terra::res(raster_in))    # Minimum resolution of input raster
  pus_res <- min(terra::res(pus_temp))        # Minimum resolution of re-projected pus

  # Determine the appropriate projection method based on data type and resolution comparison
  if (!is.null(method_override)) {
    if (!(method_override %in% valid_resampling_methods)) {
      stop("Invalid method_override. Choose one of: ", paste(valid_resampling_methods, collapse = ", "))
    }
    method <- method_override  # Use the override method if provided and valid
  } else {
    # Default method selection based on data type and resolution comparison
    method <- "mean"
    }

  # Re-project the original raster_in to the CRS of the PUs
  dat_aligned <- elsar::crop_global_raster(
    raster_in = raster_in,
    pus = pus) %>%
    terra::project(
      y = terra::crs(pus),
      threads = threads
      )

  if (is.null(dat_aligned) || isTRUE(is.na(suppressWarnings(terra::minmax(dat_aligned)[2]))) || terra::minmax(dat_aligned)[2] == 0) {
    log_msg("No valid raster values found — returning empty raster.")
    return(terra::ifel(pus == 1, 0, NA))
  } else {
    # Apply the conditional expression if provided to the input raster - for example if the input is landcover
    # dataset that you want to reclass before doing any aggregation.
    if (!is.null(input_raster_conditional_expression)) {
      dat_aligned <- input_raster_conditional_expression(dat_aligned)
    }

    # Resample the projected raster to the resolution of the PUs using exactextractr::exact_resample and the method provided
    dat_aligned <- exactextractr::exact_resample(x = dat_aligned, y = pus, fun = method)

    # Apply the conditional expression if provided
    if (!is.null(conditional_expression)) {
      dat_aligned <- conditional_expression(dat_aligned)
    }

    # Fill in NA background values before masking
    if (fill_na) {
      dat_aligned[is.na(dat_aligned)] <- 0
      dat_aligned <- dat_aligned  %>%
        terra::mask(pus, maskvalues = 0) # Mask areas outside of planning units
    }

    # Invert values if required
    if (invert) {
      dat_aligned <- -dat_aligned
    }

    # Rescale the raster values if needed
    if (rescaled) {
      dat_aligned <- elsar::rescale_raster(dat_aligned)
    } else {
      warning("NOTE: Raster values are NOT rescaled.")
    }
  }

  # Save the output file if output_path is provided
  if (!is.null(output_path)) {
    # Determine the appropriate data type and NoData value for saving the raster
    data_type <- if (terra::is.int(dat_aligned)) "INT1S" else "FLT4S"  # Adjust data type
    na_value <- if (terra::is.int(dat_aligned)) 255 else -9999         # Adjust NoData value

    # Dynamically set GDAL options while avoiding NULL values
    gdal_options <- c(
      "COMPRESS=ZSTD",
      "NUM_THREADS=ALL_CPUS",
      "OVERVIEWS=NONE",
      if (data_type == "INT1S") "PREDICTOR=2" else if (data_type == "FLT4S") "PREDICTOR=3"
    )

    # Remove NULL values (in case no PREDICTOR was added)
    gdal_options <- gdal_options[!is.na(gdal_options)]

    # Write the raster with the determined settings
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

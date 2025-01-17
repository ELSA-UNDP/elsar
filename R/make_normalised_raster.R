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
#' @param conditional_expression `function` Optional method to apply a function to the raster before resampling to the PU layer (default: `NULL`).
#' @param fill_na `logical` If `TRUE`, fills `NA` values with 0 before masking (default: `TRUE`).
#' @param name_out `character` The name of the output raster file (without the extension).
#' @param output_path `character` The directory path to save the output raster (default: `NULL`, i.e., not saved).
#'
#' @details This function reprojects the input raster (`raster_in`) to match the CRS and resolution
#' of the planning units (`pus`). The method for reprojection can be overridden using `method_override`.
#' If `conditional_expression` is provided, it is applied before any reprojection. The function can
#' optionally rescale (0-1) and invert the raster values.
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
#' raster_out <- make_normalised_raster(
#'   raster_in = my_raster,
#'   pus = my_pus,
#'   iso3 = "USA",
#'   conditional_expression = function(r) ifel(r %in% c(1:4, 7, 9), 1, 0)
#' )
#' }
#'
make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   invert = FALSE,
                                   rescaled = TRUE,
                                   method_override = NULL,
                                   conditional_expression = NULL,
                                   fill_na = TRUE,
                                   name_out,
                                   output_path = NULL) {
  # Valid methods for terra::project
  valid_terra_methods <- c("near", "bilinear", "cubic", "cubicspline", "lanczos", "min", "q1", "med", "q3", "max", "average", "mode", "rms")


  # then crop (make data a lot smaller)
  # dat_aligned <- terra::crop(raster_in, pus_reproject) %>%
  #  terra::subst(., NA, 0)

  # Temporarily reproject pus to the CRS of raster_in for resolution comparison
  pus_temp <- terra::project(pus, terra::crs(raster_in))

  # Calculate resolutions of both rasters in the same CRS
  raster_res <- min(terra::res(raster_in))    # Minimum resolution of input raster
  pus_res <- min(terra::res(pus_temp))        # Minimum resolution of re-projected pus


  # Determine the appropriate projection method based on data type and resolution comparison
  if (!is.null(method_override)) {
    if (!(method_override %in% valid_terra_methods)) {
      stop("Invalid method_override. Choose one of: ", paste(valid_terra_methods, collapse = ", "))
    }
    method <- method_override  # Use the override method if provided and valid
  } else {
    # Default method selection based on data type and resolution comparison
    method <- if (terra::is.int(raster_in)) {
      if (raster_res >= pus_res) "near" else "mode"
    } else {
      if (raster_res >= pus_res) "near" else "bilinear"
    }
  }

  # Apply the conditional expression if provided
  if (!is.null(conditional_expression)) {
    raster_in <- conditional_expression(raster_in)
  }

  # Re-project the original raster_in to match PUs with the selected method
  dat_aligned <- terra::project(
    x = raster_in,
    y = pus,
    method = method,
    threads = TRUE
  )

  # Fill in NA background values before masking
  if (fill_na) {
    dat_aligned[is.na(dat_aligned)] <- 0
    dat_aligned <- dat_aligned %>% terra::mask(pus) # Mask areas outside of planning units
  }

  # Invert values if required
  if (invert) {
    dat_aligned <- -dat_aligned
  }

  # Rescale the raster if needed
  if (rescaled) {
    dat_aligned <- rescale_raster(dat_aligned)
  } else {
    warning("NOTE: Raster is NOT rescaled.")
  }

  # Save the output file if output_path is provided
  if (!is.null(output_path)) {
    # Determine the appropriate data type and NoData value for saving the raster
    data_type <- if (terra::is.int(dat_aligned)) "INT1S" else "FLT4S"  # Adjust data type
    na_value <- if (terra::is.int(dat_aligned)) 255 else -9999         # Adjust NoData value

    # Write the raster with the determined settings
    terra::writeRaster(
      dat_aligned,
      glue::glue("{output_path}/{name_out}_{iso3}.tif"),
      gdal = c("COMPRESS=ZSTD", "NUM_THREADS=4", "OVERVIEWS=NONE", "PREDICTOR=3"),
      datatype = data_type,
      NAflag = na_value,
      overwrite = TRUE,
      filetype = "COG"
    )
  }

  return(dat_aligned)
}

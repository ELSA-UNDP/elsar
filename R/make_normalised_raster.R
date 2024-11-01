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

  dat_aligned <- dat_aligned |>
    terra::mask(pus) # Mask areas outside of planning units

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

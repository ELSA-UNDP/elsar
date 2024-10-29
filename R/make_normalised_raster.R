make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   invert = FALSE,
                                   rescaled = TRUE,
                                   name_out,
                                   output_path = NULL) {
  # Reproject the planning units (PUs) to match the projection of the input raster
  pus_reproject <- terra::project(pus, terra::crs(raster_in))

  # Crop the input raster to the extent of the planning units
  dat_aligned <- terra::crop(raster_in, pus_reproject)

  # Replace NA values in the cropped raster with 0
  dat_aligned[is.na(dat_aligned)] <- 0

  # Align projection and extent of the raster with the planning units
  dat_aligned <- dat_aligned %>%
    terra::project(terra::crs(pus)) %>%  # Project to original PU CRS
    terra::resample(pus) %>%             # Resample to match resolution and extent of PUs
    terra::mask(pus, maskvalues = 0)     # Mask areas outside of planning units (assuming outside = 0)

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
    if (terra::is.int(dat_aligned)) {
      data_type <- "INT2S"  # 16-bit signed integer
      na_value <- 255       # Byte-specific NoData value
    } else {
      data_type <- "FLT4S"  # 32-bit float
      na_value <- -9999     # Float-specific NoData value
    }

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

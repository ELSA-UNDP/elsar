#' Function to extract general raster data
#'
#' `make_normalised_raster()`allows you to align, normalise and save a raster file. This is applicable for certified forests, etc. (ADD LATER)
#'
#' @param raster_in A raster file that contains the data to be put into right format
#' @param pus A raster file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param name_out A string with the data name that will be used for the output `tif`file
#' @param write_file A logical command on whether the file will be saved rather than given as an output variable (Default: TRUE)
#' @param output_path An output path for the created file.
#'
#' @return A `raster`file that has been aligned and normalised
#' @export
#'
make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   name_out,
                                   write_file = TRUE,
                                   output_path) {
  # reprojecting the global data would take too long
  # to speed up: reproject PUs to projection of global data first
  pus_reproject <- terra::project(pus, terra::crs(raster_in))

  # then crop (make data a lot smaller)
  dat_aligned <- terra::crop(raster_in, pus_reproject) %>%
    terra::project(., terra::crs(pus)) %>% # reproject the data to the crs we actually want (the original pu crs)
    terra::resample(., pus) %>%
    terra::mask(pus, maskvalues = 0) # maskvalues denotes the background value in the raster that's not data (since this should always be planning region/units is 1 and outside is 0, this is hard-coded to 0)

  raster_rescaled <- rescale_raster(dat_aligned)

  if (write_file == FALSE) {
    return(raster_rescaled)
  } else if (write_file == TRUE) {
    terra::writeRaster(raster_rescaled,
                       glue::glue("{output_path}/{name_out}_{iso3}.tif"),
                       gdal = c("COMPRESS=DEFLATE"),
                       NAflag = -9999,
                       overwrite = TRUE
    )
  } else {
    message("write_file expects a TRUE or FALSE input.
            TRUE saves the output, while FALSE allows you to load the output
            into your R environment.")
  }
}

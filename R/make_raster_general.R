#' Function to extract general raster data
#'
#' `make_raster_general()`allows you to align, normalise and save a raster file. This is applicable for certified forests, etc. (ADD LATER)
#'
#' @param rasterIn A raster file that contains the data to be put into right format
#' @param rasterRef A raster file that contains the reference spatial extent, crs etc.
#' @param rasterMask Calculate this from rasterRef?
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param nameOut A string with the data name that will be used for the output `tif`file
#' @param writeFile A logical command on whether the file will be saved rather than given as an output variable (Default: TRUE)
#'
#' @return A raster file that has been aligned and normalised
#' @export
#'
make_raster_general <- function(rasterIn, rasterRef, rasterMask, iso3, nameOut, writeFile = TRUE) {
  gdalUtils::align_rasters( #gdalUtils was archived from CRAN Switch to terra?
    unaligned = rasterIn,
    reference = rasterRef,
    dstfile = "tmp.tif",
    nThreads = 4,
    r = "average",
    overwrite = TRUE
  )

  rasterAligned <- terra::rast("tmp.tif")
  rasterAligned[is.na(rasterAligned)] <- 0
  rasterAligned <- terra::rescale(rasterAligned * rasterMask)

  if (writeFile == FALSE) {
    return(rasterAligned)
  } else if (writeFile == TRUE) {
    terra::writeRaster(rasterAligned,
      paste0(nameOut, "_", iso3, ".tif"),
      options = c("compress=deflate", "tiled=yes"),
      NAflag = -9999,
      overwrite = T
    )
  } else {
    message("writeFile expects a TRUE or FALSE input.
            TRUE saves the output, while FALSE allows you to load the output
            into your R environment.")
  }

  file.remove("tmp.tif")
  gc()
}

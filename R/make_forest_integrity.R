#' Create the forest integrity index using flii and fsii data
#'
#' @param raster_flii A `SpatRaster` file with the flii information.
#' @param raster_fsii A `SpatRaster` file with the fsii information.
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param output_path An optional output path for the created file.
#'
#' @return A `SpatRaster` file of the forest integrity index that has been aligned and normalised
#' @export
#'
#' @examples
#' \dontrun{
#' forest_integrity <- make_forest_integrity(
#'   raster_flii = raster_flii,
#'   raster_fsii = raster_fsii,
#'   pus = pus
#' )
#' }
make_forest_integrity <- function(raster_flii,
                                  raster_fsii,
                                  pus,
                                  iso3,
                                  output_path = NULL) {
  # crop and align to pus
  suppressWarnings(flii <- make_normalised_raster(
    raster_in = raster_flii,
    pus = pus,
    rescaled = FALSE
  ))

  suppressWarnings(fsii <- make_normalised_raster(
    raster_in = raster_fsii,
    pus = pus,
    rescaled = FALSE
  ))

  # get mean of fsii and flii and rescale
  fi <- terra::mean(fsii + flii)
  raster_rescaled <- rescale_raster(fi)

  if (!is.null(output_path)) {
    terra::writeRaster(raster_rescaled,
      glue::glue("{output_path}/forest_integrity_{iso3}.tif"),
      gdal = c("COMPRESS=DEFLATE"),
      NAflag = -9999,
      overwrite = TRUE,
      filetype = "COG"
    )
  }

  return(raster_rescaled)
}

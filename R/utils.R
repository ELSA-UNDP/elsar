#' Rescale rasters to between 0 and 1
#'
#' @param raster_in Input raster
#' @param raster_in_min Min value of raster
#' @param raster_in_max Max value of raster
#' @param new_min 0
#' @param new_max 1
#'
#' @return `raster` file that is normalised from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' raster_rescaled <- rescale_raster(dat_aligned)
#' }
rescale_raster <- function(raster_in,
                           raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min,
                           raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max,
                           new_min = 0,
                           new_max = 1){
  if(is.null(raster_in_min)) raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min
  if(is.null(raster_in_max)) raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max
  new_min + (raster_in - raster_in_min) * ((new_max - new_min) / (raster_in_max - raster_in_min))
}

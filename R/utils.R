#' Rescale rasters to between 0 and 1
#'
#' @param x Input raster
#' @param x.min Min value of raster
#' @param x.max Max value of raster
#' @param new.min 0
#' @param new.max 1
#'
#' @return `raster` file that is normalised from 0 to 1
#' @export
#'
#' @examples
rescale_raster <- function(x,
                           x.min = global(x, min, na.rm = TRUE)$min,
                           x.max = global(x, max, na.rm = TRUE)$max,
                           new.min = 0,
                           new.max = 1){
  if(is.null(x.min)) x.min = global(x, min, na.rm = TRUE)$min
  if(is.null(x.max)) x.max = global(x, max, na.rm = TRUE)$max
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}

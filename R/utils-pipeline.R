#' Extract binary layers within a raster stack
#'
#' This function extracts all layers in a raster stack that have either c(0,1) or only 1 or only 0.
#' This is needed to split the raster stack saved from the pipeline into two raster stacks, one int and one float.
#'
#' @param raster_stack A `terra` `SpatRaster` containing several raster layers, some of which can be binary.
#'
#' @return Two `terra` `SpatRaster` raster stacks, one containing only the binary layers, and one containing the float layers
#' @export
#'
#' @examples
#' \dontrun{
#' raster_out <- get_binary_layers(raster_stack = stack)
#'
#' int_stack <- raster_out[[1]]
#' float_stack <- raster_out[[2]]
#' }
get_binary_layers <- function(raster_stack) {
  assertthat::assert_that(inherits(raster_stack, "SpatRaster"))

  # get frequency of values per raster layer
  freq_tab <- terra::freq(raster_stack, digits=4, bylayer=TRUE)

  # Split frequency table by layer
  freq_by_layer <- split(freq_tab, freq_tab$layer)

  # Identify binary layers: only values 0, 1, or both
  binary_mask <- sapply(freq_by_layer, function(df) {
    all(df$value %in% c(0, 1))
  })

  # Subset only if binary layers were found
  if (any(binary_mask, na.rm = TRUE)) {
    int_layers <- raster_stack[[which(binary_mask)]]
    float_layers <- raster_stack[[which(!binary_mask)]]
  } else {
    warning("No binary layers found.")
    int_layers <- NULL
    float_layers <- raster_stack
  }

  return(list(int_layers, float_layers))
}

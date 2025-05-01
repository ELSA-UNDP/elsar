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

  # compute min and max per layer
  val_ranges <- terra::global(raster_stack, fun = "range", na.rm = TRUE)

  # Identify binary layers: must contain either c(0, 1) or just 0 or just 1
  binary_mask <- (val_ranges$min == 0 & val_ranges$max == 1) | (val_ranges$min == 0 & val_ranges$max == 0) | (val_ranges$min == 1 & val_ranges$max == 1)

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

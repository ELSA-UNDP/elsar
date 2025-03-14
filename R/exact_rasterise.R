#' Efficient Rasterization of polygon features with Coverage Fraction, based on some attribute
#'
#' Rasterises vector features using the attribute values and their actual coverage
#' fraction over a planning unit (PU) layer. This method is optimized for speed and
#' precision, allowing for faster rasterisation than standard `rasterize()` functions,
#' where polygons in a dataset may overlap or have attributes to map.
#'
#' @param features A spatial feature object (e.g., `sf` or `SpatVector`) containing
#'                 the vector features to be rasterised.
#' @param attribute A string representing the column name in `features` used to assign
#'                  values to each raster cell.
#' @param pu_layer A raster layer (planning unit) that defines the resolution and extent
#'                 for rasterization.
#' @param fun A function used to aggregate values in overlapping layers (default is `sum`).
#'            Other functions, like `mean` or `max`, can also be used.
#'
#' @return A raster stack with each layer representing the rasterized attribute for each
#'         feature, based on some attribute. If multiple features are present, the specified aggregation function
#'         is applied across the stack to produce a single layer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Rasterize using the 'value' attribute with sum aggregation
#' exact_rasterise(features = my_features, attribute = "value", pu_layer = my_raster, fun = sum)
#' }
exact_rasterise <- function(features, attribute, pu_layer, fun = sum, cores = 4) {
  r_stack <- terra::rast()

  if (nrow(features) > 1) {
    for (i in 1:nrow(features)) {
      f <- dplyr::slice(features, i)
      f_r <- exactextractr::coverage_fraction(pu_layer, f)[[1]]
      f_r <- f_r * dplyr::pull(f, attribute) # Map attribute values onto raster cells
      r_stack <- c(r_stack, f_r)

      print(paste0("Feature ", i, " processed. "))

    }
    # Apply the specified function across the raster stack (default: sum)
    r_stack <- terra::app(r_stack, cores = cores, function(r, ...) fun(r, na.rm = TRUE))
  } else {
    r_stack <- exactextractr::coverage_fraction(pu_layer, features)[[1]]
    r_stack <- fun(r_stack, na.rm = TRUE)
  }

  return(r_stack)
}

#' Get wad data saved as tif
#'
#' @rdname wad_data
#' @export
get_wad_data <- function() {
  x <- terra::rast(
    system.file("extdata", "wad_dat.tif", package = "elsaFunctions")
  )
  x
}

#' Data used to extract the Nepal boundary
#'
#' `sf` file containing the Souther Asia boundary information
#'
#' @source https:// add link here
"boundary_dat"

#' Get wad data saved as tif
#'
#' @rdname wad_data
#' @export
get_wad_data <- function() {
  x <- terra::rast(
    system.file("extdata", "wad_dat.tif", package = "elsar")
  )
  x
}

#' Data used to extract the Nepal boundary
#'
#' `sf` file containing the Souther Asia boundary information
#'
#' @source https:// add link here
"boundary_dat"


#' Data used for current protected areas
#'
#' `current_pas_sf` file from wdpa database
#'
#' @source https:// add link here
"current_pas_sf"





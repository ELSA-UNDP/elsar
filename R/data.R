#' Get wad data saved as tif
#'
#' @rdname wad_subset
#' @export
get_wad_data <- function() {
  x <- terra::rast(
    system.file("extdata", "wad_subset.tif", package = "elsar")
  )
  return(x)
}

#' Data used to extract the Nepal boundary
#'
#' `sf` file containing the Southern Asia boundary information
#'
#' @source https:// add link here
"boundary_dat"


#' Data used for current protected areas
#'
#' `current_pas_sf` file from WDPA database
#'
#' @source https:// add link here
"current_pas_sf"


#' Data used for mangroves
#'
#' `sf` file from WDPA database
#'
#' @source https:// add link here
"mangroves_hti"





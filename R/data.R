#' Get WAD (World Atlas of Desertification) subset data
#'
#' Loads a subset of WAD data saved as a TIF file from the package's
#' extdata directory.
#'
#' @return A `SpatRaster` object containing the WAD subset data.
#' @export
#'
#' @examples
#' \dontrun{
#' wad <- get_wad_data()
#' }
get_wad_data <- function() {

  log_message("Loading WAD subset data from package...")


  file_path <- system.file("extdata", "wad_subset.tif", package = "elsar")

  assertthat::assert_that(
    nzchar(file_path),
    msg = "WAD subset data file not found in package extdata."
  )

  x <- terra::rast(file_path)

  log_message("WAD subset data loaded successfully.")
  return(x)
}

#' Boundary data for Southern Asia
#'
#' An `sf` object containing boundary information for Southern Asia,
#' used for extracting country boundaries (e.g., Nepal).
#'
#' @format An `sf` data frame with polygon geometries.
#' @source GADM database <https://gadm.org/>
"boundary_dat"


#' Current Protected Areas
#'
#' An `sf` object containing current protected areas from the
#' World Database on Protected Areas (WDPA).
#'
#' @format An `sf` data frame with polygon geometries.
#' @source UNEP-WCMC and IUCN WDPA <https://www.protectedplanet.net/>
"current_pas_sf"





#' Function to extract general raster data
#'
#' `make_normalised_raster()`allows you to align, normalise and save a raster file. This is applicable for the following data
#' This is applicable for the following data sets: certified forests, drought risk, flood risk, intact wilderness area, soc difference, threatened species richness, voc, wad convergence evidence
#'
#' @param raster_in A raster file that contains the data to be put into right format
#' @param pus A raster file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param invert Logical. If TRUE, highest values in the original dataset should be valued the lowest in the prioritisation.
#' @param name_out A string with the data name that will be used for the output `tif`file
#' @param output_path An optional output path for the created file.
#'
#' @return A `raster`file that has been aligned and normalised
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd",
#'   do_project = TRUE
#' )
#'
#' pus <- make_planning_units(boundary_proj = boundary_proj,
#'                            pu_size = NULL,
#'                            pu_threshold = 8.5e5,
#'                            limit_to_mainland = FALSE)
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(raster_in = wad_dat,
#'                                  pus = pus,
#'                                  iso3 = "NPL")
make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   invert = FALSE,
                                   name_out,
                                   output_path = NULL) {
  # reprojecting the global data would take too long
  # to speed up: reproject PUs to projection of global data first
  pus_reproject <- terra::project(pus, terra::crs(raster_in))

  # then crop (make data a lot smaller)
  dat_aligned <- terra::crop(raster_in, pus_reproject) %>%
    terra::project(., terra::crs(pus)) %>% # reproject the data to the crs we actually want (the original pu crs)
    terra::resample(., pus) %>%
    terra::mask(pus, maskvalues = 0) # maskvalues denotes the background value in the raster that's not data (since this should always be planning region/units is 1 and outside is 0, this is hard-coded to 0)

  if (invert) {
    dat_aligned = -dat_aligned
  }

  raster_rescaled <- rescale_raster(dat_aligned)

  if (!is.null(output_path)) {
    terra::writeRaster(raster_rescaled,
                       glue::glue("{output_path}/{name_out}_{iso3}.tif"),
                       gdal = c("COMPRESS=DEFLATE"),
                       NAflag = -9999,
                       overwrite = TRUE
    )
  }

  return(raster_rescaled)
}

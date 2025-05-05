#' Function to make standardised mangroves data
#'
#' `make_mangroves()`allows you to align, normalise an `sf`file and save it as a raster file. Originally created to prepare the Global Mangrove Watch data.
#'
#' @param sf_in An `sf` file that contains the data to be put into right format
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param name_out A string with the data name that will be used for the output `tif`file
#' @param output_path An optional output path for the created file.
#'
#' @return A `SpatRaster` file that has been aligned and normalised
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "IND",
#'   iso3_column = "iso3cd"
#'   )
#'
#' # make planning units
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#'   )
#'
#' path_in <- "<yourPath>"
#' ist_dat <- extract_filename_filetype(
#'   data_name = "gmw",
#'   file_path = path_in
#'   )
#'
#' mangroves <- load_data(
#'   file_name = paste0(list_dat["filename"][[1]], list_dat["filetype"][[1]]),
#'   file_path = path_in,
#'   wkt_filter = TRUE,
#'   bb_extend = pus
#'   )
#'
#' mangrove_raster <- make_mangroves(
#'   sf_in = mangroves,
#'   pus = pus
#'   )
#' }
make_mangroves <- function(sf_in, # rename function name later
                           pus,
                           iso3,
                           name_out,
                           output_path = NULL) {
  # re-project sf, turn into raster and normalise
  if (nrow(sf_in) > 0) {
    sf_in <- sf_in %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_make_valid() %>%
      dplyr::summarise()

    raster_out <- exactextractr::coverage_fraction(pus, sf_in)[[1]] %>%
      elsar::make_normalised_raster(
        pus = pus,
        iso3 = iso3
        )

  } else {
    log_msg("No mangrove areas found. Returning an empty raster.")
    raster_out <- terra::ifel(pus == 1, 0, NA)
  }

  #save if wanted
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = raster_out,
      filename = glue::glue("{output_path}/{name_out}_{iso3}.tif"),
      datatype = "FLT4S"
    )
  }
  return(raster_out)
}

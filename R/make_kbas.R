#' Function to make standardised KBA data
#'
#' @param kba_in An `sf` file that contains the data to be put into right format
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param aze_only Logical. TBA explanation by Scott
#' @param incl_regional_kba Logical. Whether to include regional KBAs
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
#' )
#'
#' # make planning units
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#'
#' path_in <- "<yourPath>"
#' ist_dat <- extract_filename_filetype(
#'   data_name = "gmw",
#'   file_path = path_in
#' )
#'
#' kba_in <- load_data(
#'   file_name = paste0(list_dat["filename"][[1]], list_dat["filetype"][[1]]),
#'   file_path = path_in,
#'   file_type = list_dat["filetype"][[1]],
#'   file_lyr = file_layer
#' )
#'
#' kba_raster <- make_kbas(
#'   kba_in = kba_in,
#'   pus = pus,
#'   iso3 = iso3
#' )
#' }
make_kbas <- function(kba_in,
                      pus,
                      iso3,
                      aze_only = FALSE,
                      incl_regional_kba = FALSE,
                      name_out,
                      output_path = NULL) {
  kba_in <- kba_in %>%
    dplyr::filter(iso3 == iso3)

  if (aze_only) {
    kba_in <- kba_in %>%
      dplyr::filter(azestatus == "confirmed")
  }

  if (nrow(kba_in)== 0){
    if (aze_only) {
      cat("No Alliance for Zero Extinction Sites in the study region")
    } else {
      cat("No Key Biodiversity Areas in the study region")
    }

    kba_out <- pus

  } else {

  if (!incl_regional_kba) {
    kba_in <- kba_in %>%
      dplyr::filter(kba_qual %ni% c("Regional", "Global/ Regional to be determined"))
  }

  kba_in <- kba_in %>%
    sf::st_transform(crs = sf::st_crs(pus)) %>%
    sf::st_make_valid() %>%
    dplyr::summarise() %>%
    sf::st_make_valid()

  kba_out <- exactextractr::coverage_fraction(pus, kba_in)[[1]] %>%
    terra::mask(pus, maskvalues = 0) %>%
    rescale_raster()

  kba_out[is.na(kba_out)] <- 0
  }

  #save if wanted
  if (!is.null(output_path)) {
    terra::writeRaster(kba_out,
                       glue::glue("{output_path}/{name_out}_{iso3}.tif"),
                       gdal = c(
                         "COMPRESS=ZSTD",
                         "PREDICTOR=3",
                         "OVERVIEWS=NONE",
                         "NUM_THREADS=ALL_CPUS"
                         ),
                       NAflag = -9999,
                       overwrite = TRUE,
                       filetype = "COG"
    )
  }
  return(kba_out)
}

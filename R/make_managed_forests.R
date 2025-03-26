#' Create managed forests data layer
#'
#' @param raster_in A `SpatRaster` file that contains the data to be put into right format
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param manual_cats If file does not contain default categories (11 (not managed), 20 (disturbed forests; can be interpreted as managed), 31, 32, 40, 53 (all managed)), allows to provide a vector with the right values.
#' @param include_disturbed_forest logical. Whether or not to include disturbed forests as managed forests (default is FALSE).
#' @param name_out A string with the data name that will be used for the output `tif`file
#' @param output_path An optional output path for the created file.
#' @param make_productive Logical. Also make the Productive Managed Forests layer.
#' @param raster_npp A `SpatRaster` file containing the NPP information needed to calculate the Productive Managed Forests layer. Needed if make_productive = TRUE
#'
#' @importFrom terra %in%
#'
#' @return A `SpatRaster` file of managed forests that has been aligned and normalised
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd",
#'   do_project = TRUE
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#'
#' managed_forests <- make_managed_forests(
#'   raster_in = forests_in,
#'   pus = pus
#' )
#' }
make_managed_forests <- function(raster_in, # ADD: option to generate productive managed forests when NPP layer supplied (dont have that currently)
                                 pus,
                                 iso3,
                                 manual_cats = NULL,
                                 include_disturbed_forest = FALSE,
                                 make_productive = FALSE,
                                 raster_npp = NULL,
                                 name_out,
                                 output_path = NULL) {

  if (!is.null(raster_in) & !is.null(npp_in)) {

    assertthat::assert_that(
      inherits(raster_in, "SpatRaster"),
      inherits(raster_npp, "SpatRaster")
    )
  }

  if (make_productive) {
    assertthat::assert_that(
      inherits(raster_npp, "SpatRaster"),
      msg = "NPP data needs to be provided as a SpatRaster when make_productive option is selected."
    )
  }
  # reprojecting the global data would take too long
  # to speed up: reproject PUs to projection of global data first
  pus_reproject <- terra::project(pus, terra::crs(raster_in))

  # then crop (make data a lot smaller)
  dat_aligned <- terra::crop(raster_in, pus_reproject)

  # check if data is categorical
  if (!(terra::is.factor(dat_aligned))) {
    dat_aligned <- terra::as.factor(dat_aligned)
  }

  if (!is.null(manual_cats)) {
    dat_aligned <- terra::ifel(
      test = dat_aligned %in% manual_cats,
      yes = 1,
      no = 0
    )
  } else { # predefined categories: 11: undisturbed forest (NOT managed), 20: disturbed forest (can be considered managed), rest: managed (palm oil, etc.)
    if (include_disturbed_forest) {
      dat_aligned <- terra::ifel(
        test = dat_aligned %in% c(20, 31, 32, 40, 53),
        yes = 1,
        no = 0
      )
    } else {
      dat_aligned <- terra::ifel(
        test = dat_aligned %in% c(31, 32, 40, 53),
        yes = 1,
        no = 0
      )
    }
  }

  #dat_aligned[is.na(dat_aligned)] <- 0

  dat_aligned <- dat_aligned %>%
    terra::project(., terra::crs(pus)) %>% # reproject the data to the crs we actually want (the original pu crs)
    exactextractr::exact_resample(., pus, "mean") %>%
    terra::mask(pus, maskvalues = 0) %>% # maskvalues denotes the background value in the raster that's not data (since this should always be planning region/units is 1 and outside is 0, this is hard-coded to 0)
    rescale_raster()

  #dat_aligned[is.na(dat_aligned)] <- 0

  if (!is.null(output_path)) {
    terra::writeRaster(dat_aligned,
      glue::glue("{output_path}/managed_forests_{iso3}.tif"),
      gdal = c("COMPRESS=DEFLATE"),
      NAflag = -9999,
      overwrite = TRUE,
      filetype = "COG"
    )
  }

  if (make_productive) {
    npp_norm <- elsar::make_normalised_raster( # doesn't need to be saved because would have been included separately
      raster_in = raster_npp,
      pus = pus,
      iso3 = iso3
      # output_path = output_path,
      # name_out = dat_default[[i]]
    )

    prod_man_forest <- dat_aligned*npp_norm
    prod_man_forest <- rescale_raster( prod_man_forest)

    if (!is.null(output_path)) {
      terra::writeRaster(npp_norm,
                         glue::glue("{output_path}/productive_managed_forests_{iso3}.tif"),
                         gdal = c("COMPRESS=DEFLATE"),
                         NAflag = -9999,
                         overwrite = TRUE,
                         filetype = "COG"
      )}

    all_managed_forests <- c(dat_aligned, prod_man_forest)
    return(all_managed_forests)

  } else {
    return(dat_aligned)
  }
}

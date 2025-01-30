#' Function to create protection zone for ELSA analysis
#'
#' The function creates a `SpatRaster` used to filter the planning units
#' available for the "protect" action in the ELSA framework based on HFP,
#' agricultural areas and buildings. It can also be used to create a layer used
#' as a locked-out constraint for the same zone.
#'
#' @param current_pas Either NULL or a vector file with the current protected areas. If NULL, PA `sf` object will be created with the `make_protected_areas()` function.
#' @param hfp_in A `SpatRaster` of the human footprint index. Will be cropped and re-projected with the PUs.
#' @param crop_in A `SpatRaster` of the crop data. Will be cropped (CURRENTLY: rescaled) and re-projected with the PUs.
#' @param built_in A `SpatRaster` of building data. Will be cropped (CURRENTLY: rescaled) and re-projected with the PUs.
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units.
#' @param iso3 A string of the iso3 name of the data (country name).
#' @param hfp_threshold If <1, HFP threshold is calculated based on a quantile approach (HFP inside PAs excluding x% highest; default is 0.95). If >1, that value will be used as the threshold.
#' @param filter_patch_size Logical. Whether to filter out clumps smaller than a given threshold.
#' @param min_patch_size Positive integer. Clumps smaller than this are removed.
#' @param make_locked_out Logical. Whether to change data from available planning units for a zone to a layer for a locked-out constraint in `prioritizr`. Will swap 0s and 1s.
#' @param output_path  An optional output path for the created file.
#'
#' @return A `SpatRaster` file that either serves as available planning units for the protection zone in the ELSA analysis or locked-out areas for the same zone.
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
#' path_in <- "YourDataPath"
#'
#' list_dat <- extract_filename_filetype(
#'   data_name = "hfp",
#'   file_path = path_in
#' )
#'
#' load_hfp <- elsar_load_data(
#'   file_name = paste0(list_dat["filename"][[1]], list_dat["filetype"][[1]]),
#'   file_type = list_dat["filetype"][[1]], file_path = path_in
#' )
#'
#' list_dat <- extract_filename_filetype(
#'   data_name = "crop_suit",
#'   file_path = path_in
#' )
#'
#' load_crop <- elsar_load_data(
#'   file_name = paste0(list_dat["filename"][[1]], list_dat["filetype"][[1]]),
#'   file_type = list_dat["filetype"][[1]], file_path = path_in
#' )
#'
#' list_dat <- extract_filename_filetype(
#'   data_name = "build",
#'   file_path = path_in
#' )
#'
#' load_built <- elsar_load_data(
#'   file_name = paste0(list_dat["filename"][[1]], list_dat["filetype"][[1]]),
#'   file_type = list_dat["filetype"][[1]], file_path = path_in
#' )
#'
#' protection_zone1 <- make_protection_zone(
#'   hfp_in = load_hfp,
#'   crop_in = load_crop,
#'   built_in = load_built,
#'   pus = pus,
#'   iso3 = "NPL",
#'   make_locked_out = FALSE
#' )
#'
#' protection_zone2 <- make_protection_zone(
#'   hfp_in = load_hfp,
#'   crop_in = load_crop,
#'   built_in = load_built,
#'   filter_patch_size = FALSE,
#'   pus = pus,
#'   iso3 = "NPL",
#'   make_locked_out = FALSE
#' )
#'
#' protection_zone3 <- make_protection_zone(
#'   hfp_in = load_hfp,
#'   built_in = load_built,
#'   hfp_threshold = 13,
#'   pus = pus,
#'   iso3 = "NPL",
#'   make_locked_out = TRUE
#' )
#' }
make_protection_zone <- function(current_pas = NULL,
                                 hfp_in,
                                 crop_in = NULL,
                                 built_in = NULL,
                                 pus,
                                 iso3,
                                 hfp_threshold = 0.95,
                                 filter_patch_size = TRUE,
                                 min_patch_size = 20,
                                 make_locked_out = FALSE,
                                 output_path = NULL) {
  if (hfp_threshold <= 1) {  # If HFP already provided as absolute number, don't need PA info
    if (is.null(current_pas)) {
      current_pas <- make_protected_areas(
        iso3 = iso3,
        download_path = here::here(),
        buffer_points = TRUE,
        pus = pus,
        return_sf = TRUE
      )
    } else {
      assertthat::assert_that(
        (inherits(current_pas, "sf") | inherits(current_pas, "SpatVector")),
        msg = "The data provided for current protected areas needs to be in
        vector format."
      )
    }
  }

  # prepocess (match resolution and crs, crop) other data
  suppressWarnings(hfp <- make_normalised_raster(
    raster_in = hfp_in,
    pus = pus,
    iso3 = iso3,
    rescaled = FALSE
  ))

  if (!is.null(crop_in)) {
    assertthat::assert_that(
      inherits(crop_in, "SpatRaster"),
      msg = "'Crops' data needs to be a SpatRaster."
    )

    crops <- make_normalised_raster(
      raster_in = crop_in,
      pus = pus,
      iso3 = iso3
    )
  }

  if (!is.null(built_in)) {
    assertthat::assert_that(
      inherits(built_in, "SpatRaster"),
      msg = "'Built' data needs to be a SpatRaster."
    )

    built <- make_normalised_raster(
      raster_in = built_in,
      pus = pus,
      iso3 = iso3
    )
  }

  # HFP inside PAs excluding x% highest
  if (hfp_threshold <= 1) {
    breaks <- terra::extract(hfp, terra::vect(current_pas),
      fun = terra::quantile, probs = hfp_threshold,
      na.rm = TRUE
    )[, 2]

    message(glue::glue("HFP threshold calculated based on quantile ({hfp_threshold}).
                       Threshold used is {breaks}."))
  } else if (hfp_threshold > 1) {
    breaks <- hfp_threshold

    message(glue::glue("HFP threshold used is {breaks}. If you would like to calculate
                       the threshold based on a quantile provide an input <1."))
  } else {
    message("Invalid hfp_threshold value supplied.
            Value must be >0. If <1, HFP threshold is calculated based on
            a quantile approach, if >1, that value will be used as the threshold.")
  }

  # get zone info based on where hfp is smaller than breaks (threshold) and re-project
  zone <- terra::ifel(hfp < breaks, 1, 0) %>%
    terra::project(x = ., y = pus, method = "near") %>%
    terra::mask(pus, maskvalues = 0)

  # filter data for agricultural areas and where buildings are
  if (!is.null(built_in) & is.null(crop_in)) {
    zone[built > 0] <- NA
  } else if (is.null(built_in) & !is.null(crop_in)) {
    zone[crops > 0] <- NA
  } else if (!is.null(built_in) & !is.null(crop_in)) {
    zone[crops > 0 | built > 0] <- NA
  } else if (is.null(built_in) & is.null(crop_in)) {
    message("No building or agricultural data provided.
                            Layer will be built only based on HFP.")
  }

  # Buffer out by 2 cells
  # dist <- terra::distance(zone)
  #
  # zone[dist <= terra::res(zone)[1] * 2 & dist > 0] <- 1 # ? ask Scott about this --> produces odd artefacts around borders

  zone <- terra::subst(zone, NA, 0) %>%
    terra::mask(pus, maskvalues = 0)

  # filter our patches of small sizes
  if (filter_patch_size) {
    zone <- terra::sieve(zone, threshold = min_patch_size)
  }

  # switch ones and zeroes if rather than creating a zone, a locked out area is created
  if (make_locked_out) {
    zone <- 1 - zone
  }

  zone <- zone %>%
    terra::mask(pus, maskvalues = 0)

  if (!is.null(output_path)) {
    terra::writeRaster(zone,
      glue::glue("{output_path}/protection_zone_{iso3}.tif"),
      gdal = c("COMPRESS=DEFLATE", "OVERVIEWS=NONE"),
      NAflag = -9999,
      overwrite = TRUE,
      # datatype = "INT1U",
      filetype = "COG"
    )
  }

  return(zone)
}

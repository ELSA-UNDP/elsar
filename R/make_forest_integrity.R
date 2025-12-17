#' Create the forest integrity index using flii and fsii data
#'
#' @param raster_flii A `SpatRaster` file with the flii information.
#' @param raster_fsii A `SpatRaster` file with the fsii information.
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param output_path An optional output path for the created file.
#'
#' @return A `SpatRaster` file of the forest integrity index that has been aligned and normalised
#' @export
#'
#' @examples
#' \dontrun{
#' forest_integrity1 <- make_forest_integrity(
#'   raster_flii = raster_flii,
#'   pus = pus
#' )
#'
#' forest_integrity2 <- make_forest_integrity(
#'   raster_fsii = raster_fsii,
#'   pus = pus
#' )
#'
#' forest_integrity3 <- make_forest_integrity(
#'   raster_flii = raster_flii,
#'   raster_fsii = raster_fsii,
#'   pus = pus
#' )
#'
#' zero_fsii <- raster_fsii
#' zero_fsii[zero_fsii < 14] = 0
#'
#' forest_integrity4 <- make_forest_integrity(
#'   raster_flii = raster_flii,
#'   raster_fsii = raster_fsii,
#'   pus = pus
#' )
#'
#' }
make_forest_integrity <- function(raster_flii = NULL,
                                  raster_fsii = NULL,
                                  pus,
                                  iso3,
                                  output_path = NULL) {

  if (!is.null(raster_flii) & !is.null(raster_fsii)) {

    assertthat::assert_that(
      inherits(raster_flii, "SpatRaster"),
      inherits(raster_fsii, "SpatRaster")
    )

  # crop and align to pus
  suppressWarnings(flii <- make_normalised_raster(
    raster_in = raster_flii,
    pus = pus,
    rescaled = FALSE
  ))

  suppressWarnings(fsii <- make_normalised_raster(
    raster_in = raster_fsii,
    pus = pus,
    rescaled = FALSE
  ))

  if ((terra::global(fsii, sum, na.rm=TRUE)[[1]]) == 0) {
    log_message("FSII only contains 0s for the study area. Forest integrity will be calculated using FLII only.")

    raster_rescaled <- rescale_raster(flii)

  } else if ((terra::global(flii, sum, na.rm=TRUE)[[1]]) == 0) {

    log_message("FLII only contains 0s for the study area. Forest integrity will be calculated using FSII only.")

    raster_rescaled <- rescale_raster(fsii)

  } else if (((terra::global(fsii, sum, na.rm=TRUE)[[1]]) > 0) & ((terra::global(fsii, sum, na.rm=TRUE)[[1]]) > 0)) {
    log_message("Forest integrity will be calculated using FLII and FSII.")

    # get mean of fsii and flii and rescale
    fi <- terra::mean(fsii + flii)
    raster_rescaled <- rescale_raster(fi)

  } else {
    log_message("Both of your input data files only contain 0 values. Please check your input data.")
  }

  } else if (is.null(raster_flii) & !is.null(raster_fsii)) {

    log_message("Only FSII data provided. Forest integrity will be calculated using FSII only.")

    raster_rescaled <- make_normalised_raster(
      raster_in = raster_fsii,
      pus = pus
    )

  } else if (!is.null(raster_flii) & is.null(raster_fsii)) {
    log_message("Only FLII data provided. Forest integrity will be calculated using FLII only.")

    raster_rescaled <- make_normalised_raster(
      raster_in = raster_flii,
      pus = pus
    )

  }

  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = raster_rescaled,
      filename = glue::glue("{output_path}/forest_integrity_{iso3}.tif"),
      datatype = "FLT4S"
    )
  }

  return(raster_rescaled)
}

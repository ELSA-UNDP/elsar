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

  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }
  assertthat::assert_that(
    !is.null(raster_flii) || !is.null(raster_fsii),
    msg = "Provide 'raster_flii', 'raster_fsii' or both."
  )

  # Each index is rescaled to 0-1 before they are combined. FLII is published
  # on a 0-10000 scale, so before 0.4.2 adding the raw values let FLII swamp
  # FSII; an index that is all 0 in the region is left out.
  indices <- list(FLII = raster_flii, FSII = raster_fsii)
  indices <- indices[!vapply(indices, is.null, logical(1))]
  aligned <- lapply(indices, function(r) {
    assertthat::assert_that(inherits(r, "SpatRaster"))
    suppressWarnings(make_normalised_raster(raster_in = r, pus = pus, iso3 = iso3))
  })
  has_data <- vapply(aligned, function(r) {
    isTRUE(terra::global(r, "max", na.rm = TRUE)[[1]] > 0)
  }, logical(1))

  if (!any(has_data)) {
    warning("Forest integrity inputs only contain 0 values in the region; returning an all-0 layer.",
            call. = FALSE)
    raster_rescaled <- terra::ifel(pus == 1, 0, NA)
  } else {
    used <- names(aligned)[has_data]
    log_message("Forest integrity calculated from {paste(used, collapse = ' and ')}.")
    combined <- Reduce(`+`, aligned[has_data]) / length(used)
    raster_rescaled <- if (length(used) > 1) rescale_raster(combined) else combined
  }
  names(raster_rescaled) <- "forest_integrity"

  if (!is.null(output_path)) {
    save_raster(
      raster = raster_rescaled,
      filename = glue::glue("{output_path}/forest_integrity_{iso3}.tif"),
      datatype = "FLT4S"
    )
  }

  raster_rescaled
}

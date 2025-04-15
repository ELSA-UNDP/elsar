#' Create Managed Forests and Productive Managed Forests Raster Layers
#'
#' This function generates a standardised raster of managed forests, optionally including disturbed forests
#' and a separate layer for productive managed forests (based on NPP data). The input raster should contain
#' LULC classification values which are reclassified according to defined managed forest class codes.
#'
#' If `make_productive = TRUE`, the function multiplies the managed forest raster by a normalised raster of
#' Net Primary Productivity (NPP) to generate a layer representing productive managed forests.
#'
#' @param raster_in A `SpatRaster` representing LULC classifications.
#' @param pus A `SpatRaster` used as a template for extent, resolution, and alignment.
#' @param iso3 A 3-letter ISO country code string.
#' @param forest_classes Integer vector of LULC class codes representing managed forests (default = c(31, 32, 40, 53)).
#' @param include_disturbed_forest Logical. If TRUE, includes disturbed forest class (code 20) in `forest_classes`.
#' @param make_productive Logical. If TRUE, also generates a productive managed forest raster.
#' @param raster_npp A `SpatRaster` of NPP data used for generating the productive forest layer. Required if `make_productive = TRUE`.
#' @param name_out Character. Name stem for output file(s) (optional, not currently used).
#' @param output_path Optional character. Directory to save output raster(s) if provided.
#'
#' @return A `SpatRaster` with either one layer (managed forests) or two layers (managed forests and productive managed forests).
#' @export
#'
#' @examples
#' \dontrun{
#' managed_forests <- make_managed_forests(
#'   raster_in = forest_lulc,
#'   pus = planning_units,
#'   iso3 = "NPL",
#'   make_productive = TRUE,
#'   raster_npp = npp_raster
#' )
#' }
make_managed_forests <- function(
    raster_in,
    pus,
    iso3,
    forest_classes = c(31, 32, 40, 53),
    include_disturbed_forest = FALSE,
    make_productive = FALSE,
    raster_npp = NULL,
    name_out,
    output_path = NULL) {

  if (!is.null(raster_in) & !is.null(raster_npp)) {
    assertthat::assert_that(
      inherits(raster_in, "SpatRaster"),
      inherits(raster_npp, "SpatRaster")
    )
  }

  if (make_productive) {
    assertthat::assert_that(
      inherits(raster_npp, "SpatRaster"),
      msg = "NPP data must be provided as a SpatRaster when make_productive is TRUE."
    )
  }

  # Include disturbed forest class if selected
  if (include_disturbed_forest) {
    forest_classes <- c(20, forest_classes)
  }

  # Create managed forests raster
  dat_aligned <- make_normalised_raster(
    raster_in = raster_in,
    pus = pus,
    iso3 = iso3,
    method_override = "mean",
    input_raster_conditional_expression = function(x) {
      terra::classify(
        x,
        rcl = matrix(cbind(forest_classes, 1), ncol = 2),
        others = 0
        )
      }
    )

  if (!is.null(output_path)) {
    terra::writeRaster(
      dat_aligned,
      filename = glue::glue("{output_path}/managed_forests_{iso3}.tif"),
      datatype = "FLT4S",
      filetype = "COG",
      gdal = c(
        "COMPRESS=ZSTD",
        "PREDICTOR=3",
        "NUM_THREADS=ALL_CPUS",
        "OVERVIEWS=NONE"
      ),
      overwrite = TRUE
    )
  }

  if (make_productive) {
    # Normalise NPP raster
    npp_norm <- make_normalised_raster(
      raster_in = raster_npp,
      pus = pus,
      iso3 = iso3,
      method_override = 'mean'
    )

    # Generate productive managed forests by multiplying with NPP
    prod_man_forest <- (dat_aligned * npp_norm) %>%
      elsar::rescale_raster()

    if (!is.null(output_path)) {
      terra::writeRaster(
        prod_man_forest,
        filename = glue::glue("{output_path}/productive_managed_forests_{iso3}.tif"),
        datatype = "FLT4S",
        filetype = "COG",
        gdal = c(
          "COMPRESS=ZSTD",
          "PREDICTOR=3",
          "NUM_THREADS=ALL_CPUS",
          "OVERVIEWS=NONE"
        ),
        overwrite = TRUE
      )
    }

    return(c(dat_aligned, prod_man_forest))
  } else {
    return(dat_aligned)
  }
}

#' Create Planning Units Raster
#'
#' This function creates a planning units (PUs) raster layer for spatial prioritization using a provided boundary.
#' If `pu_size` is not provided, it will dynamically determine a PU resolution that ensures the number of planning units
#' is below a threshold (`pu_threshold`, default 850,000), using proportional scaling and clean rounding (e.g., 100, 125, 150 m).
#'
#' @param boundary_proj A `sf` object representing the projected boundary of the planning region.
#'   Preferably generated with [make_boundary()], and must be in a projected CRS with units in meters.
#' @param pu_size Optional numeric. Planning unit edge length in meters. If `NULL`, a size is estimated to keep the number
#'   of planning units below the threshold.
#' @param pu_threshold Maximum number of allowed planning units. Default is `8.5e5` (850,000), balancing resolution and performance.
#' @param limit_to_mainland Logical (default `FALSE`). Reserved for future use; will limit planning units to mainland areas only.
#' @param iso3 ISO3 country code used in naming the output raster file.
#' @param output_path Optional output path to write the resulting Cloud Optimized GeoTIFF (`.tif`). If `NULL`, no file is saved.
#'
#' @return A `terra` raster object with a single layer ("Planning Units") where each non-zero cell represents a planning unit.
#'
#' @export
#'
#' @examples
#' # Automatically calculate PU size to keep below 850,000 PUs
#' boundary_proj <- make_boundary(boundary_in = boundary_data, iso3 = "NPL", iso3_column = "iso3cd")
#' pu_raster <- make_planning_units(boundary_proj, iso3 = "NPL")
#'
#' # Force a specific PU size (e.g., 250m resolution)
#' pu_raster <- make_planning_units(boundary_proj, pu_size = 250, iso3 = "NPL")
make_planning_units <- function(boundary_proj,
                                pu_size = NULL,
                                pu_threshold = 8.5e5,
                                limit_to_mainland = FALSE,
                                iso3,
                                output_path = NULL) {

  # Allow up to 5% more PUs than the target threshold
  tolerance <- 0.05
  threshold_soft <- pu_threshold * (1 + tolerance)

  # Rounds PU size to nearest clean value depending on scale
  round_clean <- function(x) {
    if (x < 200) {
      plyr::round_any(x, 25, ceiling)
    } else if (x < 500) {
      plyr::round_any(x, 50, ceiling)
    } else {
      plyr::round_any(x, 100, ceiling)
    }
  }

  # Case 1: PU size is not provided - estimate it
  if (is.null(pu_size)) {
    # Estimate area and initial PU edge length
    area_km2 <- units::drop_units(sf::st_area(boundary_proj)) / 1e6
    initial_size <- sqrt((area_km2 / pu_threshold) * 1e6)  # meters

    pu_size <- round_clean(initial_size)

    iter <- 0
    max_iter <- 20

    repeat {
      iter <- iter + 1
      if (iter > max_iter) {
        stop("Exceeded maximum number of PU size adjustment iterations")
      }

      # Create raster template with current PU size
      rasterMask <- terra::rast(
        resolution = pu_size,
        crs = terra::crs(boundary_proj),
        extent = terra::ext(boundary_proj)
      )

      # Rasterize boundary to create PU mask
      r1 <- terra::rasterize(
        terra::vect(boundary_proj),
        rasterMask,
        touches = TRUE,
        update = TRUE,
        background = 0
      )

      # Count number of PUs
      pu_sum <- terra::global(r1, sum, na.rm = TRUE)[[1]]
      log_msg(glue::glue("With a resolution of {pu_size} m, the current number of planning units is: {as.integer(pu_sum)}"))

      # Break if under soft threshold
      if (pu_sum <= threshold_soft) break

      # Adjust PU size proportionally and round to clean step
      overage_ratio <- pu_sum / pu_threshold
      pu_size <- round_clean(pu_size * sqrt(overage_ratio))
    }

  } else {
    # Case 2: PU size provided - use directly
    pu_size <- round_clean(pu_size)

    rasterMask <- terra::rast(
      resolution = pu_size,
      crs = terra::crs(boundary_proj),
      extent = terra::ext(boundary_proj)
    )

    r1 <- terra::rasterize(
      terra::vect(boundary_proj),
      rasterMask,
      touches = TRUE,
      update = TRUE,
      background = 0
    )

    pu_sum <- terra::global(r1, sum, na.rm = TRUE)[[1]]

    if (pu_sum > pu_threshold) {
      log_msg(glue::glue("The planning unit size you provided ({pu_size} m) produces {pu_sum} PUs, which is above the recommended threshold of {pu_threshold}. Consider increasing the PU size or letting it be calculated automatically."))
    }
  }

  names(r1) <- "Planning Units"

  log_msg(glue::glue("Created a planning unit layer with {as.integer(pu_sum)} PUs and a resolution of {as.integer(pu_size)} m."))

  # Optionally write to disk
  if (!is.null(output_path)) {
    terra::writeRaster(
      r1,
      filename = glue::glue("{output_path}/planning_units_{iso3}.tif"),
      filetype = "COG",
      datatype = "INT1U",
      gdal = c(
        "COMPRESS=ZSTD",
        "NUM_THREADS=4",
        "OVERVIEWS=NONE",
        "PREDICTOR=1"
      ),
      NAflag = 255,
      overwrite = TRUE
    )
  }

  return(r1)
}

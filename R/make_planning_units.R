#' Create a Planning Units Raster
#'
#' Generates a planning units (PUs) raster for spatial prioritization using a user-defined boundary.
#' If `pu_size` is not provided, the function automatically estimates an appropriate resolution (in meters)
#' that keeps the number of planning units under the specified threshold (`pu_threshold`), while allowing a configurable
#' tolerance (default 5%). The finest resolution meeting this constraint is selected using adaptive increments
#' and clean rounding steps. The result can optionally be saved as a Cloud Optimized GeoTIFF (.tif).
#'
#' @param boundary_proj A projected `sf` object representing the boundary of the planning region.
#'   Must be in a projected CRS with linear units (typically meters). Use [make_boundary()] to generate this.
#' @param pu_size Optional numeric. Length in meters of each square planning unit. If `NULL`, the function
#'   estimates a size that keeps the total number of PUs under `pu_threshold`.
#' @param pu_threshold Numeric. Maximum number of planning units allowed (default is 850,000).
#'   A soft threshold of `pu_threshold * (1 + pu_tolerance)` is used during optimization.
#' @param pu_tolerance Numeric. Fractional tolerance (default is `0.05`, or 5%) above `pu_threshold` allowed
#'   when estimating PU size automatically.
#' @param limit_to_mainland Logical. Reserved for future use. If `TRUE`, limits planning units to mainland regions only.
#' @param iso3 Character. ISO3 country code used to name the output raster (e.g., "KEN", "BRA").
#' @param background_fill The value to apply to all pixels outside the boundary_proj. This default to NA (e.g., nodata).
#'   You should have a good reason for wanting to use a different value.
#' @param output_path Optional character. Directory path to save the resulting raster. If `NULL`, the raster is not saved.
#'
#' @return A single-layer `SpatRaster` object from the `terra` package. All non-zero cells represent valid planning units.
#'
#' @export
#'
#' @examples
#' # Automatically estimate PU size to stay under 850,000 PUs
#' boundary_proj <- make_boundary(boundary_in = boundary_data, iso3 = "ZMB", iso3_column = "iso3cd")
#' pu_raster <- make_planning_units(boundary_proj, iso3 = "ZMB")
#'
#' # Use a fixed PU size (e.g., 250 meters)
#' pu_raster <- make_planning_units(boundary_proj, pu_size = 250, iso3 = "ZMB")
make_planning_units <- function(boundary_proj,
                                pu_size = NULL,
                                pu_threshold = 8.5e5,
                                pu_tolerance = 0.05,
                                limit_to_mainland = FALSE,
                                iso3,
                                background_fill = NA,
                                output_path = NULL) {

  threshold_soft <- pu_threshold * (1 + pu_tolerance)

  # Clean rounding of PU sizes
  round_clean <- function(x) {
    if (x < 50 ) {
      plyr::round_any(x, 5, ceiling)
    } else if (x < 250) {
      plyr::round_any(x, 25, ceiling)
    } else if (x < 800) {
      plyr::round_any(x, 50, ceiling)
    } else {
      plyr::round_any(x, 100, ceiling)
    }
  }

  # Adaptive increment based on PU size
  get_increment <- function(size) {
    if (size < 50) return(10)
    if (size < 500) return(25)
    if (size < 800) return(50)
    return(100)
  }

  if (is.null(pu_size)) {
    log_message(glue::glue("pu_size not provided: estimating size to target <= {pu_threshold} PUs (allowing {round(pu_tolerance * 100, 0)}% tolerance)."))

    # Estimate starting size from area
    area_km2 <- units::drop_units(sf::st_area(boundary_proj)) / 1e6
    initial_size <- sqrt((area_km2 / pu_threshold) * 1e6)
    pu_size <- round_clean(initial_size)

    iter <- 0
    max_iter <- 30
    min_pu_size <- 10

    best_r1 <- NULL
    best_pu_sum <- Inf
    best_pu_size <- NA

    repeat {
      iter <- iter + 1
      if (iter > max_iter || pu_size < min_pu_size) {
        if (!is.null(best_r1)) {
          r1 <- best_r1
          pu_sum <- best_pu_sum
          pu_size <- best_pu_size
          log_message("Max iterations reached or PU size too small. Using best result under soft threshold.")
          break
        } else {
          stop("Unable to find PU size under soft threshold within iteration or size limits.")
        }
      }

      # Create raster template and rasterize
      rasterMask <- terra::rast(
        resolution = pu_size,
        crs = terra::crs(boundary_proj),
        extent = terra::ext(boundary_proj)
      )

      r_temp <- terra::rasterize(
        terra::vect(boundary_proj),
        rasterMask,
        touches = TRUE,
        update = TRUE,
        background = background_fill
      )

      pu_sum_temp <- terra::global(r_temp, sum, na.rm = TRUE)[[1]]
      log_message(glue::glue("Iteration {iter}: {as.integer(pu_sum_temp)} PUs at resolution {pu_size} m"))

      if (pu_sum_temp <= threshold_soft) {
        best_r1 <- r_temp
        best_pu_sum <- pu_sum_temp
        best_pu_size <- pu_size

        # Try smaller PU size
        pu_size <- pu_size - get_increment(pu_size)
      } else {
        # Too many PUs; fallback to best below soft threshold
        if (!is.null(best_r1)) {
          r1 <- best_r1
          pu_sum <- best_pu_sum
          pu_size <- best_pu_size
          log_message(glue::glue("Exceeded soft threshold ({threshold_soft}); using best previous result."))
          break
        } else {
          stop("No valid PU size found under soft threshold.")
        }
      }
    }
  } else {
    # Use user-defined PU size directly
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
      background = background_fill
    )

    pu_sum <- terra::global(r1, sum, na.rm = TRUE)[[1]]

    if (pu_sum > pu_threshold) {
      log_message(glue::glue("The provided PU size ({pu_size} m) results in {pu_sum} PUs, exceeding the threshold ({pu_threshold}). Consider using automatic estimation."))
    }
  }

  names(r1) <- "Planning Units"

  log_message(glue::glue("Final PU layer: {as.integer(pu_sum)} PUs at {as.integer(pu_size)} m resolution."))

  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = r1,
      filename = glue::glue("{output_path}/planning_units_{iso3}.tif"),
      datatype = "INT1U"
    )
  }

  return(r1)
}

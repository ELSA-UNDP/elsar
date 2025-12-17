# Function to create degraded areas layer for a specified country
#' Create degraded areas layer for restoration planning
#'
#' This function generates a degraded areas layer based on multiple inputs, including
#' agriculture, built-up areas, human influence index (HII), and productivity decline layers.
#' The output is a raster identifying degraded areas according to set thresholds.
#'
#' @param country_iso ISO3 country code (e.g., "CHL" for Chile).
#' @param pu Planning units raster (SpatRaster).
#' @param sdg_degradation_input Productivity degradation layer (SpatRaster).
#' @param hii_input Human Influence Index (HII) raster (SpatRaster).
#' @param agriculture_input Agriculture layer (SpatRaster).
#' @param built_areas_input Built-up areas layer (SpatRaster).
#' @param output_path Directory to save output rasters. If NULL, output is not saved.
#' @param hii_threshold Threshold for the Human Influence Index (default: 4).
#' @param lulc_threshold Threshold for built and agriculture layers (default: 0.1).
#'
#' @return A SpatRaster object containing the degraded areas layer.
#' @export
#'
#' @examples
#' restore_zone <- make_degraded_areas(
#'   "CHL",
#'   pu,
#'   sdg_degradation_input,
#'   hii_input,
#'   agriculture_input,
#'   built_areas_input,
#'   output_path = "./output"
#'   )
make_degraded_areas <- function(country_iso,
                                pu,
                                sdg_degradation_input = NULL,
                                hii_input = NULL,
                                agriculture_input = NULL,
                                built_areas_input = NULL,
                                output_path = NULL,
                                hii_threshold = 4,
                                lulc_threshold = 0.1) {

  # Ensure necessary inputs are provided
  if (is.null(sdg_degradation_input) || is.null(hii_input) || is.null(agriculture_input) || is.null(built_areas_input)) {
    stop("All required input rasters (sdg_degradation_input, hii_input, agriculture_input, built_areas_input) must be provided.")
  }

  # Resample and align SDG degradation layer to the planning units (PU) raster
  sdg_degradation_resampled <- terra::resample(
    sdg_degradation_input,
    terra::project(pu, y = terra::crs(sdg_degradation_input), method = "near"),
    method = "near"
  ) %>%
    terra::project(y = pu, method = "near") %>%
    terra::resample(y = pu, method = "near") * terra::subst(pu, 0, NA)

  # Identify degraded areas based on SDG degradation: -1 means degraded, 0 otherwise
  sdg_degraded <- terra::ifel(sdg_degradation_resampled == -1, 1, 0)

  # Resample and align agriculture layer
  agriculture_resampled <- terra::resample(
    agriculture_input,
    terra::project(pu, y = terra::crs(agriculture_input), method = "near"),
    method = "bilinear"
  ) %>%
    terra::project(y = pu, method = "near") %>%
    terra::resample(y = pu, method = "bilinear") * terra::subst(pu, 0, NA)

  # Save the agriculture layer if output_path is provided
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = agriculture_resampled,
      filename = glue::glue("{output_path}/esa_agriculture_resample_{country_iso}.tif"),
      datatype = "INT1U"
    )
  }

  # Resample and align built-up areas layer
  built_areas_resampled <- terra::resample(
    built_areas_input,
    terra::project(pu, y = terra::crs(built_areas_input), method = "near"),
    method = "bilinear"
  ) %>%
    terra::project(y = pu, method = "near") %>%
    terra::resample(y = pu, method = "bilinear") * terra::subst(pu, 0, NA)

  # Save the built-up areas layer if output_path is provided
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = built_areas_resampled,
      filename = glue::glue("{output_path}/built_areas_{country_iso}.tif"),
      datatype = "INT1U"
    )
  }

  # Resample and align Human Influence Index (HII) layer
  hii_resampled <- terra::resample(
    hii_input,
    terra::project(pu, y = terra::crs(hii_input), method = "near"),
    method = "near"
  ) %>%
    terra::project(y = pu, method = "near") %>%
    terra::resample(y = pu, method = "near") * terra::subst(pu, 0, NA)

  # Save the HII layer if output_path is provided
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = hii_resampled,
      filename = glue::glue("{output_path}/hii_{country_iso}.tif"),
      datatype = "FLT4S"
    )
  }

  # Combine all layers to create the restoration zone layer
  restore_zone <- terra::ifel(
    agriculture_resampled > lulc_threshold |  # Areas converted to agriculture
      built_areas_resampled > lulc_threshold |  # Areas converted to built-up
      sdg_degraded == 1 |  # Productivity-degraded areas
      hii_resampled >= hii_threshold,  # Areas under high human pressure
    1,  # Degraded areas
    0   # Non-degraded areas
  ) * terra::subst(pu, 0, NA)  # Mask out non-planning unit areas

  # Save the restoration zone layer if output_path is provided
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = restore_zone,
      filename = glue::glue("{output_path}/restore_zone_{country_iso}.tif"),
      datatype = "INT1U"
    )
  }

  log_message(glue::glue("Degraded areas/Restore Zone layer created for {country_iso}"))

  return(restore_zone)  # Return the final restoration zone raster
}

#' Create Urban Greening Opportunities Raster
#'
#' This function generates an urban greening opportunities raster by integrating NDVI (Normalized
#' Difference Vegetation Index), land use/land cover (LULC) classification or a pre-computed built areas raster,
#' and urban heat intensity from SDEI/GHS data. It identifies urban pixels with high heat exposure
#' and low greenness for targeted greening interventions.
#'
#' The built areas layer can be sourced either from a precomputed binary raster (`built_areas_raster`)
#' or derived dynamically from a LULC raster (`lulc_raster`), which is classified on-the-fly to extract urban pixels.
#' When aggregating built areas to planning units, fractional values are thresholded to generate a binary raster
#' representing urban (1) and non-urban (0) planning units.
#'
#' @param ndvi_raster A `SpatRaster` representing NDVI values.
#' @param lulc_raster Optional. A `SpatRaster` representing land use/land cover (LULC) classes.
#'                   Required if `built_areas_raster` is not provided. Urban classes should be coded as `7`.
#' @param built_areas_raster Optional. A `SpatRaster` representing pre-classified binary built areas
#'                           (1 = built, 0 = non-built). Skips internal classification from `lulc_raster`.
#'                           Required if `lulc_raster` is not provided.
#' @param sdei_statistics An `sf` object with urban heat exposure data (e.g., WBGT statistics).
#' @param threshold Numeric. The fractional threshold (0–1) to classify planning units as urban after aggregation.
#'                  Planning units with built area fraction above this threshold are considered urban. Default is `0.10`.
#' @param pus A `SpatRaster` defining the planning units.
#' @param iso3 Character. ISO3 country code used to subset `sdei_statistics`.
#' @param return_urban_areas Logical. If `TRUE`, returns a two-layer stack with both greening opportunities
#'                           and binary urban extent.
#' @param output_path Optional. Directory path to save the resulting raster(s).
#' @param cores Integer. Number of CPU cores to use (for future expansion, currently unused).
#'
#' @return A normalized `SpatRaster` representing urban greening opportunities.
#'         If `return_urban_areas = TRUE`, returns a raster stack with both the greening opportunities raster
#'         and the binary built areas raster.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- make_urban_greening_opportunities(
#'   ndvi_raster = ndvi,
#'   built_areas_raster = built_binary,
#'   sdei_statistics = sdei,
#'   pus = planning_units,
#'   iso3 = "BRA"
#' )
#' }
make_urban_greening_opportunities <- function(
    ndvi_raster,
    lulc_raster = NULL,
    built_areas_raster = NULL,
    sdei_statistics,
    threshold = 0.10,
    pus,
    iso3,
    return_urban_areas = FALSE,
    output_path = NULL,
    cores = 4
) {
  # Validate inputs
  assertthat::assert_that(inherits(ndvi_raster, "SpatRaster"), msg = "ndvi_raster must be a SpatRaster object.")
  if (!is.null(lulc_raster)) {
    assertthat::assert_that(inherits(lulc_raster, "SpatRaster"), msg = "lulc_raster must be a SpatRaster object.")
  }
  assertthat::assert_that(inherits(sdei_statistics, "sf"), msg = "sdei_statistics must be an sf object.")
  if (!is.null(built_areas_raster)) {
    assertthat::assert_that(inherits(built_areas_raster, "SpatRaster"), msg = "built_areas_raster must be a SpatRaster object.")
  }
  if (is.null(lulc_raster) && is.null(built_areas_raster)) {
    stop("You must provide either `lulc_raster` or `built_areas_raster` to classify urban areas.")
  }

  # Normalize NDVI — invert to reflect greening need, and remove negative values (water or invalid)
  log_msg("Normalizing NDVI: removing negatives and inverting to reflect greening need...")
  rev_ndvi <- make_normalised_raster(
    raster_in = ndvi_raster,
    pus = pus,
    iso3 = iso3,
    invert = TRUE,
    conditional_expression = function(x) terra::ifel(x < 0, NA, 1 - x)
  )

  # Handle urban area detection
  if (!is.null(built_areas_raster)) {
    log_msg("Using precomputed built areas raster...")
    urban_areas <- make_normalised_raster(
      raster_in = built_areas_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean"
    )
  } else {
    log_msg("Classifying built areas from LULC raster...")
    urban_areas <- make_normalised_raster(
      raster_in = lulc_raster,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      crop_global_input = FALSE,
      input_raster_conditional_expression = function(x) terra::ifel(x == 7, 1, 0)
    )
  }

  # Threshold fractional urban areas to binary
  urban_areas <- terra::ifel(urban_areas > threshold, 1, 0)
  urban_areas <- terra::mask(urban_areas, pus)
  names(urban_areas) <- "built_areas"

  # Process urban heat exposure from SDEI
  log_msg("Rasterizing urban heat exposure from SDEI...")
  pu_proj <- terra::as.polygons(pus) %>%
    sf::st_as_sf() %>%
    dplyr::filter(`Planning Units` == 1) %>%
    sf::st_transform(sf::st_crs(sdei_statistics)) %>%
    sf::st_make_valid() %>%
    terra::vect()

  sdei_filtered <- sdei_statistics %>%
    dplyr::filter(.data$CTR_MN_ISO == iso3) %>%
    terra::vect() %>%
    terra::intersect(y = pu_proj) %>%
    sf::st_as_sf()

  if (!"ID_HDC_G0" %in% names(sdei_filtered) || !"avg_intens" %in% names(sdei_filtered)) {
    log_msg("No usable heat exposure data: generating empty raster.")
    urban_extreme_heat <- terra::ifel(pus == 1, 0, NA)
  } else {
    sdei_summary <- sdei_filtered %>%
      dplyr::group_by(.data$ID_HDC_G0) %>%
      dplyr::summarise(avg_intens = mean(.data$avg_intens, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(.data$avg_intens))

    if (nrow(sdei_summary) == 0) {
      log_msg("No non-NA values in urban heat: generating empty raster.")
      urban_extreme_heat <- terra::ifel(pus == 1, 0, NA)
    } else {
      urban_extreme_heat <- exact_rasterise(
        attribute = "avg_intens",
        features = sdei_summary,
        pus = pus,
        iso3 = iso3,
        fun = mean
      ) %>%
        elsar::rescale_raster()
    }
  }

  # Combine layers: areas with low NDVI, high heat, and urban presence
  log_msg("Combining NDVI, heat, and urban data into final greening opportunities raster...")
  urban_greening_opportunities <- ((rev_ndvi + urban_extreme_heat) / 2 * urban_areas) %>%
    elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  names(urban_greening_opportunities) <- "urban_greening_opportunities"

  # Save output if requested
  if (!is.null(output_path)) {
    output_file <- glue::glue("{output_path}/urban_greening_opportunities_{iso3}.tif")
    elsar::save_raster(
      raster = urban_greening_opportunities,
      filename = output_file,
      datatype = "FLT4S"
    )
    log_msg(glue::glue("Urban greening raster saved to: {output_file}"))
  }

  # Optionally return both layers
  if (return_urban_areas) {
    return(c(urban_greening_opportunities, urban_areas))
  } else {
    return(urban_greening_opportunities)
  }
}

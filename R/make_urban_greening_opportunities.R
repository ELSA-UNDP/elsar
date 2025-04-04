#' Create Urban Greening Opportunities Raster
#'
#' This function generates an urban greening opportunities raster by integrating NDVI (Normalized
#' Difference Vegetation Index), land use/land cover (LULC) classification, and urban heat intensity
#' from SDEI/GHS data. The process includes normalizing NDVI, identifying urban areas from LULC,
#' rasterizing urban heat exposure, and combining all layers to map priority areas for urban greening.
#'
#' @param ndvi_raster A `SpatRaster` representing NDVI data (vegetation greenness).
#' @param lulc_raster A `SpatRaster` representing land use/land cover (LULC) classes.
#' @param sdei_statistics An `sf` object of urban heat exposure statistics (e.g., WBGT).
#' @param pus A `SpatRaster` defining planning units (PUs) to align outputs to.
#' @param iso3 ISO3 country code used for filtering the urban heat data.
#' @param return_urban_areas Logical. If TRUE, also returns the raster of binary urban areas.
#' @param output_path Optional. A directory path to write the resulting raster(s).
#' @param cores Integer. Number of CPU cores to use in multi-threaded operations (if supported). Default = 4.
#'
#' @return A `SpatRaster` of normalized urban greening opportunities. If `return_urban_areas = TRUE`,
#'         a raster stack is returned with both greening opportunities and urban extent.
#' @export
#'
#' @examples
#' \dontrun{
#' ndvi <- elsar::elsar_load_data(
#'   file_path = ".",
#'   file_name = "ndvi.tif"
#'   )
#'
#' lulc <- elsar::elsar_load_data(
#'   file_path = ".",
#'   file_name = "lulc.tif",
#'   )
#'
#' sdei <- sf::read_sf("path_to_sdei_data.gpkg")
#'
#' result <- make_urban_greening_opportunities(
#'   ndvi_raster = ndvi,
#'   lulc_raster = lulc,
#'   sdei_statistics = sdei,
#'   pus = planning_units,
#'   iso3 = "NPL"
#' )
#' }
make_urban_greening_opportunities <- function(
    ndvi_raster,
    lulc_raster,
    sdei_statistics,
    pus,
    iso3,
    return_urban_areas = FALSE,
    output_path = NULL,
    cores = 4
) {
  # Validate inputs
  assertthat::assert_that(inherits(ndvi_raster, "SpatRaster"), msg = "ndvi_raster must be a SpatRaster object.")
  assertthat::assert_that(inherits(lulc_raster, "SpatRaster"), msg = "lulc_raster must be a SpatRaster object.")
  assertthat::assert_that(inherits(sdei_statistics, "sf"), msg = "sdei_statistics must be an sf object.")

  # Normalize NDVI — remove water (NDVI < 0), invert to reflect greening potential
  log_msg("Normalizing NDVI raster, removing negative values, and inverting...")
  rev_ndvi <- make_normalised_raster(
    raster_in = ndvi_raster,
    pus = pus,
    iso3 = iso3,
    invert = TRUE,
    conditional_expression = function(x) terra::ifel(x < 0, NA, 1 - x)
  )

  # Identify urban areas from LULC raster (value 7 for urban)
  log_msg("Extracting urban areas from LULC raster...")
  urban_areas <- make_normalised_raster(
    raster_in = lulc_raster,
    pus = pus,
    iso3 = iso3,
    method_override = "mean",
    input_raster_conditional_expression = function(x) terra::ifel(x == 7, 1, 0)
  )

  # Rasterize extreme heat exposure from SDEI statistics for urban areas
  log_msg("Processing SDEI urban heat exposure statistics...")

  # Get spatial extent of PUs for subsetting SDEI
  pu_proj <- terra::as.polygons(pus) %>%
    sf::st_as_sf() %>%
    dplyr::filter(`Planning Units` == 1) %>%
    sf::st_transform(sf::st_crs(sdei_statistics)) %>%
    sf::st_make_valid() %>%
    terra::vect()

  # Filter and intersect with PUs
  urban_extreme_heat <- sdei_statistics %>%
    dplyr::filter(CTR_MN_ISO == iso3) %>%
    terra::vect() %>%
    terra::intersect(y = pu_proj) %>%
    sf::st_as_sf()

  # Handle potential missing or malformed data
  if (!"ID_HDC_G0" %in% names(urban_extreme_heat)) {
    log_msg("No urban extreme heat values to rasterise: returning empty raster.")
    urban_extreme_heat <- terra::ifel(pus == 1, 0, NA)
  } else {
    urban_extreme_heat <- urban_extreme_heat %>%
      dplyr::group_by(ID_HDC_G0) %>%
      dplyr::summarise(avg_intens = mean(avg_intens, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(avg_intens))

    if (nrow(urban_extreme_heat) == 0) {
      log_msg("No urban extreme heat `avg_intense` values to rasterise: returning empty raster.")
      urban_extreme_heat <- terra::ifel(pus == 1, 0, NA)
    } else {
      urban_extreme_heat <- elsar::exact_rasterise(
        attribute = "avg_intens",
        features = urban_extreme_heat,
        pus = pus,
        iso3 = iso3,
        fun = mean
      ) %>%
        elsar::rescale_raster()
    }
  }

  # Combine layers and re-normalize
  log_msg("Combining NDVI, heat, and urban layers to generate urban greening opportunities...")
  urban_greening_opportunities <- ((rev_ndvi + urban_extreme_heat) / 2 * urban_areas) %>%
    elsar::make_normalised_raster(pus = pus, iso3 = iso3)

  # Optionally write to file
  if (!is.null(output_path)) {
    output_file <- glue::glue("{output_path}/urban_greening_opportunities_{iso3}.tif")

    terra::writeRaster(
      urban_greening_opportunities,
      filename = output_file,
      filetype = "COG",
      datatype = "FLT4S",
      gdal = c(
        "COMPRESS=ZSTD",
        "PREDICTOR=3",
        "OVERVIEWS=NONE",
        "NUM_THREADS=ALL_CPUS"
      ),
      overwrite = TRUE
    )
    log_msg(glue::glue("Urban greening raster saved to: {output_file}"))
  }

  # Optionally return the urban areas raster
  if (return_urban_areas) {
    return(c(urban_greening_opportunities, urban_areas))
  } else {
    return(urban_greening_opportunities)
  }
}

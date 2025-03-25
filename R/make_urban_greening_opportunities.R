#' Create Urban Greening Opportunities Raster
#'
#' This function generates an urban greening opportunities raster by integrating NDVI,
#' land-use classifications (LULC), and extreme heat exposure from GHS data.
#' The process involves normalizing NDVI, extracting urban areas from LULC,
#' rasterizing extreme heat exposure, and combining these layers to identify potential
#' areas for greening interventions.
#'
#' @param ndvi_raster A `SpatRaster` representing the NDVI data.
#' @param lulc_raster A `SpatRaster` of the land use/land cover (LULC) classification.
#' @param sdei_statistics A `sf` object representing GHS boundaries and SDEI extreme
#'      urban heat data.
#' @param pus A `SpatRaster` defining the planning unit (PU) grid.
#' @param iso3 A character string representing the ISO3 country code.
#' @param return_urban_areas logical. Whether to also return the urban areas raster
#'        for potential downstream analyses.
#' @param output_path A character string specifying the output directory (optional).
#' @param cores A number allocating the available cores for `terra` tools that allow
#'        multi-core processing (if available on your machine). Defaults to 4.
#'
#' @return A `SpatRaster` representing urban greening opportunities.
#' @export
#'
#' @examples
#' \dontrun{
#' ndvi_raster <- elsar::elsar_load_data(
#'   file_path = ".",
#'   file_name = "mod13q1_2022_ndvi_wgs84.tif",
#'   file_type = "tiff"
#'   )
#'
#' lulc_raster <- elsar::elsar_load_data(
#'   file_path = ".",
#'   file_name = "esri_10m_lulc_2023_NPL.tif",
#'   file_type = "tiff"
#'   )
#'
#' sdei_statistics <- sf::read_sf(
#'   "~/sdei-high-res-daily-uhe-1983-2016-shp/wbgtmax30_join.gpkg"
#' )
#'
#' urban_green_opportunities <- make_urban_greening_opportunities(
#'   ndvi_raster = ndvi,
#'   lulc_raster = lulc,
#'   sdei_statistics = sdei_statistics,
#'   pus = planning_units,
#'   iso3 = "NPL",
#' )
#'
#' terra::plot(urban_green_opportunities)
#' }
make_urban_greening_opportunities <- function(ndvi_raster,
                                              lulc_raster,
                                              sdei_statistics,
                                              pus,
                                              iso3,
                                              return_urban_areas = FALSE,
                                              output_path = NULL,
                                              cores = 4) {
  # Input Validation
  assertthat::assert_that(
    inherits(ndvi_raster, "SpatRaster"),
    msg = "ndvi_raster must be a SpatRaster object"
  )
  assertthat::assert_that(
    inherits(lulc_raster, "SpatRaster"),
    msg = "lulc_raster must be a SpatRaster object"
  )
  assertthat::assert_that(
    inherits(sdei_statistics, "sf"),
    msg = "sdei_stats_file must be a sf object"
  )

  # Normalizing NDVI
  print("Normalizing NDVI raster and and inverting values...")
  rev_ndvi <- make_normalised_raster(
    raster_in = ndvi_raster,
    pus = pus,
    iso3 = iso3,
    invert = TRUE,
    conditional_expression = function(x) {
      terra::ifel(x < 0, NA, 1 - x)
    },
  )

  # Extracting Urban Areas from LULC - Converts inputs to float type to allow for
  # output to be non-binary
  print("Extracting urban areas from LULC raster...")
  urban_areas <- make_normalised_raster(
    raster_in = lulc_raster,
    pus = pus,
    iso3 = iso3,
    method_override = "bilinear", # Use bilinear so that values are continuous 0-1.
    input_raster_conditional_expression = function(x) terra::ifel(x == 7, 1, 0)
  )

  # Loading and Subsetting Global Human Settlement (GHS) Data
  # NOTE: This works for now, per the methods used before, but it needs to be revisited ASAP
  print("Reading, spatially filtering GHS and SDEI statistics, and rasterising data...")

  # Get PU boundary for spatial subsetting of global layer
  pu_proj <- terra::as.polygons(pus) %>%
    sf::st_as_sf() %>%
    dplyr::filter(layer == 1) %>%
    sf::st_transform(sf::st_crs(sdei_statistics)) %>% # Re-project to match sdei_statistics
    sf::st_make_valid() %>%
    terra::vect()

  urban_extreme_heat <- sdei_statistics %>%
    dplyr::filter(CTR_MN_ISO == iso3) %>%
    terra::vect() %>%
    terra::intersect(y = pu_proj) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = sf::st_crs(pus)) %>% # Re-project back to PU crs
    dplyr::group_by(ID_HDC_G0) %>%
    dplyr::summarise(avg_intens = mean(avg_intens)) %>%
    dplyr::filter(!is.na(avg_intens)) %>%
    elsar::exact_rasterise(
      attribute = "avg_intens",
      pu_layer = pus,
      fun = mean
    ) %>%
    elsar::rescale_raster()

  # Combining and Normalizing Urban Greening Opportunities
  print("Combining layers and normalising urban greening opportunities layer...")
  urban_greening_opportunities <- ((rev_ndvi + urban_extreme_heat) / 2 * urban_areas) %>%
    elsar::rescale_raster()

#  names(urban_greening_opportunities) <- "urban_greening_opportunities"

  # Writing Output
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

    print(glue::glue("Urban greening opportunities raster created and saved to: {output_file}"))
  }

  if (return_urban_areas) {
    combined_urban <- c(urban_greening_opportunities, urban_areas)
    return(combined_urban)
  } else{
  return(urban_greening_opportunities)
  }
}

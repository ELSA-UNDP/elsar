#' Create Threatened Ecosystems (for Protection) Raster Based on IUCN GET and Ecological
#'  Intactness
#'
#' This function identifies ecosystems that are ecologically threatened by evaluating
#' the proportion of intact area for each IUCN GET ecosystem type. It compares the
#' intactness within each ecosystem against the national median ecological integrity
#' value, and generates a raster layer representing ecosystem-level threat based on
#' under-intactness.
#'
#' @param iso3 Character. ISO3 country code (e.g., "KEN") used for naming and processing.
#' @param pus SpatRaster. Planning units raster used for resolution and extent.
#' @param boundary_layer sf object. National boundary used for subsetting intactness.
#' @param intactness_input SpatRaster. Raster of ecological integrity values.
#' @param iucn_get_directory Character. Directory containing IUCN GET `.gpkg` files.
#' @param iucn_get_prefixes Optional character vector of prefixes (e.g., "T", "M", etc.) to include.
#' @param include_minor_occurrence Logical. Whether to include ecosystems marked as minor (default: TRUE).
#' @param output_path Character or NULL. Optional output directory to save the resulting raster.
#'
#' @return A normalized SpatRaster showing per-pixel average ecosystem threat due to low intactness.
#' @export
#'
#' @examples
#' \dontrun{
#' threatened <- make_threatened_ecosystems_protection(
#'   iso3 = "KEN",
#'   pus = planning_units,
#'   boundary_layer = sf::st_read("data/boundary.gpkg"),
#'   intactness_input = rast("data/intactness.tif"),
#'   iucn_get_directory = "data/iucn_layers",
#'   output_path = "outputs"
#' )
#' }
make_threatened_ecosystems_protection <- function(
    iso3,
    pus,
    boundary_layer = boundary_layer,
    intactness_input = NULL,
    iucn_get_directory,
    iucn_get_prefixes = NULL,
    include_minor_occurrence = TRUE,
    output_path = NULL
) {
  assertthat::assert_that(assertthat::is.string(iucn_get_directory))
  assertthat::assert_that(dir.exists(iucn_get_directory))
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(boundary_layer, "sf"))

  # Resample intactness raster to planning units
  intactness_resampled <- elsar::make_normalised_raster(
    raster_in = intactness_input,
    pus = pus,
    iso3 = iso3
  )

  # Calculate national median intactness
  intactness_median <- exactextractr::exact_extract(
    intactness_resampled,
    boundary_layer,
    fun = "median"
  )

  # Generate binary intact mask (1 = intact)
  non_intact_areas <- terra::ifel(intactness_resampled < intactness_median, 0, 1) %>%
    terra::as.polygons(na.rm = TRUE) %>%
    sf::st_as_sf() %>%
    dplyr::filter(.[[1]] == 1) %>%
    sf::st_make_valid()

  # Extract IUCN GET ecosystems from .gpkg files
  cat("Collecting IUCN GET ecosystems...\n")
  iucn_ecosystems <- elsar::get_iucn_ecosystems(
    iucn_get_directory = iucn_get_directory,
    iso3 = iso3,
    boundary_layer = boundary_layer,
    pus = pus,
    iucn_get_prefixes = iucn_get_prefixes,
    include_minor_occurrence = include_minor_occurrence,
    output_path = NULL
  )

  # Calculate intact area per ecosystem
  cat("Calculating intactness of each IUCN GET ecosystem...\n")
  iucn_ecosysytems_intactness_area <- iucn_ecosystems %>%
    sf::st_intersection(non_intact_areas) %>%
    sf::st_make_valid() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area_intact = units::drop_units(sf::st_area(.))) %>%
    sf::st_set_geometry(NULL)

  # Compute total area and intactness ratio per ecosystem
  iucn_ecosysytems_total <- iucn_ecosystems %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(iucn_ecosysytems_intactness_area, by = 'id') %>%
    dplyr::mutate(threat = .data$area_intact / .data$area * 100)

  # Rasterize and normalize the threat values
  cat("Calculating average intactness and normalising raster output...\n")
  threatened_ecosystems_for_protection <- elsar::exact_rasterise(
    features = iucn_ecosysytems_total,
    pus = pus,
    iso3 = iso3,
    attribute = "threat"
  )

  names(threatened_ecosystems_for_protection) <- "threatened_ecosystems_for_protection"

  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/threatened_ecosystems_for_protection_{iso3}.tif")
    cat(glue::glue("Writing output to: {out_file}"), "\n")

    terra::writeRaster(
      threatened_ecosystems_for_protection,
      filename = out_file,
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

  return(threatened_ecosystems_for_protection)
}

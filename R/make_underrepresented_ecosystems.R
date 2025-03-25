#' Create Underrepresented Ecosystems Raster Based on IUCN GET and Protection Gaps
#'
#' This function processes IUCN GET ecosystem vector layers and computes an underrepresentation
#' score for each based on their representation within existing protected areas relative to a 30%
#' protection target. The final output is a normalized raster showing the average degree
#' to which ecosystems are underrepresented across planning units.
#'
#' @param iucn_get_directory Character. Path to the directory containing the IUCN `.gpkg` files.
#' @param iso3 Character. ISO3 country code, used for layer processing and output naming.
#' @param pus SpatRaster. Planning units raster (terra object) defining resolution and extent.
#' @param current_protected_areas Optional. An `sf` or `SpatVector` of protected areas. If `NULL`, the function calls `make_protected_areas()`.
#' @param iucn_get_prefixes Optional character vector. Filter to only include layers with specific IUCN ecosystem prefixes (e.g., "T", "M", etc.).
#' @param include_minor_occurence Logical. Whether to include ecosystems marked as minor occurrences (default: TRUE).
#' @param output_path Optional character. If provided, saves the output raster to this directory.
#'
#' @return A normalized `SpatRaster` layer representing average underrepresentation across ecosystems.
#' @export
#'
#' @examples
#' \dontrun{
#' underrep <- make_underrepresented_ecosystems(
#'   iucn_get_directory = "data/iucn_layers",
#'   iso3 = "KEN",
#'   pus = planning_units,
#'   output_path = "outputs"
#' )
#' }
make_underrepresented_ecosystems <- function(
    iucn_get_directory,
    iso3,
    pus,
    current_protected_areas = NULL,
    iucn_get_prefixes = NULL,
    include_minor_occurence = TRUE,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory))
  assertthat::assert_that(dir.exists(iucn_get_directory))
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a valid ISO3 code, e.g., 'NPL'.")

  # Load or validate protected areas
  if (is.null(current_protected_areas)) {
    current_protected_areas <- elsar::make_protected_areas(
      iso3 = iso3,
      download_path = here::here(),
      buffer_points = TRUE,
      pus = pus,
      return_sf = TRUE
    )
  } else {
    assertthat::assert_that(
      inherits(current_protected_areas, "sf") || inherits(current_protected_areas, "SpatVector"),
      msg = "'current_protected_areas' must be an 'sf' or 'SpatVector' object."
    )
  }

  # Extract IUCN GET ecosystems from .gpkg files
  cat("Collecting IUCN GET ecosystems...\n")
  iucn_ecosystems <- get_iucn_ecosystems(
    iucn_get_directory = iucn_get_directory,
    iso3 = iso3,
    pus = pus,
    iucn_get_prefixes = iucn_get_prefixes,
    include_minor_occurence = include_minor_occurence,
    output_path = NULL
  )

  # Clip ecosystems to planning unit boundary (if not already)
  iucn_ecosystems <- iucn_ecosystems %>%
    sf::st_make_valid()

  # Calculate protected area coverage per ecosystem
  cat("Calculating protected area coverage of each IUCN GET ecosystem...\n")
  iucn_ecosysytems_pa_area <- iucn_ecosystems %>%
    sf::st_filter(current_protected_areas) %>%
    sf::st_make_valid() %>%
    sf::st_intersection(current_protected_areas) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area_protected = units::drop_units(sf::st_area(.))) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(ID, area_protected)

  # Calculate total area and underrepresentation gap per ecosystem
  cat("Calculating representation gap from 30% protection target...\n")
  iucn_ecosysytems_total <- iucn_ecosystems %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(iucn_ecosysytems_pa_area, by = 'ID') %>%
    dplyr::mutate(
      percent_protected = area_protected / area * 100,
      target = ifelse(percent_protected < 30, 30 - percent_protected, 0)
    )

  # Rasterize and normalize the representation gap
  cat("Calculating average representation gap and normalising raster output...\n")
  underrepresented_ecosystems <- elsar::get_underrepresented_ecosystems(
    x = iucn_ecosysytems_total,
    pus = pus
    )
  names(underrepresented_ecosystems) <- "underrepresented_ecosystems"

  # Optionally write output
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/underrepresented_ecosystems_{iso3}.tif")
    cat(glue::glue("Writing output to: {out_file}"), "\n")

    terra::writeRaster(
      underrepresented_ecosystems,
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

  return(underrepresented_ecosystems)
}

#' Create Underrepresented Ecosystems Raster Based on IUCN GET and Protection Gaps
#'
#' This function calculates the representation gap of ecosystems defined in the IUCN Global Ecosystem Typology (GET)
#' dataset, identifying those that fall short of a 30% protection target. The resulting raster layer indicates
#' the average degree of underrepresentation across each planning unit.
#'
#' The function uses spatial overlays to calculate the proportion of each ecosystem that is currently protected,
#' computes the gap toward a 30% target, and then rasterizes this value onto the planning units. The output raster
#' can be used to prioritise areas for protection of underrepresented ecosystems.
#'
#' @param iucn_get_directory Character. Path to the directory containing the IUCN GET `.gpkg` files.
#' @param iso3 Character. ISO3 country code (e.g., "KEN").
#' @param pus A `SpatRaster`. Raster of planning units (e.g., from `make_planning_units()`).
#' @param boundary_layer An `sf` object of the national boundary used to clip ecosystems.
#' @param current_protected_areas Optional. An `sf` or `SpatVector` of protected areas. If NULL, data is fetched using `make_protected_areas()`.
#' @param iucn_get_prefixes Optional character vector. Filters `.gpkg` filenames by prefixes (e.g., c("T", "F")).
#' @param include_minor_occurrence Logical. Whether to include ecosystems marked as "minor" (default = TRUE).
#' @param output_path Optional character. Directory path to save the output raster.
#'
#' @return A normalised `SpatRaster` layer representing the average protection gap across ecosystems.
#' @export
#'
#' @examples
#' \dontrun{
#' underrep <- make_underrepresented_ecosystems(
#'   iucn_get_directory = "data/iucn_layers",
#'   boundary_layer = boundary_layer,
#'   iso3 = "KEN",
#'   pus = planning_units,
#'   output_path = "outputs"
#' )
#' }
make_underrepresented_ecosystems <- function(
    iucn_get_directory,
    iso3,
    pus,
    boundary_layer = boundary_layer,
    current_protected_areas = NULL,
    iucn_get_prefixes = NULL,
    include_minor_occurrence = TRUE,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory))
  assertthat::assert_that(dir.exists(iucn_get_directory))
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a valid ISO3 code, e.g., 'NPL'.")
  assertthat::assert_that(inherits(boundary_layer, "sf"), msg = "'boundary_layer' must be a sf object.")

  # Load and validate protected areas
    assertthat::assert_that(
      inherits(current_protected_areas, "sf") || inherits(current_protected_areas, "SpatVector"),
      msg = "'current_protected_areas' must be an 'sf' or 'SpatVector' object."
    )

  # Extract IUCN GET ecosystems from .gpkg files
  log_msg("Collecting IUCN GET ecosystems...")
  iucn_ecosystems <- elsar::get_iucn_ecosystems(
    iucn_get_directory = iucn_get_directory,
    iso3 = iso3,
    boundary_layer = boundary_layer,
    pus = pus,
    iucn_get_prefixes = iucn_get_prefixes,
    include_minor_occurrence = include_minor_occurrence,
    output_path = NULL
  )

  # Calculate protected area coverage per ecosystem
  log_msg("Calculating protected area coverage of each IUCN GET ecosystem...")
  iucn_ecosysytems_pa_area <- iucn_ecosystems %>%
    sf::st_filter(current_protected_areas) %>%
    sf::st_intersection(current_protected_areas) %>%
    sf::st_make_valid() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area_protected = units::drop_units(sf::st_area(.))) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select("id", "area_protected")

  # Calculate total area and underrepresentation gap per ecosystem
  log_msg("Calculating representation gap from 30% protection target...")
  iucn_ecosysytems_total <- iucn_ecosystems %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(iucn_ecosysytems_pa_area, by = 'id') %>%
    dplyr::mutate(
      percent_protected = .data$area_protected / .data$area * 100,
      target = ifelse(.data$percent_protected < 30, 30 - .data$percent_protected, 0)
    )

  # Rasterize and normalize the representation gap
  log_msg("Calculating average representation gap and normalising raster output...")
  underrepresented_ecosystems <- elsar::exact_rasterise(
    features = iucn_ecosysytems_total,
    pus = pus,
    iso3 = iso3,
    attribute = "target"
  )
  names(underrepresented_ecosystems) <- "underrepresented_ecosystems"

  # Optionally write output
  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/underrepresented_ecosystems_{iso3}.tif")
    log_msg(glue::glue("Writing output to: {out_file}"))

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

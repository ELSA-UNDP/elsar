#' Create a Representation Gap Raster for Ecosystems
#'
#' This function calculates the representation gap of ecosystems provided, identifying
#' those that fall short of a 30% protection target. For each ecosystem the gap is
#' `target_percent` minus its protected percentage (0 once the target is met). Each
#' planning unit takes the largest gap of the ecosystems it overlaps, divided by
#' `target_percent`, so 0 means the target is met and 1 means no protection.
#'
#' @param ecosystems_sf `sf` object. Polygon features representing ecosystems, clipped to the analysis area.
#' @param group_attribute `character`. Column name in `ecosystems_sf` used to group features (e.g., "econame" or "get_id").
#' @param target_percent `numeric`. Target percentage of protection for each ecosystem group (default = 30).
#' @param protected_areas_sf `sf` or `SpatVector`. Protected areas dataset to be used in the gap calculation.
#'   May have 0 rows (a country with no protected areas): every ecosystem then
#'   has the full gap, and the result is 1 wherever there is an ecosystem, with a warning.
#' @param pus `SpatRaster`. Raster of planning units (e.g., from `make_planning_units()`), used as the target grid for rasterization.
#' @param iso3 `character`, optional. ISO3 country code used for naming output files (e.g., "BRA").
#' @param output_path `character`, optional. Directory path to save the output raster as a GeoTIFF.
#'
#' @return A `SpatRaster` with one layer: `"underrepresented_ecosystems"`, showing the largest gap per planning unit as a share of the target (0-1). The
#'   scale is absolute, not rescaled across ecosystems (before 0.4.2 the largest gap
#'   in the region became 1, however small it was).
#' @export
#'
#' @examples
#' \dontrun{
#' gap_raster <- make_underrepresented_ecosystems(
#'   ecosystems_sf = my_ecosystem_polygons,
#'   group_attribute = "eco_type",
#'   target_percent = 30,
#'   protected_areas_sf = wdpa,
#'   pus = planning_units,
#'   iso3 = "KEN",
#'   output_path = "outputs"
#' )
#' }
#'
make_underrepresented_ecosystems <- function(
    ecosystems_sf,
    group_attribute,
    target_percent = 30,
    protected_areas_sf,
    pus,
    iso3 = NULL,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(inherits(ecosystems_sf, "sf"),
                          msg = "'ecosystems_sf' must be an sf object.")
  assertthat::assert_that(group_attribute %in% colnames(ecosystems_sf),
                          msg = glue::glue("'{group_attribute}' not found in 'ecosystems_sf' columns."))
  assertthat::assert_that(inherits(pus, "SpatRaster"),
                          msg = "'pus' must be a SpatRaster object.")
  assertthat::assert_that(
    inherits(protected_areas_sf, "sf") || inherits(protected_areas_sf, "SpatVector"),
    msg = "'protected_areas_sf' must be an sf or SpatVector object."
  )
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  log_message("Calculating protection gaps for ecosystems grouped by '{group_attribute}'...")

  # Convert to sf if needed
  if (inherits(protected_areas_sf, "SpatVector")) {
    protected_areas_sf <- sf::st_as_sf(protected_areas_sf)
  }

  # Ensure CRS match
  if (!all(sf::st_crs(protected_areas_sf) == sf::st_crs(ecosystems_sf))) {
    protected_areas_sf <- sf::st_transform(protected_areas_sf, sf::st_crs(ecosystems_sf))
  }

  # Ecosystem and protected area per group, intersected tile by tile. A loop
  # over every protected-area polygon took 85 min for Denmark (150,000
  # polygons); by tile it takes about a minute. Both layers are dissolved
  # first (by tile), so overlapping protected areas, or overlapping polygons
  # of one ecosystem, are not counted twice.
  tiles <- overlay_tiles(list(ecosystems_sf, protected_areas_sf), pus)
  ov <- group_overlap_tiled(ecosystems_sf, group_attribute, protected_areas_sf, tiles)
  gaps <- ov$areas %>%
    dplyr::mutate(
      # An ecosystem no protected area overlaps is 0% protected (the full gap)
      area_protected = .data$area_overlap,
      percent_protected = .data$area_protected / .data$area * 100,
      target_gap = dplyr::if_else(.data$percent_protected < target_percent,
                                  target_percent - .data$percent_protected, 0)
    )
  ecosystems_summary <- ov$pieces %>%
    dplyr::left_join(gaps, by = group_attribute)

  # Rasterize the gap as a share of the target: 0 where the target is met, 1
  # for an ecosystem with no protection. (Before 0.4.2 the gaps were rescaled
  # to 0-1 across ecosystems, so the largest gap became 1 however small it
  # was, and a region with a single ecosystem became a constant 1.)
  no_pas <- nrow(protected_areas_sf) == 0 || all(sf::st_is_empty(protected_areas_sf))
  if (no_pas) {
    warning("No protected areas: every ecosystem is 0% protected, so the ",
            "representation gap is uniform (", target_percent, "%) and the ",
            "layer carries no information for prioritisation.", call. = FALSE)
  }
  log_message("Rasterizing the largest target gap per planning unit...")
  underrepresented_ecosystems <- elsar::exact_rasterise(
    features = ecosystems_summary,
    pus = pus,
    fun = "max",
    iso3 = iso3,
    attribute = "target_gap",
    rescaled = FALSE
  ) / target_percent
  names(underrepresented_ecosystems) <- "underrepresented_ecosystems"

  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/representation_gap_{tolower(group_attribute)}_{iso3}.tif")
    elsar::save_raster(underrepresented_ecosystems, out_file, datatype = "FLT4S")
  }

  return(underrepresented_ecosystems)
}

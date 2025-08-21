#' Create a Representation Gap Raster for Ecosystems
#'
#' This function calculates the representation gap of ecosystems provided, identifying
#' those that fall short of a 30% protection target. The resulting raster layer indicates
#' the average degree of underrepresentation across each planning unit.
#'#'
#' @param ecosystems_sf `sf` object. Polygon features representing ecosystems, clipped to the analysis area.
#' @param group_attribute `character`. Column name in `ecosystems_sf` used to group features (e.g., "econame" or "get_id").
#' @param target_percent `numeric`. Target percentage of protection for each ecosystem group (default = 30).
#' @param protected_areas_sf `sf` or `SpatVector`. Protected areas dataset to be used in the gap calculation.
#' @param pus `SpatRaster`. Raster of planning units (e.g., from `make_planning_units()`), used as the target grid for rasterization.
#' @param iso3 `character`, optional. ISO3 country code used for naming output files (e.g., "BRA").
#' @param output_path `character`, optional. Directory path to save the output raster as a GeoTIFF.
#'
#' @return A `SpatRaster` with one layer: `"underrepresented_ecosystems"`, showing the maximum gap (0 to target_percent) per planning unit.
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
  stopifnot(inherits(ecosystems_sf, "sf"))
  stopifnot(group_attribute %in% colnames(ecosystems_sf))
  stopifnot(inherits(pus, "SpatRaster"))
  stopifnot(inherits(protected_areas_sf, "sf") || inherits(protected_areas_sf, "SpatVector"))

  message(glue::glue("Calculating protection gaps for ecosystems grouped by '{group_attribute}'..."))

  # Convert to sf if needed
  if (inherits(protected_areas_sf, "SpatVector")) {
    protected_areas_sf <- sf::st_as_sf(protected_areas_sf)
  }

  # Ensure CRS match
  if (!all(sf::st_crs(protected_areas_sf) == sf::st_crs(ecosystems_sf))) {
    protected_areas_sf <- sf::st_transform(protected_areas_sf, sf::st_crs(ecosystems_sf))
  }

  # Perform intersection (sequential)
  protected_intersections <- list()
  for (i in seq_len(nrow(protected_areas_sf))) {
    pa <- protected_areas_sf[i, ]
    try({
      int <- sf::st_intersection(ecosystems_sf, pa)
      int <- sf::st_make_valid(int)
      int$area_protected <- units::drop_units(sf::st_area(int))
      protected_intersections[[i]] <- sf::st_set_geometry(int, NULL)[, c(group_attribute, "area_protected"), drop = FALSE]
    }, silent = TRUE)
  }

  # Bind and summarize
  protected_df <- dplyr::bind_rows(Filter(Negate(is.null), protected_intersections)) %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(area_protected = sum(.data$area_protected, na.rm = TRUE), .groups = "drop")

  # Total ecosystem area by group
  ecosystems_summary <- ecosystems_sf %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(protected_df, by = group_attribute) %>%
    dplyr::mutate(
      percent_protected = .data$area_protected / .data$area * 100,
      target_gap = dplyr::if_else(.data$percent_protected < target_percent, target_percent - .data$percent_protected, 0)
    )

  # Rasterize
  message("Rasterizing average target gap per planning unit...")
  underrepresented_ecosystems <- elsar::exact_rasterise(
    features = ecosystems_summary,
    pus = pus,
    fun = "max",
    iso3 = iso3,
    attribute = "target_gap"
  )
  names(underrepresented_ecosystems) <- "underrepresented_ecosystems"

  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/representation_gap_{tolower(group_attribute)}_{iso3}.tif")
    elsar::save_raster(underrepresented_ecosystems, out_file, datatype = "FLT4S")
  }

  return(underrepresented_ecosystems)
}

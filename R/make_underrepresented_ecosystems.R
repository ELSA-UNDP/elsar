#' Create a Representation Gap Raster for Ecosystems
#'
#' Calculates the protection gap for a given set of ecosystem features by dissolving
#' them by an attribute, computing total and protected area, and rasterizing the gap toward a given protection target.
#'
#' @param ecosystems_sf sf object. Ecosystem polygons clipped to the analysis area.
#' @param group_attribute Character. Column name in `ecosystems_sf` to dissolve by (e.g., "econame").
#' @param target_percent Numeric. Desired protection target (default is 30 for 30%).
#' @param protected_areas_sf sf or SpatVector. Current protected area polygons.
#' @param pus SpatRaster. Planning unit raster (e.g., from `make_planning_units()`).
#' @param iso3 Optional ISO3 country code for logging or output naming.
#' @param output_path Optional path to save the output raster.
#'
#' @return SpatRaster of average representation gap across planning units.
#' @export
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

  log_msg(glue::glue("Calculating protection gaps for ecosystems grouped by '{group_attribute}'..."))

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
    dplyr::summarise(area_protected = sum(area_protected, na.rm = TRUE), .groups = "drop")

  # Total ecosystem area by group
  ecosystems_summary <- ecosystems_sf %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(protected_df, by = group_attribute) %>%
    dplyr::mutate(
      percent_protected = area_protected / area * 100,
      target_gap = dplyr::if_else(percent_protected < target_percent, target_percent - percent_protected, 0)
    )

  # Rasterize
  log_msg("Rasterizing average target gap per planning unit...")
  underrep_raster <- elsar::exact_rasterise(
    features = ecosystems_summary,
    pus = pus,
    fun = "max",
    iso3 = iso3,
    attribute = "target_gap"
  )
  names(underrep_raster) <- "representation_gap"

  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/representation_gap_{tolower(group_attribute)}_{iso3}.tif")
    elsar::save_raster(underrep_raster, out_file, datatype = "FLT4S")
  }

  return(underrep_raster)
}

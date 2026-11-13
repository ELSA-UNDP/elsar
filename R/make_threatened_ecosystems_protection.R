#' Create a Threatened Ecosystems Raster Based on Integrity or Degradation
#'
#' This function calculates ecosystem-level threat scores based on the proportion
#' of degraded or low-integrity land within each ecosystem polygon. It supports either
#' an Ecological Intactness Index (EII) style continuous integrity raster (thresholded using
#' the global median value) or a binary degraded areas raster.
#'
#' The threat score is calculated as the percentage of each ecosystem area that is
#' below the integrity threshold (i.e., not intact), and rasterized by planning unit.
#'
#' @param ecosystems_sf sf. Polygons representing ecosystems.
#' @param group_attribute character. Column name used to group/dissolve ecosystems (e.g., `"eco_id"` or `"get_id"`).
#' @param pus SpatRaster. Planning unit raster used for alignment and rasterization.
#' @param boundary_layer sf. National boundary used to calculate the median integrity value if `integrity_type = "eii"`.
#' @param integrity_raster SpatRaster. Raster representing either continuous integrity (e.g., EII) or binary degraded areas.
#' @param integrity_type character. One of `"eii"` (default) or `"degraded"`, indicating how to interpret the integrity raster.
#' @param iso3 character, optional. ISO3 code used for naming output files (e.g., `"KEN"`).
#' @param output_path character, optional. Directory to write the output raster as a GeoTIFF.
#'
#' @return A `SpatRaster` with one layer: `"threatened_ecosystems"`. Each
#'   ecosystem's threat (0-100% of its area not intact) is rescaled to 0-1
#'   across the ecosystems present; higher values mean more threatened.
#'   Planning units outside every ecosystem polygon take the nearest
#'   ecosystem's value. If every ecosystem has the same threat, a warning of
#'   class `elsar_uninformative_layer` is raised.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using EII (Ecological Integrity Index)
#' threatened_raster_eii <- make_threatened_ecosystems_protection(
#'   ecosystems_sf = my_ecosystems,
#'   group_attribute = "eco_id",
#'   pus = planning_units,
#'   boundary_layer = national_boundary,
#'   integrity_raster = eii_raster,
#'   integrity_type = "eii",
#'   iso3 = "KEN",
#'   output_path = "outputs"
#' )
#'
#' # Using binary degraded areas raster (1 = degraded, 0 = intact)
#' threatened_raster_degraded <- make_threatened_ecosystems_protection(
#'   ecosystems_sf = my_ecosystems,
#'   group_attribute = "eco_id",
#'   pus = planning_units,
#'   boundary_layer = national_boundary,
#'   integrity_raster = degraded_raster,
#'   integrity_type = "degraded",
#'   iso3 = "KEN"
#' )
#' }
make_threatened_ecosystems_protection <- function(
    ecosystems_sf,
    group_attribute,
    pus,
    boundary_layer,
    integrity_raster,
    integrity_type = c("eii", "degraded"),
    iso3 = NULL,
    output_path = NULL
) {
  integrity_type <- match.arg(integrity_type)

  # Input validation
 assertthat::assert_that(inherits(ecosystems_sf, "sf"),
                          msg = "'ecosystems_sf' must be an sf object.")
  assertthat::assert_that(group_attribute %in% colnames(ecosystems_sf),
                          msg = glue::glue("'{group_attribute}' not found in 'ecosystems_sf' columns."))
  assertthat::assert_that(inherits(pus, "SpatRaster"),
                          msg = "'pus' must be a SpatRaster object.")
  assertthat::assert_that(inherits(boundary_layer, "sf"),
                          msg = "'boundary_layer' must be an sf object.")
  assertthat::assert_that(inherits(integrity_raster, "SpatRaster"),
                          msg = "'integrity_raster' must be a SpatRaster object.")
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  log_message("Calculating ecosystem threat using integrity data type: {integrity_type}")

  # Align and normalize integrity raster to match planning units
  integrity_aligned <- elsar::make_normalised_raster(
    raster_in = integrity_raster,
    pus = pus
  )

  # Create binary mask of intact areas
  if (integrity_type == "eii") {
    med_val <- median_from_rast(integrity_raster) # Median calculated from pre-computed histogram
    log_message(
      "Global median intactness value ({med_val}) used to define intact areas."
    )
    mask <- terra::ifel(integrity_aligned >= med_val, 1, 0)
  } else {
    mask <- terra::ifel(integrity_aligned == 0, 1, 0)
  }

  # Convert intact mask to valid polygons
  non_intact_areas <- mask %>%
    terra::as.polygons(na.rm = TRUE) %>%
    sf::st_as_sf() %>%
    dplyr::filter(.[[1]] == 1) %>%
    # The mask is on the planning-unit grid: transform it to the ecosystems'
    # CRS (before 0.4.2 it was relabelled, giving an all-NA layer whenever the
    # ecosystems were in another CRS)
    sf::st_make_valid() %>%
    sf::st_transform(sf::st_crs(ecosystems_sf)) %>%
    sf::st_cast("POLYGON")

  # Intact area per ecosystem, intersected tile by tile (both layers
  # dissolved by tile first). Replaces a whole-layer union of the ecosystems
  # and an intersection of every ecosystem with every intact-area polygon.
  log_message("Calculating intact areas within each ecosystem...")
  tiles <- overlay_tiles(list(ecosystems_sf, non_intact_areas), pus)
  # The mask polygons come from one raster value, so they cannot overlap
  ov <- group_overlap_tiled(ecosystems_sf, group_attribute, non_intact_areas, tiles,
                            y_disjoint = TRUE)

  log_message("Summarising total ecosystem area and computing threat scores...")
  threat <- ov$areas %>%
    dplyr::mutate(threat = (1 - .data$area_overlap / .data$area) * 100) %>%
    dplyr::select(dplyr::all_of(c(group_attribute, "threat")))
  eco_summary <- ov$pieces %>%
    dplyr::left_join(threat, by = group_attribute)

  # The same score for every ecosystem cannot rank planning units; after
  # rescaling it would only show where the ecosystem polygons are
  uniq <- unique(round(threat$threat, 6))
  if (length(uniq) == 1) {
    warn_uninformative(
      "Threatened ecosystems",
      paste0("all ", nrow(threat), " ecosystem(s) are ", uniq,
             "% non-intact (integrity below the ",
             if (integrity_type == "eii") "global median" else "intact class", ").")
    )
  }

  # Rasterize threat scores. Planning units outside every ecosystem polygon
  # (e.g. coast the coarse polygons miss) take the nearest ecosystem's score;
  # before, they were 0, i.e. "not threatened".
  log_message("Rasterizing threat score per planning unit...")
  threat_raster <- elsar::exact_rasterise(
    features = eco_summary,
    pus = pus,
    iso3 = iso3,
    attribute = "threat",
    fill_nearest = TRUE
  )
  names(threat_raster) <- "threatened_ecosystems"

  # Optionally write output raster
  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/threatened_ecosystems_{iso3}.tif")
    elsar::save_raster(threat_raster, out_file, datatype = "FLT4S")
  }

  return(threat_raster)
}

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
#' @param ecosystems sf. Polygons representing ecosystems, or a
#'   single-layer `SpatRaster` of ecosystem classes (integer codes, or a
#'   categorical raster; NA = no ecosystem). A raster is projected to the
#'   planning-unit CRS (nearest neighbour) if needed, and only its cells inside
#'   the planning units count. See [ecosystem_raster].
#' @param group_attribute character. Column name used to group/dissolve ecosystems (e.g., `"eco_id"` or `"get_id"`).
#'   For a categorical raster, a column of its levels table (`NULL`: the
#'   active category); a plain integer raster is grouped by cell value and
#'   this is not used.
#' @param pus SpatRaster. Planning unit raster used for alignment and rasterization.
#' @param boundary_layer sf. National boundary used to calculate the median integrity value if `integrity_type = "eii"`.
#' @param integrity_raster SpatRaster. Raster representing either continuous integrity (e.g., EII) or binary degraded areas.
#' @param integrity_type character. One of `"eii"` (default) or `"degraded"`, indicating how to interpret the integrity raster.
#' @param iso3 character, optional. ISO3 code used for naming output files (e.g., `"KEN"`).
#' @param ecosystems_sf `r lifecycle::badge("deprecated")` Use `ecosystems`.
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
#'   ecosystems = my_ecosystems,
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
#'   ecosystems = my_ecosystems,
#'   group_attribute = "eco_id",
#'   pus = planning_units,
#'   boundary_layer = national_boundary,
#'   integrity_raster = degraded_raster,
#'   integrity_type = "degraded",
#'   iso3 = "KEN"
#' )
#' }
make_threatened_ecosystems_protection <- function(
    ecosystems,
    group_attribute = NULL,
    pus,
    boundary_layer,
    integrity_raster,
    integrity_type = c("eii", "degraded"),
    iso3 = NULL,
    output_path = NULL,
    ecosystems_sf = deprecated()
) {
  if (lifecycle::is_present(ecosystems_sf)) {
    lifecycle::deprecate_warn("0.8.0", "make_threatened_ecosystems_protection(ecosystems_sf)", "make_threatened_ecosystems_protection(ecosystems)")
    ecosystems <- ecosystems_sf
  }
  integrity_type <- match.arg(integrity_type)

  # Input validation
  is_raster <- inherits(ecosystems, "SpatRaster")
  assertthat::assert_that(is_raster || inherits(ecosystems, "sf"),
                          msg = "'ecosystems' must be an sf object or a SpatRaster.")
  if (!is_raster) {
    assertthat::assert_that(!is.null(group_attribute) && group_attribute %in% colnames(ecosystems),
                            msg = glue::glue("'{group_attribute}' not found in 'ecosystems' columns."))
  }
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

  if (is_raster) {
    # Share of every ecosystem cell that is intact: the PU mask resampled
    # exactly to the ecosystem grid. Cells outside the mask count as not
    # intact, as in the polygon path.
    eco <- prepare_ecosystem_raster(ecosystems, group_attribute, pus)
    mask0 <- terra::ifel(is.na(mask), 0, mask)
    intact <- exactextractr::exact_resample(mask0, eco$r, fun = "mean")
    log_message("Summarising total ecosystem area and computing threat scores...")
    areas <- ecosystem_raster_areas(eco$r, intact)
    group_attribute <- "gid"
  } else {
    # Convert intact mask to valid polygons
    intact_areas <- mask %>%
      terra::as.polygons(na.rm = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::filter(.[[1]] == 1) %>%
      # The mask is on the planning-unit grid: transform it to the ecosystems'
      # CRS (before 0.4.2 it was relabelled, giving an all-NA layer whenever the
      # ecosystems were in another CRS)
      sf::st_make_valid() %>%
      sf::st_transform(sf::st_crs(ecosystems)) %>%
      sf::st_cast("POLYGON")

    # Intact area per ecosystem, intersected tile by tile (both layers
    # dissolved by tile first). Replaces a whole-layer union of the ecosystems
    # and an intersection of every ecosystem with every intact-area polygon.
    log_message("Calculating intact areas within each ecosystem...")
    tiles <- overlay_tiles(list(ecosystems, intact_areas), pus)
    # The mask polygons come from one raster value, so they cannot overlap
    ov <- group_overlap_tiled(ecosystems, group_attribute, intact_areas, tiles,
                              y_disjoint = TRUE)
    log_message("Summarising total ecosystem area and computing threat scores...")
    areas <- ov$areas
  }

  threat <- areas %>%
    dplyr::mutate(threat = (1 - .data$area_overlap / .data$area) * 100) %>%
    dplyr::select(dplyr::all_of(c(group_attribute, "threat")))

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
  threat_raster <- if (is_raster) {
    ecosystem_raster_to_pus(eco$r, threat$gid, threat$threat, pus = pus,
                            method = "mean", iso3 = iso3, rescaled = TRUE)
  } else {
    elsar::exact_rasterise(
      features = dplyr::left_join(ov$pieces, threat, by = group_attribute),
      pus = pus,
      iso3 = iso3,
      attribute = "threat",
      fill_nearest = TRUE
    )
  }
  names(threat_raster) <- "threatened_ecosystems"

  # Optionally write output raster
  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/threatened_ecosystems_{iso3}.tif")
    elsar::save_raster(threat_raster, out_file, datatype = "FLT4S")
  }

  return(threat_raster)
}

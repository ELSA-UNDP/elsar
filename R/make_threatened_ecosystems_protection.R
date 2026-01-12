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
#' @return A `SpatRaster` with one layer: `"threatened_ecosystems"`, with values from 0 to 100.
#'         Higher values indicate greater ecosystem degradation or threat.
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
    sf::st_make_valid() %>%
    sf::st_set_crs(sf::st_crs(ecosystems_sf)) %>%
    sf::st_cast("POLYGON")

  log_message("Calculating intact areas within each ecosystem...")

  # Setup parallel processing
  if (!requireNamespace("future.apply", quietly = TRUE)) stop("Please install the 'future.apply' package.")
  if (!requireNamespace("progressr", quietly = TRUE)) stop("Please install the 'progressr' package.")

  n_cores <- parallel::detectCores(logical = FALSE)
  n_workers <- max(1, floor(n_cores / 2))

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  if (.Platform$OS.type == "unix" && !interactive()) {
    future::plan(future::multicore, workers = n_workers)
  } else {
    future::plan(future::multisession, workers = n_workers)
  }

  progressr::handlers("txtprogressbar")

  # Split ecosystems into list of single-row sf objects
  eco_list <- split(ecosystems_sf, seq_len(nrow(ecosystems_sf)))

  # Calculate intact area for each ecosystem polygon
  ecosystems_intact_area_list <- progressr::with_progress({
    p <- progressr::progressor(along = eco_list)

    future.apply::future_lapply(eco_list, function(eco) {
      p()

      tryCatch({
        result <- sf::st_intersection(eco, non_intact_areas)
        result <- sf::st_make_valid(result)
        area_intact <- sum(units::drop_units(sf::st_area(result)))

        data.frame(group = eco[[group_attribute]], area_intact = area_intact)
      }, error = function(e) {
        message("Error processing ecosystem ID ", eco[[group_attribute]], ": ", conditionMessage(e))
        return(data.frame(group = eco[[group_attribute]], area_intact = NA))
      })
    })
  })

  # Combine and aggregate intact area results
  ecosystems_intact_area <- dplyr::bind_rows(Filter(Negate(is.null), ecosystems_intact_area_list)) %>%
    dplyr::rename(!!group_attribute := group) %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(area_intact = sum(area_intact, na.rm = TRUE), .groups = "drop")

  # Summarize total area and compute threat score
  log_message("Summarising total ecosystem area and computing threat scores...")

  eco_summary <- ecosystems_sf %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::mutate(area_total = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(ecosystems_intact_area, by = group_attribute) %>%
    dplyr::mutate(
      threat = dplyr::if_else(is.na(area_intact), 100, (1 - area_intact / area_total) * 100)
    )

  # Rasterize threat scores
  log_message("Rasterizing threat score per planning unit...")
  threat_raster <- elsar::exact_rasterise(
    features = eco_summary,
    pus = pus,
    iso3 = iso3,
    attribute = "threat"
  )
  names(threat_raster) <- "threatened_ecosystems"

  # Optionally write output raster
  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/threatened_ecosystems_{iso3}.tif")
    elsar::save_raster(threat_raster, out_file, datatype = "FLT4S")
  }

  return(threat_raster)
}

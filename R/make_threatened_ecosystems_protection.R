#' Create a Threatened Ecosystems Raster Based on Integrity or Degradation
#'
#' This generic function calculates ecosystem-level threat scores based on the
#' proportion of degraded or low-integrity land within each ecosystem polygon.
#' It supports either an EII-style continuous integrity raster (thresholded using
#' the national median) or a binary degraded areas raster.
#'
#' @param ecosystems_sf sf. Polygons representing ecosystems.
#' @param group_attribute Character. Column name to group/dissolve ecosystems by (e.g., "eco_id").
#' @param pus SpatRaster. Planning unit raster for alignment and rasterization.
#' @param boundary_layer sf. National boundary used to compute the median EII if `integrity_type = "eii"`.
#' @param integrity_raster SpatRaster. Raster representing either EII (continuous) or degraded areas (binary).
#' @param integrity_type Character. One of `"eii"` (default) or `"degraded"`, indicating how to interpret `integrity_raster`.
#' @param iso3 Optional character. ISO3 country code for naming (e.g., "KEN").
#' @param output_path Optional character. Folder path to save the output raster.
#'
#' @return A `SpatRaster` with one layer: `"threatened_ecosystems"` (range 0–100).
#' @export
#'
#' @examples
#' \dontrun{
#' # Using EII (ecological integrity index)
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
#' # Using a binary degraded areas raster (0 = intact, 1 = degraded)
#' threatened_raster_degraded <- make_threatened_ecosystems_protection(
#'   ecosystems_sf = my_ecosystems,
#'   group_attribute = "eco_id",
#'   pus = planning_units,
#'   boundary_layer = national_boundary,
#'   integrity_raster = degraded_raster,
#'   integrity_type = "degraded",
#'   iso3 = "KEN"
#' )
#'
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

  stopifnot(inherits(ecosystems_sf, "sf"))
  stopifnot(group_attribute %in% colnames(ecosystems_sf))
  stopifnot(inherits(pus, "SpatRaster"))
  stopifnot(inherits(boundary_layer, "sf"))
  stopifnot(inherits(integrity_raster, "SpatRaster"))

  log_msg(glue::glue("Preparing threat assessment using integrity type: {integrity_type}"))

  # Normalize or align integrity raster
  integrity_aligned <- elsar::make_normalised_raster(
    raster_in = integrity_raster,
    pus = pus,
    iso3 = iso3
  )

  # Build binary mask: 1 = intact, 0 = degraded/low-integrity
  if (integrity_type == "eii") {
    med_val <- exactextractr::exact_extract(integrity_aligned, boundary_layer, fun = "median")
    mask <- terra::ifel(integrity_aligned >= med_val, 1, 0)
  } else {
    mask <- terra::ifel(integrity_aligned == 0, 1, 0)
  }

  # Convert to polygon
  non_intact_areas <- mask %>%
    terra::as.polygons(na.rm = TRUE) %>%
    sf::st_as_sf() %>%
    dplyr::filter(.[[1]] == 1) %>%
    sf::st_make_valid() %>%
    sf::st_set_crs(sf::st_crs(ecosystems_sf)) %>%
    sf::st_cast("POLYGON")

  log_msg("Calculating threat within each ecosystem using parallel processing...")

  # Setup parallel backend if not already done
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

  # Split ecosystems into single row sf objects
  eco_list <- split(ecosystems_sf, seq_len(nrow(ecosystems_sf)))

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
        message("Error processing ecosystem id ", eco[[group_attribute]], ": ", conditionMessage(e))
        return(data.frame(group = eco[[group_attribute]], area_intact = NA))
      })
    })
  })

  # Combine results and summarize
  ecosystems_intact_area <- dplyr::bind_rows(Filter(Negate(is.null), ecosystems_intact_area_list)) %>%
    dplyr::rename(!!group_attribute := group) %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(area_intact = sum(area_intact, na.rm = TRUE), .groups = "drop")

  # Total area and threat calculation per ecosystem
  log_msg("Summarising threat within each ecosystem...")

  eco_summary <- ecosystems_sf %>%
    dplyr::group_by(.data[[group_attribute]]) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::mutate(area_total = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(ecosystems_intact_area, by = group_attribute) %>%
    dplyr::mutate(
      threat = dplyr::if_else(is.na(area_intact), 100, (1 - area_intact / area_total) * 100)
    )

  # Rasterize
  log_msg("Rasterizing threat score by ecosystem...")
  threat_raster <- elsar::exact_rasterise(
    features = eco_summary,
    pus = pus,
    iso3 = iso3,
    attribute = "threat"
  )
  names(threat_raster) <- "threatened_ecosystems"

  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/threatened_ecosystems_{iso3}.tif")
    elsar::save_raster(threat_raster, out_file, datatype = "FLT4S")
  }

  return(threat_raster)
}

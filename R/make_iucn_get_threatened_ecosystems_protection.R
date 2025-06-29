#' Create Threatened Ecosystems (for Protection) Raster
#'
#' This function identifies threatened ecosystems by comparing ecological integrity (EII)
#' values against a national median. For each IUCN GET ecosystem, it calculates the area
#' of intact land (above the median) and expresses the ratio of intact to total area as a
#' threat score. These scores are then rasterized and returned as a continuous raster.
#'
#' @param iso3 Character. ISO3 country code (e.g., `"KEN"`) used for naming and processing.
#' @param pus SpatRaster. Planning units raster used for alignment and resolution.
#' @param boundary_layer sf. National boundary used for calculating the national median.
#' @param intactness_input SpatRaster. Ecological integrity raster.
#' @param iucn_get_sf sf. IUCN GET polygons already subset to the country.
#' @param iucn_get_prefixes Optional character vector. Ecosystem type prefixes to include (e.g., `"T"`, `"M"`).
#' @param include_minor_occurrence Logical. If FALSE, filters out ecosystems marked as minor (occurrence == 1).
#' @param output_path Character or NULL. Directory to write the output raster if desired.
#'
#' @return A normalized SpatRaster layer with ecosystem threat scores (0–100).
#' @export
#'
#' @examples
#' \dontrun{
#' threatened <- make_iucn_get_threatened_ecosystems_protection(
#'   iso3 = "KEN",
#'   pus = planning_units,
#'   boundary_layer = sf::st_read("data/boundary.gpkg"),
#'   intactness_input = rast("data/intactness.tif"),
#'   iucn_get_sf = iucn_get_sf,
#'   output_path = "outputs"
#' )
#' }
make_iucn_get_threatened_ecosystems_protection <- function(
    iso3,
    pus,
    boundary_layer,
    intactness_input,
    iucn_get_sf,
    iucn_get_prefixes = NULL,
    include_minor_occurrence = TRUE,
    output_path = NULL
) {
  # Input checks
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(boundary_layer, "sf"))
  assertthat::assert_that(inherits(iucn_get_sf, "sf"))

  # Filter IUCN GET polygons if needed
  if (!is.null(iucn_get_prefixes)) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, prefix %in% iucn_get_prefixes)
  }

  if (!include_minor_occurrence) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, occurrence != 1)
  }

  # Resample ecological integrity raster to match planning units
  intactness_resampled <- elsar::make_normalised_raster(
    raster_in = intactness_input,
    pus = pus,
    iso3 = iso3
  )

  # National median ecological integrity
  intactness_median <- exactextractr::exact_extract(
    intactness_resampled,
    boundary_layer,
    fun = "median"
  )

  # Mask of intact areas (1 = intact)
  non_intact_areas <- terra::ifel(intactness_resampled < intactness_median, 0, 1) %>%
    terra::as.polygons(na.rm = TRUE) %>%
    sf::st_as_sf() %>%
    dplyr::filter(.[[1]] == 1) %>%
    sf::st_make_valid() %>%
    sf::st_set_crs(sf::st_crs(iucn_get_sf))

  log_msg("Calculating threat within each IUCN GET ecosystem using parallel processing...")

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

  # Split IUCN ecosystems into single row sf objects
  eco_list <- split(iucn_get_sf, seq_len(nrow(iucn_get_sf)))

  ecosystems_intact_area_list <- progressr::with_progress({
    p <- progressr::progressor(along = eco_list)

    future.apply::future_lapply(eco_list, function(eco) {
      p()

      tryCatch({
        result <- sf::st_intersection(eco, non_intact_areas)
        result <- sf::st_make_valid(result)
        area_intact <- sum(units::drop_units(sf::st_area(result)))

        data.frame(get_id = eco$get_id, area_intact = area_intact)

      }, error = function(e) {
        message("Error processing ecosystem id ", eco$get_id, ": ", conditionMessage(e))
        return(data.frame(get_id = eco$get_id, area_intact = NA))
      })
    })
  })

  # Combine results
  ecosystems_intact_area <- dplyr::bind_rows(ecosystems_intact_area_list)

  # Total area and threat calculation per ecosystem
  log_msg("Summarising threat within each ecosystem...")

  ecosystems_total <- iucn_get_sf %>%
    dplyr::group_by(.data$get_id) %>%
    dplyr::summarise() %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(ecosystems_intact_area, by = "get_id") %>%
    dplyr::mutate(threat = dplyr::if_else(is.na(area_intact), 100, (1 - area_intact / area) * 100))

  # Rasterize threat scores
  log_msg("Calculating average intactness and normalising raster output...")

  threat_raster <- elsar::exact_rasterise(
    features = ecosystems_total,
    pus = pus,
    iso3 = iso3,
    attribute = "threat"
  )

  names(threat_raster) <- "threatened_ecosystems_for_protection"

  # Optional write to file
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path), msg = "'output_path' does not exist.")
    out_file <- glue::glue("{output_path}/threatened_ecosystems_for_protection_{iso3}.tif")

    elsar::save_raster(
      raster = threat_raster,
      filename = out_file,
      datatype = "FLT4S"
    )
  }

  return(threat_raster)
}

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
#' @param iucn_get_sf sf object. IUCN GET polygons already loaded and clipped to the analysis area.
#' @param iso3 Character. ISO3 country code (e.g., "KEN").
#' @param pus A `SpatRaster`. Raster of planning units (e.g., from `make_planning_units()`).
#' @param boundary_layer An `sf` object of the national boundary used to clip ecosystems.
#' @param current_protected_areas An `sf` or `SpatVector` object representing protected areas.
#' @param iucn_get_prefixes Optional character vector. Filters the `sf` input by prefix (e.g., c("T", "F")).
#' @param include_minor_occurrence Logical. Whether to include ecosystems marked as "minor" (default = TRUE).
#' @param output_path Optional character. Directory path to save the output raster.
#'
#' @return A normalized `SpatRaster` layer representing the average protection gap across ecosystems.
#' @export
#'
#' @examples
#' \dontrun{
#' underrep <- make_underrepresented_ecosystems(
#'   iucn_get_sf = iucn_data,
#'   boundary_layer = boundary_layer,
#'   current_protected_areas = protected_areas,
#'   iso3 = "KEN",
#'   pus = planning_units,
#'   output_path = "outputs"
#' )
#' }

make_underrepresented_ecosystems <- function(
    iucn_get_sf,
    iso3,
    pus,
    boundary_layer,
    current_protected_areas,
    iucn_get_prefixes = NULL,
    include_minor_occurrence = TRUE,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(inherits(iucn_get_sf, "sf"), msg = "'iucn_get_sf' must be an sf object.")
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a valid ISO3 code, e.g., 'KEN'.")
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster.")
  assertthat::assert_that(inherits(boundary_layer, "sf"), msg = "'boundary_layer' must be a sf object.")
  assertthat::assert_that(
    inherits(current_protected_areas, "sf") || inherits(current_protected_areas, "SpatVector"),
    msg = "'current_protected_areas' must be an 'sf' or 'SpatVector' object."
  )

  # Filter GET data if needed
  if (!is.null(iucn_get_prefixes)) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, prefix %in% iucn_get_prefixes)
  }
  if (!include_minor_occurrence) {
    iucn_get_sf <- dplyr::filter(iucn_get_sf, occurrence != 1)
  }

  if (nrow(iucn_get_sf) == 0) {
    stop("No IUCN GET features remaining after filtering.")
  }

  log_msg("Calculating protected area coverage of each IUCN GET ecosystem using parallel processing...")

  # Load required parallel packages
  if (!requireNamespace("future.apply", quietly = TRUE)) stop("Please install the 'future.apply' package.")
  if (!requireNamespace("progressr", quietly = TRUE)) stop("Please install the 'progressr' package.")

  future::plan(future::multisession)

  progressr::handlers("txtprogressbar")

  # Split sf into list of individual 1-row sf objects
  pa_list <- split(current_protected_areas, seq_len(nrow(current_protected_areas)))

  # Then use future_lapply over pa_list
  iucn_ecosystems_pa_area <- progressr::with_progress({
    p <- progressr::progressor(along = pa_list)

    future.apply::future_lapply(pa_list, function(pa) {
      p(sprintf("Processing PA id %s", pa$id))  # optional, informative message

      iucn_crop <- sf::st_filter(iucn_get_sf, pa)

      if (nrow(iucn_crop) == 0) return(NULL)

      tryCatch({
        result <- sf::st_intersection(iucn_crop, pa)
        result <- sf::st_make_valid(result)
        result$area_protected <- units::drop_units(sf::st_area(result))
        result_df <- sf::st_set_geometry(result, NULL)
        result_df[, c("id", "area_protected"), drop = FALSE]
      }, error = function(e) {
        message("Error processing PA id ", pa$id, ": ", conditionMessage(e))  # error message
        return(NULL)
      })
    })
  })

  # Bind and summarize
  iucn_ecosystems_pa_area <- dplyr::bind_rows(Filter(Negate(is.null), iucn_ecosystems_pa_area)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(area_protected = sum(area_protected, na.rm = TRUE), .groups = "drop")

  # Calculate total area and protection gaps
  log_msg("Calculating representation gap from 30% protection target...")
  iucn_ecosystems_total <- iucn_get_sf %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::mutate(area = units::drop_units(sf::st_area(.))) %>%
    dplyr::left_join(iucn_ecosystems_pa_area, by = "id") %>%
    dplyr::mutate(
      percent_protected = area_protected / area * 100,
      target = dplyr::if_else(percent_protected < 30, 30 - percent_protected, 0)
    )

  # Rasterize and normalize
  log_msg("Calculating average representation gap and normalising raster output...")
  underrepresented_ecosystems <- elsar::exact_rasterise(
    features = iucn_ecosystems_total,
    pus = pus,
    iso3 = iso3,
    attribute = "target"
  )
  names(underrepresented_ecosystems) <- "underrepresented_ecosystems"

  # Optional write
  if (!is.null(output_path)) {
    out_file <- glue::glue("{output_path}/underrepresented_ecosystems_{iso3}.tif")

    elsar::save_raster(
      raster = underrepresented_ecosystems,
      filename = out_file,
      datatype = "FLT4S"
    )
  }

  future::plan(future::sequential)  # Reset plan
  return(underrepresented_ecosystems)
}

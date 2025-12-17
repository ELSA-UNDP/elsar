#' Get Minimum Lock-in Targets for Zones
#'
#' Calculates the minimum lock-in targets for different zones (e.g., protect,
#' restore, manage, and optionally green). Compares input targets with the
#' minimum coverage needed for each zone and ensures targets are not set below
#' this threshold.
#'
#' @param lockin A raster or list of rasters representing locked-in areas for
#'   each zone.
#' @param input A list or dataframe containing target values for each zone
#'   (e.g., `zone_1_target`, `zone_2_target`, etc.).
#' @param pu A raster layer representing the planning unit used for calculating
#'   coverage.
#'
#' @return A `tibble` containing the adjusted target values for each zone.
#' @export
get_min_lockin_target <- function(lockin, input, pu) {
  # Input validation
  assertthat::assert_that(
    inherits(lockin, "SpatRaster") || is.list(lockin),
    msg = "'lockin' must be a SpatRaster or list of SpatRasters."
  )

 assertthat::assert_that(
    is.list(input) || is.data.frame(input),
    msg = "'input' must be a list or data frame containing zone targets."
  )

  assertthat::assert_that(
    inherits(pu, "SpatRaster"),
    msg = "'pu' must be a SpatRaster object."
  )

  log_message("Calculating minimum lock-in targets for zones...")

  # Calculate the minimum coverage of locked-in areas for each zone
  min_coverage <- get_coverage(lockin, pu)

  # Check if the 4th zone (e.g., green zone) is part of the input
  if (!is.null(input$zone_4_target)) {
    # Create a tibble with adjusted targets for all four zones (including the green zone)
    targets <- tibble::tibble(
      zone_1_target = ifelse(
        input$zone_1_target < min_coverage[1],
        min_coverage[1],  # Ensure zone_1 target is at least the minimum coverage
        input$zone_1_target
      ),
      zone_2_target = ifelse(
        input$zone_2_target < min_coverage[2],
        min_coverage[2],  # Ensure zone_2 target is at least the minimum coverage
        input$zone_2_target
      ),
      zone_3_target = ifelse(
        input$zone_3_target < min_coverage[3],
        min_coverage[3],  # Ensure zone_3 target is at least the minimum coverage
        input$zone_3_target
      ),
      zone_4_target = ifelse(
        input$zone_4_target < min_coverage[4],
        min_coverage[4],  # Ensure zone_4 target is at least the minimum coverage
        input$zone_4_target
      )
    )
  } else {
    # Create a tibble with adjusted targets for only three zones (no green zone)
    targets <- tibble::tibble(
      zone_1_target = ifelse(
        input$zone_1_target < min_coverage[1],
        min_coverage[1],  # Ensure zone_1 target is at least the minimum coverage
        input$zone_1_target
      ),
      zone_2_target = ifelse(
        input$zone_2_target < min_coverage[2],
        min_coverage[2],  # Ensure zone_2 target is at least the minimum coverage
        input$zone_2_target
      ),
      zone_3_target = ifelse(
        input$zone_3_target < min_coverage[3],
        min_coverage[3],  # Ensure zone_3 target is at least the minimum coverage
        input$zone_3_target
      )
    )
  }

  log_message("Adjusted targets: zone_1={targets$zone_1_target}, zone_2={targets$zone_2_target}, zone_3={targets$zone_3_target}")

  # Return the tibble with the adjusted targets
  return(targets)
}

#' Calculate Target Area from Percentage
#'
#' Calculates a target value by taking a percentage of the total area
#' represented by the planning units (PU). The result is rounded to the nearest
#' whole number, and if the calculated target is zero, a small value (1e-4) is
#' returned to avoid issues with zero targets in optimization.
#'
#' @param pu A `SpatRaster` representing the planning units, where the total
#'   area is calculated.
#' @param target A numeric value representing the percentage target (e.g., 10
#'   for 10%).
#'
#' @return A numeric value representing the target area. If the calculated
#'   target is zero, returns a very small value (1e-4) to ensure a non-zero
#'   target.
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate target for 10% of planning units
#' get_target(pu = my_planning_units_raster, target = 10)
#' }
get_target <- function(pu = NULL, target = NULL) {
  # Input validation
  assertthat::assert_that(
    inherits(pu, "SpatRaster"),
    msg = "'pu' must be a SpatRaster object."
  )

  assertthat::assert_that(
    is.numeric(target) && target >= 0 && target <= 100,
    msg = "'target' must be a numeric value between 0 and 100."
  )

  # Calculate the total area of the planning units and determine the target based on the given percentage.
  # 'terra::global' is used to calculate the total sum of the values in the PU raster (ignoring NA values).
  # The target is calculated as the specified percentage of this total.
  tar <- round(terra::global(pu, "sum", na.rm = TRUE) / 100 * target, 0)$sum

  # If the calculated target is zero, return a small value (1e-4) to avoid zero targets in the optimization process.
  # Otherwise, return the calculated target value.
  result <- ifelse(tar == 0, 1e-4, tar)
  log_message("Calculated target: {result} ({target}% of total PU area)")
  return(result)
}

#' Calculate restore and urban green zone budgets as percentages
#'
#' This function calculates the budget for **restore** and **urban green** zones
#' as a percentage of the total planning unit (PU) area for a specified country.
#'
#' The restore zone typically represents degraded areas, and the urban green zone
#' typically represents mapped urban areas. Each budget is calculated as a fixed
#' fraction (default 30%) of the coverage of its respective zone over the PU layer.
#'
#' This function assumes input rasters are co-aligned and represent valid binary zone masks.
#'
#' @param iso3 A 3-letter ISO3 country code (e.g., `"MWI"`). Used only for reference.
#' @param workspace_dir Path to the directory containing all raster inputs (not hardcoded by ISO3).
#' @param coverage_fraction Fraction of zone coverage to use as budget (default: `0.3`).
#' @param pu_raster Filename of the planning unit raster (within `workspace_dir`). Default: `"planning_units.tif"`.
#' @param restore_zone_raster Filename of the restore zone raster (within `workspace_dir`). Default: `"restore_zone_v1.tif"`.
#' @param urban_areas_raster Filename of the urban areas raster (within `workspace_dir`). Default: `"urban_areas.tif"`.
#'
#' @return A named numeric vector with two elements: `restore_budget_pct` and `urban_green_budget_pct`,
#'         each representing the budget as a percentage of the total PU area.
#'
#' @examples
#' calculate_restore_and_urban_budgets("MWI", workspace_dir = "/path/to/my-data/az_uploads")
#'
#' @export
calculate_restore_and_urban_budgets <- function(iso3,
                                                workspace_dir = NULL,
                                                coverage_fraction = 0.3,
                                                pu_raster = "planning_units.tif",
                                                restore_zone_raster = "restore_zone_v1.tif",
                                                urban_areas_raster = "urban_areas.tif") {
  # Input validation
  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )

  assertthat::assert_that(
    !is.null(workspace_dir) && dir.exists(workspace_dir),
    msg = "'workspace_dir' must be an existing directory path."
  )

  assertthat::assert_that(
    is.numeric(coverage_fraction) && coverage_fraction > 0 && coverage_fraction <= 1,
    msg = "'coverage_fraction' must be a numeric value between 0 and 1."
  )

  iso3 <- toupper(iso3)
  log_message("Calculating restore and urban green budgets for {iso3}...")

  # Load raster layers
  log_message("Loading raster layers from {workspace_dir}...")
  pus <- terra::rast(file.path(workspace_dir, pu_raster))
  rest_zone <- terra::rast(file.path(workspace_dir, restore_zone_raster))
  green_zone <- terra::rast(file.path(workspace_dir, urban_areas_raster))

  # Calculate zone coverages
  log_message("Calculating zone coverages with {coverage_fraction * 100}% coverage fraction...")
  restore_pct <- elsar::get_coverage(zone_layer = rest_zone, pu_layer = pus) * coverage_fraction
  green_pct <- elsar::get_coverage(zone_layer = green_zone, pu_layer = pus) * coverage_fraction

  result <- round(c(restore_budget_pct = restore_pct, urban_green_budget_pct = green_pct), 2)
  log_message("Budget results: restore={result['restore_budget_pct']}%, urban_green={result['urban_green_budget_pct']}%")

  # Return rounded named vector
  return(result)
}

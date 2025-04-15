#' Get Minimum Lock-in Targets for Zones
#'
#' This function calculates the minimum lock-in targets for different zones (e.g., protect, restore, manage, and optionally green).
#' It compares the input targets with the minimum coverage needed for each zone and ensures that the target is not set below this threshold.
#'
#' @param lockin A raster or list of rasters representing the locked-in areas for each zone.
#' @param input A list or dataframe containing the target values for each zone (e.g., `zone_1_target`, `zone_2_target`, etc.).
#' @param pu A raster layer representing the planning unit used for calculating coverage.
#' @return A `tibble` containing the adjusted target values for each zone.
#' @export
get_elsa_min_lockin_target <- function(lockin, input, pu) {

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

  # Return the tibble with the adjusted targets
  return(targets)
}

#' Calculate the Percentage Area Coverage for a Given Zone
#'
#' This function calculates the percentage of the total area covered by a specific zone (e.g., protected areas, managed areas)
#' relative to the entire planning unit (PU) layer. It returns the areal coverage as a percentage.
#'
#' The function uses `terra::global()` to calculate the sum of values in both the zone and PU layers,
#' ignoring missing values (`NA`s), and computes the percentage of the zone's total area relative to the PU's total area.
#'
#' @param zone_layer A `terra` raster object representing the specific zone (e.g., protected or managed areas) for which
#'        the coverage percentage will be calculated.
#' @param pu_layer A `terra` raster object representing the total planning units (PU) layer that defines the entire area of interest.
#'
#' @return A numeric value representing the percentage coverage of the `zone_layer` relative to the `pu_layer`.
#'
#' @seealso [terra::global()] which is used to sum the values in the raster layers.
#' @export
#' @examples
#' \dontrun{
#' # Calculate coverage for protected areas
#' get_coverage(PA, pu)
#' # Calculate coverage for protected zones
#' get_coverage(zone_protect, pu)
#' # Calculate coverage for managed zones
#' get_coverage(zone_manage, pu)
#' }
get_elsa_coverage <- function(zone_layer, pu_layer) {

  # Calculate the total area (sum of all values) in the `zone_layer`, ignoring NA values
  zone_sum <- terra::global(zone_layer, sum, na.rm = TRUE)$sum

  # Calculate the total area (sum of all values) in the `pu_layer`, ignoring NA values
  pu_sum <- terra::global(pu_layer, sum, na.rm = TRUE)$sum

  # Return the percentage coverage of the `zone_layer` relative to the `pu_layer`
  (zone_sum / pu_sum) * 100
}

#' Get country-specific target value based on the total area of planning units (PU)
#'
#' This function calculates a target value by taking a percentage of the total area
#' represented by the planning units (PU). The result is rounded to the nearest whole number,
#' and if the calculated target is zero, a small value (1e-4) is returned to avoid issues with zero targets.
#'
#' @param PU A `terra` raster or object representing the planning units, where the total area is calculated.
#' @param target A numeric value representing the percentage target (e.g., 10 for 10%).
#'
#' @return A numeric value representing the target area. If the calculated target is zero,
#' the function returns a very small value (1e-4) to ensure a non-zero target.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with a raster of planning units and a target of 10%
#' get_target(PU = my_planning_units_raster, target = 10)
#' }
get_elsa_target <- function(PU = NULL, target = NULL) {

  # Calculate the total area of the planning units and determine the target based on the given percentage.
  # 'terra::global' is used to calculate the total sum of the values in the PU raster (ignoring NA values).
  # The target is calculated as the specified percentage of this total.
  tar <- round(terra::global(PU, "sum", na.rm = TRUE) / 100 * target, 0)$sum

  # If the calculated target is zero, return a small value (1e-4) to avoid zero targets in the optimization process.
  # Otherwise, return the calculated target value.
  ifelse(tar == 0, 1e-4, tar)
}

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
  iso3 <- toupper(iso3)
  if (is.null(workspace_dir)) {
    stop("Please provide a workspace_dir containing the required rasters.")
  }
  
  # Load raster layers
  pus <- terra::rast(file.path(workspace_dir, pu_raster))
  rest_zone <- terra::rast(file.path(workspace_dir, restore_zone_raster))
  green_zone <- terra::rast(file.path(workspace_dir, urban_areas_raster))
  
  # Calculate zone coverages
  restore_pct <- elsar::get_coverage(zone_layer = rest_zone, pu_layer = pus) * coverage_fraction
  green_pct <- elsar::get_coverage(zone_layer = green_zone, pu_layer = pus) * coverage_fraction
  
  # Return rounded named vector
  round(c(restore_budget_pct = restore_pct, urban_green_budget_pct = green_pct), 2)
}

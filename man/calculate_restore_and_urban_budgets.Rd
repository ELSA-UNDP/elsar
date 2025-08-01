% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-elsa.R
\name{calculate_restore_and_urban_budgets}
\alias{calculate_restore_and_urban_budgets}
\title{Calculate restore and urban green zone budgets as percentages}
\usage{
calculate_restore_and_urban_budgets(
  iso3,
  workspace_dir = NULL,
  coverage_fraction = 0.3,
  pu_raster = "planning_units.tif",
  restore_zone_raster = "restore_zone_v1.tif",
  urban_areas_raster = "urban_areas.tif"
)
}
\arguments{
\item{iso3}{A 3-letter ISO3 country code (e.g., \code{"MWI"}). Used only for reference.}

\item{workspace_dir}{Path to the directory containing all raster inputs (not hardcoded by ISO3).}

\item{coverage_fraction}{Fraction of zone coverage to use as budget (default: \code{0.3}).}

\item{pu_raster}{Filename of the planning unit raster (within \code{workspace_dir}). Default: \code{"planning_units.tif"}.}

\item{restore_zone_raster}{Filename of the restore zone raster (within \code{workspace_dir}). Default: \code{"restore_zone_v1.tif"}.}

\item{urban_areas_raster}{Filename of the urban areas raster (within \code{workspace_dir}). Default: \code{"urban_areas.tif"}.}
}
\value{
A named numeric vector with two elements: \code{restore_budget_pct} and \code{urban_green_budget_pct},
each representing the budget as a percentage of the total PU area.
}
\description{
This function calculates the budget for \strong{restore} and \strong{urban green} zones
as a percentage of the total planning unit (PU) area for a specified country.
}
\details{
The restore zone typically represents degraded areas, and the urban green zone
typically represents mapped urban areas. Each budget is calculated as a fixed
fraction (default 30\%) of the coverage of its respective zone over the PU layer.

This function assumes input rasters are co-aligned and represent valid binary zone masks.
}
\examples{
calculate_restore_and_urban_budgets("MWI", workspace_dir = "/path/to/my-data/az_uploads")

}

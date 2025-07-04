% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_planning_units.R
\name{make_planning_units}
\alias{make_planning_units}
\title{Create a Planning Units Raster}
\usage{
make_planning_units(
  boundary_proj,
  pu_size = NULL,
  pu_threshold = 850000,
  pu_tolerance = 0.05,
  limit_to_mainland = FALSE,
  iso3,
  background_fill = NA,
  output_path = NULL
)
}
\arguments{
\item{boundary_proj}{A projected \code{sf} object representing the boundary of the planning region.
Must be in a projected CRS with linear units (typically meters). Use \code{\link[=make_boundary]{make_boundary()}} to generate this.}

\item{pu_size}{Optional numeric. Length in meters of each square planning unit. If \code{NULL}, the function
estimates a size that keeps the total number of PUs under \code{pu_threshold}.}

\item{pu_threshold}{Numeric. Maximum number of planning units allowed (default is 850,000).
A soft threshold of \code{pu_threshold * (1 + pu_tolerance)} is used during optimization.}

\item{pu_tolerance}{Numeric. Fractional tolerance (default is \code{0.05}, or 5\%) above \code{pu_threshold} allowed
when estimating PU size automatically.}

\item{limit_to_mainland}{Logical. Reserved for future use. If \code{TRUE}, limits planning units to mainland regions only.}

\item{iso3}{Character. ISO3 country code used to name the output raster (e.g., "KEN", "BRA").}

\item{background_fill}{The value to apply to all pixels outside the boundary_proj. This default to NA (e.g., nodata).
You should have a good reason for wanting to use a different value.}

\item{output_path}{Optional character. Directory path to save the resulting raster. If \code{NULL}, the raster is not saved.}
}
\value{
A single-layer \code{SpatRaster} object from the \code{terra} package. All non-zero cells represent valid planning units.
}
\description{
Generates a planning units (PUs) raster for spatial prioritization using a user-defined boundary.
If \code{pu_size} is not provided, the function automatically estimates an appropriate resolution (in meters)
that keeps the number of planning units under the specified threshold (\code{pu_threshold}), while allowing a configurable
tolerance (default 5\%). The finest resolution meeting this constraint is selected using adaptive increments
and clean rounding steps. The result can optionally be saved as a Cloud Optimized GeoTIFF (.tif).
}
\examples{
# Automatically estimate PU size to stay under 850,000 PUs
boundary_proj <- make_boundary(boundary_in = boundary_data, iso3 = "ZMB", iso3_column = "iso3cd")
pu_raster <- make_planning_units(boundary_proj, iso3 = "ZMB")

# Use a fixed PU size (e.g., 250 meters)
pu_raster <- make_planning_units(boundary_proj, pu_size = 250, iso3 = "ZMB")
}

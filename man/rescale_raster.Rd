% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rescale_raster}
\alias{rescale_raster}
\title{Rescale Raster to 0-1 Range}
\usage{
rescale_raster(
  raster_in,
  raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min,
  raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max,
  new_min = 0,
  new_max = 1
)
}
\arguments{
\item{raster_in}{Input \code{SpatRaster} to be rescaled.}

\item{raster_in_min}{Optional numeric. Minimum value of input raster. If NULL, will be calculated.}

\item{raster_in_max}{Optional numeric. Maximum value of input raster. If NULL, will be calculated.}

\item{new_min}{Numeric. New minimum of rescaled raster (default = 0).}

\item{new_max}{Numeric. New maximum of rescaled raster (default = 1).}
}
\value{
A \code{SpatRaster} with values rescaled to the specified range.
}
\description{
This function rescales values in a raster to a range between 0 and 1.
}

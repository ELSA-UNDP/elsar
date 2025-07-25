% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_kbas.R
\name{make_kbas}
\alias{make_kbas}
\title{Create a Standardised Raster of Key Biodiversity Areas (KBAs)}
\usage{
make_kbas(
  kba_in,
  pus,
  iso3,
  include_aze_sites = FALSE,
  aze_only = FALSE,
  include_regional_kba = FALSE,
  buffer_points = TRUE,
  area_column = "repareakm2",
  nQuadSegs = 50,
  output_path = NULL
)
}
\arguments{
\item{kba_in}{An \code{sf} object containing KBA vector features, including columns like \code{iso3}, \code{azestatus}, and \code{kbaclass}.}

\item{pus}{A \code{SpatRaster} object representing planning units (reference extent and resolution).}

\item{iso3}{A character string representing the 3-letter ISO country code (e.g., "KEN").}

\item{include_aze_sites}{Logical. If \code{TRUE}, includes KBAs that are also AZE sites (default is \code{FALSE}).}

\item{aze_only}{Logical. If \code{TRUE}, returns only confirmed AZE sites (default is \code{FALSE}).}

\item{include_regional_kba}{Logical. If \code{FALSE}, filters out KBAs marked as "Regional" or "Global/ Regional to be determined".}

\item{buffer_points}{Logical. If \code{TRUE}, circular buffers are generated around point geometries using the \code{area_column} attribute (default is \code{TRUE}).}

\item{area_column}{A string indicating the name of the column containing site area (in hectares), used when buffering point geometries.}

\item{nQuadSegs}{An integer specifying the number of segments used to approximate circular buffers (default: 50).}

\item{output_path}{Optional character. Directory path to save the output raster. If \code{NULL}, output is not written to file.}
}
\value{
A \code{SpatRaster} object with normalised values representing KBA (or AZE) coverage across planning units.
If \code{output_path} is provided, the raster is also written to disk.
}
\description{
This function processes a Key Biodiversity Areas (KBA) vector dataset and converts it into
a normalised raster aligned with the input planning units. It allows for filtering to
include or exclude Alliance for Zero Extinction (AZE) sites, as well as optionally excluding
KBAs marked as "Regional". By default, it includes all KBAs except regional-only sites.
It can also be used to return only AZE sites if \code{aze_only = TRUE}.
}
\examples{
\dontrun{
kba_raster <- make_kbas(
  kba_in = kba_sf,
  pus = planning_units,
  iso3 = "KEN",
  aze_only = TRUE,
  output_path = "outputs"
)
}
}

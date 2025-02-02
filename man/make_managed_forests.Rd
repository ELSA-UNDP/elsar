% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_managed_forests.R
\name{make_managed_forests}
\alias{make_managed_forests}
\title{Create managed forests data layer}
\usage{
make_managed_forests(
  raster_in,
  pus,
  iso3,
  manual_cats = NULL,
  include_disturbed_forest = FALSE,
  name_out,
  output_path = NULL
)
}
\arguments{
\item{raster_in}{A \code{SpatRaster} file that contains the data to be put into right format}

\item{pus}{A \code{SpatRaster} file that contains the reference spatial extent, crs etc.in form of the planning units}

\item{iso3}{A string of the iso3 name of the data (country name)}

\item{manual_cats}{If file does not contain default categories (11 (not managed), 20 (disturbed forests; can be interpreted as managed), 31, 32, 40, 53 (all managed)), allows to provide a vector with the right values.}

\item{include_disturbed_forest}{logical. Whether or not to include disturbed forests as managed forests (default is FALSE).}

\item{name_out}{A string with the data name that will be used for the output \code{tif}file}

\item{output_path}{An optional output path for the created file.}
}
\value{
A \code{SpatRaster} file of managed forests that has been aligned and normalised
}
\description{
Create managed forests data layer
}
\examples{
\dontrun{
boundary_proj <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  do_project = TRUE
)

pus <- make_planning_units(
  boundary_proj = boundary_proj,
  pu_size = NULL,
  pu_threshold = 8.5e5,
  limit_to_mainland = FALSE
)

managed_forests <- make_managed_forests(
  raster_in = forests_in,
  pus = pus
)
}
}

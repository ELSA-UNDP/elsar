% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_helpers.R
\name{elsar_download_gee_layer}
\alias{elsar_download_gee_layer}
\title{Download and Process a GEE Raster Layer into a Cloud-Optimized GeoTIFF}
\usage{
elsar_download_gee_layer(
  boundary_layer,
  iso3,
  output_dir = here::here(),
  asset_id,
  file_prefix,
  scale,
  datatype = "INT1U",
  gee_project = gee_project,
  googledrive_folder = "gee_exports",
  wait_time = 5
)
}
\arguments{
\item{boundary_layer}{An \code{sf} object defining the spatial boundary of interest.}

\item{iso3}{Three-letter ISO country code used to generate the export filename.}

\item{output_dir}{Path to directory for saving the final raster file.}

\item{asset_id}{Earth Engine ImageCollection asset ID (e.g., "projects/...").}

\item{file_prefix}{Prefix for export filename and GEE task description.}

\item{scale}{Resolution of the exported image in meters.}

\item{datatype}{Output datatype (GDAL style), e.g., "INT1U" or "FLT4S".}

\item{gee_project}{Earth Engine Cloud project ID.}

\item{googledrive_folder}{Name of Google Drive folder to check and export to.}

\item{wait_time}{Max time (in minutes) to wait for Drive export to appear.}
}
\value{
A \code{SpatRaster} object written to disk.
}
\description{
This function handles downloading raster data from Earth Engine. It checks if the
file already exists in a specified Google Drive folder. If not, it checks for
running Earth Engine export tasks. If neither exist, it launches a new export
based on the most recent available year, waits for it to appear in Drive,
downloads the tiles, and merges them into a single COG.
}
\details{
It uses exponential backoff to retry the download check up to a given wait time
(in minutes), sleeping 30 seconds between attempts.
}
\keyword{internal}

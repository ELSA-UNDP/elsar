% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_helpers.R
\name{download_gee_layer}
\alias{download_gee_layer}
\title{Download and Process a GEE Raster Layer into a Cloud-Optimized GeoTIFF}
\usage{
download_gee_layer(
  boundary_layer,
  iso3,
  output_dir = here::here(),
  asset_id,
  file_prefix,
  scale = 10,
  datatype = "INT1U",
  gee_project = "unbl-misc",
  googledrive_folder = NULL,
  wait_time = 5
)
}
\arguments{
\item{boundary_layer}{An \code{sf} object defining the spatial boundary of interest}

\item{iso3}{Character. Three-letter ISO country code for filename generation}

\item{output_dir}{Character. Path to directory for saving the final raster file.
Defaults to project root via \code{here::here()}}

\item{asset_id}{Character. Earth Engine ImageCollection asset ID (e.g., "projects/...")}

\item{file_prefix}{Character. Prefix for export filename and GEE task description}

\item{scale}{Numeric. Resolution of the exported image in meters. Default is 10}

\item{datatype}{Character. Output datatype (GDAL style), e.g., "INT1U" or "FLT4S"}

\item{gee_project}{Character. Earth Engine Cloud project ID. Default is "unbl-misc"}

\item{googledrive_folder}{Character. Google Drive folder name. Currently exports
to Drive root (NULL) to avoid GEE folder duplication bug}

\item{wait_time}{Numeric. Max time (in minutes) to wait for Drive export to appear}
}
\value{
A \code{SpatRaster} object written to disk, or NULL if export failed
}
\description{
Generic function for downloading any raster data from Google Earth Engine.
Handles authentication, export management, file downloading, and processing
into a local Cloud-Optimized GeoTIFF. Exports to Google Drive root to avoid
GEE folder duplication bugs.
}
\examples{
\dontrun{
# Download ESRI LULC data for Ghana
lulc <- download_gee_layer(
  boundary_layer = ghana_boundary,
  iso3 = "GHA",
  asset_id = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
  file_prefix = "esri_10m_lulc",
  scale = 10,
  datatype = "INT1U"
)
}
}
\keyword{internal}

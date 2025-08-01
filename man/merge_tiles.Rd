% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_helpers.R
\name{merge_tiles}
\alias{merge_tiles}
\title{Merge downloaded GEE tiles into a single Cloud-Optimized GeoTIFF}
\usage{
merge_tiles(local_path, output_file, datatype)
}
\arguments{
\item{local_path}{Character. Path to directory containing downloaded tile files}

\item{output_file}{Character. Full path for the output merged COG file}

\item{datatype}{Character. GDAL datatype for output raster (e.g., "INT1U", "FLT4S")}
}
\value{
A \code{SpatRaster} object of the merged and written COG
}
\description{
Takes multiple downloaded GeoTIFF tiles and merges them into a single
Cloud-Optimized GeoTIFF (COG) with appropriate compression and optimization.
}
\examples{
\dontrun{
merged_raster <- merge_tiles("/tmp/tiles", "/output/merged.tif", "INT1U")
}
}
\keyword{internal}

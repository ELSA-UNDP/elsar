% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_file_type}
\alias{get_file_type}
\title{Infer file type from file path or directory}
\usage{
get_file_type(path)
}
\arguments{
\item{path}{Character. A file path or directory path to inspect.}
}
\value{
A character string indicating the file type (e.g., \code{"shp"}, \code{"gpkg"}, \code{"tif"}).
}
\description{
This utility function takes a path (to a file or folder) and returns the detected geospatial file type,
such as \code{"shp"}, \code{"gpkg"}, \code{"tif"}, etc. It checks extensions for files and scans directory contents
for recognizable formats (e.g., \code{.shp}, \code{.gdb}).
}
\examples{
get_file_type("data/boundaries.shp")     # returns "shp"
get_file_type("data/vector_layers.gpkg") # returns "gpkg"
get_file_type("data/rasters")            # scans folder for known formats
}

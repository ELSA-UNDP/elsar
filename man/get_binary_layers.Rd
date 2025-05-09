% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-pipeline.R
\name{get_binary_layers}
\alias{get_binary_layers}
\title{Extract binary layers within a raster stack}
\usage{
get_binary_layers(raster_stack)
}
\arguments{
\item{raster_stack}{A \code{terra} \code{SpatRaster} containing several raster layers, some of which can be binary.}
}
\value{
Two \code{terra} \code{SpatRaster} raster stacks, one containing only the binary layers, and one containing the float layers
}
\description{
This function extracts all layers in a raster stack that have either c(0,1) or only 1 or only 0.
This is needed to split the raster stack saved from the pipeline into two raster stacks, one int and one float.
}
\examples{
\dontrun{
raster_out <- get_binary_layers(raster_stack = stack)

int_stack <- raster_out[[1]]
float_stack <- raster_out[[2]]
}
}

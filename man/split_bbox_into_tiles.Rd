% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{split_bbox_into_tiles}
\alias{split_bbox_into_tiles}
\title{Split a Bounding Box into a Regular Grid of Polygon Tiles}
\usage{
split_bbox_into_tiles(bbox_sf, ncols = 2, nrows = 2)
}
\arguments{
\item{bbox_sf}{An \code{sf} object. Only the bounding box is used; geometries inside are ignored.}

\item{ncols}{Integer. Number of columns to split the bounding box into. Default is 2.}

\item{nrows}{Integer. Number of rows to split the bounding box into. Default is 2.}
}
\value{
An \code{sf} object consisting of rectangular polygons covering the bounding box of the input.
Each polygon represents a tile in the grid. All geometries are valid.
}
\description{
Given an input \code{sf} object, this function extracts its bounding box and
divides it into a regular grid of rectangular polygon tiles using a specified
number of columns and rows.
}
\details{
This is useful for spatially chunking large geometries to speed up
operations like intersection or cropping, especially when used with spatial
indexing.
}
\examples{
\dontrun{
  world <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  tiles <- split_bbox_into_tiles(world, ncols = 3, nrows = 3)
  plot(st_geometry(world))
  plot(st_geometry(tiles), add = TRUE, border = "red")
}

}

# Split a Bounding Box into a Regular Grid of Polygon Tiles

Given an input `sf` object, this function extracts its bounding box and
divides it into a regular grid of rectangular polygon tiles using a
specified number of columns and rows.

## Usage

``` r
split_bbox_into_tiles(bbox_sf, ncols = 2, nrows = 2)
```

## Arguments

- bbox_sf:

  An `sf` object. Only the bounding box is used; geometries inside are
  ignored.

- ncols:

  Integer. Number of columns to split the bounding box into. Default is
  2.

- nrows:

  Integer. Number of rows to split the bounding box into. Default is 2.

## Value

An `sf` object consisting of rectangular polygons covering the bounding
box of the input. Each polygon represents a tile in the grid. All
geometries are valid.

## Details

This is useful for spatially chunking large geometries to speed up
operations like intersection or cropping, especially when used with
spatial indexing.

## Examples

``` r
if (FALSE) { # \dontrun{
  world <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  tiles <- split_bbox_into_tiles(world, ncols = 3, nrows = 3)
  plot(st_geometry(world))
  plot(st_geometry(tiles), add = TRUE, border = "red")
} # }
```

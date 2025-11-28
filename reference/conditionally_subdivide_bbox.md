# Conditionally Subdivide Bounding Box into Grid Tiles Based on Geographic Extent

This function checks whether the geographic extent of an `sf` object's
bounding box exceeds a specified threshold in degrees (longitude or
latitude). If so, it splits the bounding box into a regular grid of
rectangular tiles of approximately `tile_size_deg` degrees in size. If
not, it simply returns the original input.

## Usage

``` r
conditionally_subdivide_bbox(bbox_sf, degree_threshold = 10, tile_size_deg = 5)
```

## Arguments

- bbox_sf:

  An `sf` object. Only the bounding box is used for subdivision logic.

- degree_threshold:

  Numeric. Threshold in degrees of lat/lon span at which to trigger
  subdivision. Default is 10.

- tile_size_deg:

  Numeric. Approximate tile width/height in degrees. Default is 5.

## Value

An `sf` object. Either:

- A grid of tiles (rectangular polygons) if subdivision is triggered, or

- The original input `bbox_sf` unchanged.

## Details

This is useful to improve performance when running spatial operations
(e.g., `intersect`) on large countries or regions by breaking the work
into spatial chunks that benefit from spatial indexing.

## Examples

``` r
if (FALSE) { # \dontrun{
  world <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  tiles <- conditionally_subdivide_bbox(world)
  plot(st_geometry(world))
  plot(st_geometry(tiles), add = TRUE, border = "red")
} # }
```

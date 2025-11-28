# Convert Points to Buffered Polygons Based on Area

This function converts POINT or MULTIPOINT geometries into circular
polygons using the specified area attribute to calculate a radius
(assuming the area is in hectares or another square unit). The buffer is
created in a projected CRS (default: Mollweide, EPSG:54009) and then
transformed back to the input CRS. Optionally, original polygon features
can be retained and combined with the buffered features.

## Usage

``` r
convert_points_polygon(
  sf_layer,
  area_attr = "REP_AREA",
  area_crs = "ESRI:54009",
  nQuadSegs = 50,
  append_original_polygons = TRUE,
  area_multiplier = 1e+06
)
```

## Arguments

- sf_layer:

  An `sf` object containing geometries, including points or multipoints
  and an area column.

- area_attr:

  Character. Name of the attribute column that contains site area
  (default: `"REP_AREA"`).

- area_crs:

  Character. CRS used for buffering operation (default: `"ESRI:54009"` =
  World Mollweide).

- nQuadSegs:

  Integer. Number of segments per circle quadrant for buffering
  (default: `50`).

- append_original_polygons:

  Logical. If `TRUE`, appends original polygons to buffered features
  (default: `TRUE`).

- area_multiplier:

  Numeric. Multiplier applied to the area attribute to convert units
  (e.g., `1e4` for hectares to m2).

## Value

An `sf` object containing polygon features (either buffered points,
original polygons, or both). The `sf` object can be empty. If no valid
features are found, returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
buffered <- convert_points_polygon(
  sf_layer = my_kba_layer,
  area_attr = "gisarea",
  area_crs = "ESRI:54009",
  append_original_polygons = TRUE
)
} # }
```

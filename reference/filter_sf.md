# Load and optionally filter a single vector layer

This utility function wraps
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
to load a vector dataset (e.g., from GPKG, GDB, or SHP). It supports
optional filtering by ISO3 country code and/or a spatial filter in WKT
format. It automatically selects the first available layer if
`layer_name` is not provided and the file type supports layers.

## Usage

``` r
filter_sf(
  file_path,
  iso3 = NULL,
  iso3_column = NULL,
  layer_name = NULL,
  drop3d = TRUE,
  wkt_filter = NULL,
  file_type = NULL
)
```

## Arguments

- file_path:

  Character. Full file path to the vector dataset.

- iso3:

  Character or NULL. ISO3 country code used for attribute filtering.

- iso3_column:

  Character or NULL. Column in the dataset to match `iso3`.

- layer_name:

  Character or NULL. Optional. Specific layer name for multi-layer
  formats.

- drop3d:

  Logical. If TRUE, drops Z and M dimensions from geometries.

- wkt_filter:

  Character or NULL. Optional WKT string used for spatial filtering.

- file_type:

  Character or NULL. File type used to determine if layer name inference
  is required.

## Value

An `sf` object with valid geometry, or `NULL` on failure.

## Examples

``` r
if (FALSE) { # \dontrun{
filter_sf("data/layers.gpkg", iso3 = "KEN", iso3_column = "ISO3")
filter_sf("data/boundary.shp", wkt_filter = "POLYGON((...))")
} # }
```

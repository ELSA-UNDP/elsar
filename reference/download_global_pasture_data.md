# Download Global Pasture Watch Grassland Probability Layer

Downloads either the cultivated or natural/semi-natural grassland
probability layer from the Global Pasture Watch dataset hosted in Earth
Engine. Returns the result as a local Cloud-Optimized GeoTIFF with
appropriate layer naming.

## Usage

``` r
download_global_pasture_data(
  boundary_layer,
  iso3,
  gee_project,
  output_dir = here::here(),
  layer_type = c("cultivated", "natural"),
  ...
)
```

## Arguments

- boundary_layer:

  An `sf` object defining the spatial boundary of interest.

- iso3:

  Character. Three-letter ISO country code for filename generation.

- gee_project:

  Character. Google Earth Engine cloud project ID. This is required and
  must be a valid GEE project you have access to. Find your project ID
  in the [GEE Code Editor](https://code.earthengine.google.com/).

- output_dir:

  Character. Local output directory. Defaults to project root via
  [`here::here()`](https://here.r-lib.org/reference/here.html)

- layer_type:

  Character. One of "cultivated" (default) or "natural" to specify which
  grassland probability layer to download

## Value

A `SpatRaster` object of the downloaded grassland data, or NULL if
download failed

## Examples

``` r
if (FALSE) { # \dontrun{
# Download cultivated grassland data for Nepal
pasture <- download_global_pasture_data(
  boundary_layer = nepal_boundary,
  iso3 = "NPL",
  layer_type = "cultivated",
  gee_project = "my-gee-project"
)
} # }
```

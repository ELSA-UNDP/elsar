# Download the ESRI 10m Land Use/Land Cover Time Series (LULC) Layer

Retrieves the ESRI Global LULC Time Series at 10m resolution from Earth
Engine. Downloads the most recent year available and returns the result
as a local Cloud-Optimized GeoTIFF with appropriate layer naming.

## Usage

``` r
download_esri_lulc_data(
  boundary_layer,
  iso3,
  gee_project,
  output_dir = here::here(),
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

## Value

A `SpatRaster` object of the downloaded LULC data, or NULL if download
failed

## Examples

``` r
if (FALSE) { # \dontrun{
# Download LULC data for Ghana
lulc <- download_esri_lulc_data(
  boundary_layer = ghana_boundary,
  iso3 = "GHA",
  gee_project = "my-gee-project"
)
} # }
```

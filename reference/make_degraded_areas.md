# Create degraded areas layer for restoration planning

This function generates a degraded areas layer based on multiple inputs,
including agriculture, built-up areas, human influence index (HII), and
productivity decline layers. The output is a raster identifying degraded
areas according to set thresholds.

## Usage

``` r
make_degraded_areas(
  country_iso,
  pu,
  sdg_degradation_input = NULL,
  hii_input = NULL,
  agriculture_input = NULL,
  built_areas_input = NULL,
  output_path = NULL,
  hii_threshold = 4,
  lulc_threshold = 0.1
)
```

## Arguments

- country_iso:

  ISO3 country code (e.g., "CHL" for Chile).

- pu:

  Planning units raster (SpatRaster).

- sdg_degradation_input:

  Productivity degradation layer (SpatRaster).

- hii_input:

  Human Influence Index (HII) raster (SpatRaster).

- agriculture_input:

  Agriculture layer (SpatRaster).

- built_areas_input:

  Built-up areas layer (SpatRaster).

- output_path:

  Directory to save output rasters. If NULL, output is not saved.

- hii_threshold:

  Threshold for the Human Influence Index (default: 4).

- lulc_threshold:

  Threshold for built and agriculture layers (default: 0.1).

## Value

A SpatRaster object containing the degraded areas layer.

## Examples

``` r
restore_zone <- make_degraded_areas(
  "CHL",
  pu,
  sdg_degradation_input,
  hii_input,
  agriculture_input,
  built_areas_input,
  output_path = "./output"
  )
#> Error: object 'sdg_degradation_input' not found
```

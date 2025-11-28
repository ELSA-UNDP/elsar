# Create Threatened Ecosystems (for Restoration) Raster Based on Intactness and Degradation

This function identifies areas that are both ecologically threatened and
degraded, representing priority targets for ecological restoration. It
filters the threat values from `threatened_ecosystems_input` to retain
only those pixels that overlap with degraded areas indicated in
`degradation_input`. The output is a continuous raster with threat
values masked to degraded zones.

## Usage

``` r
make_threatened_ecosystems_restoration(
  iso3,
  pus,
  threatened_ecosystems_input = NULL,
  degradation_input = NULL,
  output_path = NULL
)
```

## Arguments

- iso3:

  Character. ISO3 country code (e.g., "KEN") used for naming and
  processing.

- pus:

  SpatRaster. Planning units raster used for resolution and extent.

- threatened_ecosystems_input:

  SpatRaster. Raster from `make_threatened_ecosystems()` containing
  ecosystem threat values.

- degradation_input:

  SpatRaster. Raster from
  [`make_restore_zone()`](https://elsa-undp.github.io/elsar/reference/make_restore_zone.md)
  indicating degraded areas.

- output_path:

  Character or NULL. Optional output directory to save the resulting
  raster.

## Value

A SpatRaster containing threat values masked to degraded areas only.

## Examples

``` r
if (FALSE) { # \dontrun{
threatened_restoration <- make_threatened_ecosystems_restoration(
  iso3 = "KEN",
  pus = planning_units,
  threatened_ecosystems_input = rast("outputs/threatened_ecosystems_for_protection_KEN.tif"),
  degradation_input = rast("outputs/restore_zones_KEN.tif"),
  output_path = "outputs"
)
} # }
```

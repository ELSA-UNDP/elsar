# Create a Threatened Ecosystems Raster Based on Integrity or Degradation

This function calculates ecosystem-level threat scores based on the
proportion of degraded or low-integrity land within each ecosystem
polygon. It supports either an Ecological Intactness Index (EII) style
continuous integrity raster (thresholded using the global median value)
or a binary degraded areas raster.

## Usage

``` r
make_threatened_ecosystems_protection(
  ecosystems_sf,
  group_attribute,
  pus,
  boundary_layer,
  integrity_raster,
  integrity_type = c("eii", "degraded"),
  iso3 = NULL,
  output_path = NULL
)
```

## Arguments

- ecosystems_sf:

  sf. Polygons representing ecosystems.

- group_attribute:

  character. Column name used to group/dissolve ecosystems (e.g.,
  `"eco_id"` or `"get_id"`).

- pus:

  SpatRaster. Planning unit raster used for alignment and rasterization.

- boundary_layer:

  sf. National boundary used to calculate the median integrity value if
  `integrity_type = "eii"`.

- integrity_raster:

  SpatRaster. Raster representing either continuous integrity (e.g.,
  EII) or binary degraded areas.

- integrity_type:

  character. One of `"eii"` (default) or `"degraded"`, indicating how to
  interpret the integrity raster.

- iso3:

  character, optional. ISO3 code used for naming output files (e.g.,
  `"KEN"`).

- output_path:

  character, optional. Directory to write the output raster as a
  GeoTIFF.

## Value

A `SpatRaster` with one layer: `"threatened_ecosystems"`, with values
from 0 to 100. Higher values indicate greater ecosystem degradation or
threat.

## Details

The threat score is calculated as the percentage of each ecosystem area
that is below the integrity threshold (i.e., not intact), and rasterized
by planning unit.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using EII (Ecological Integrity Index)
threatened_raster_eii <- make_threatened_ecosystems_protection(
  ecosystems_sf = my_ecosystems,
  group_attribute = "eco_id",
  pus = planning_units,
  boundary_layer = national_boundary,
  integrity_raster = eii_raster,
  integrity_type = "eii",
  iso3 = "KEN",
  output_path = "outputs"
)

# Using binary degraded areas raster (1 = degraded, 0 = intact)
threatened_raster_degraded <- make_threatened_ecosystems_protection(
  ecosystems_sf = my_ecosystems,
  group_attribute = "eco_id",
  pus = planning_units,
  boundary_layer = national_boundary,
  integrity_raster = degraded_raster,
  integrity_type = "degraded",
  iso3 = "KEN"
)
} # }
```

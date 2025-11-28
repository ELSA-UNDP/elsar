# Calculate Areal Coverage of a Zone Relative to Planning Units

Calculates percentage of a zone layer relative to the total area of
planning units.

## Usage

``` r
get_coverage(zone_layer, pu_layer)
```

## Arguments

- zone_layer:

  A binary `SpatRaster` representing a target zone.

- pu_layer:

  A binary `SpatRaster` representing planning units.

## Value

Numeric. Percentage of zone coverage across PUs.

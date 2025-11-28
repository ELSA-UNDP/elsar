# Get Minimum Lock-in Targets for Zones

Calculates the minimum lock-in targets for different zones (e.g.,
protect, restore, manage, and optionally green). Compares input targets
with the minimum coverage needed for each zone and ensures targets are
not set below this threshold.

## Usage

``` r
get_min_lockin_target(lockin, input, pu)
```

## Arguments

- lockin:

  A raster or list of rasters representing locked-in areas for each
  zone.

- input:

  A list or dataframe containing target values for each zone (e.g.,
  `zone_1_target`, `zone_2_target`, etc.).

- pu:

  A raster layer representing the planning unit used for calculating
  coverage.

## Value

A `tibble` containing the adjusted target values for each zone.

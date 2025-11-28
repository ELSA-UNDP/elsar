# Calculate Target Area from Percentage

Calculates a target value by taking a percentage of the total area
represented by the planning units (PU). The result is rounded to the
nearest whole number, and if the calculated target is zero, a small
value (1e-4) is returned to avoid issues with zero targets in
optimization.

## Usage

``` r
get_target(pu = NULL, target = NULL)
```

## Arguments

- pu:

  A `SpatRaster` representing the planning units, where the total area
  is calculated.

- target:

  A numeric value representing the percentage target (e.g., 10 for 10%).

## Value

A numeric value representing the target area. If the calculated target
is zero, returns a very small value (1e-4) to ensure a non-zero target.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate target for 10% of planning units
get_target(pu = my_planning_units_raster, target = 10)
} # }
```

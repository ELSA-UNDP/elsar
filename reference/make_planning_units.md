# Create a Planning Units Raster

Generates a planning units (PUs) raster for spatial prioritization using
a user-defined boundary. If `pu_size` is not provided, the function
automatically estimates an appropriate resolution (in meters) that keeps
the number of planning units under the specified threshold
(`pu_threshold`), while allowing a configurable tolerance (default 5%).
The finest resolution meeting this constraint is selected using adaptive
increments and clean rounding steps. The result can optionally be saved
as a Cloud Optimized GeoTIFF (.tif).

## Usage

``` r
make_planning_units(
  boundary_proj,
  pu_size = NULL,
  pu_threshold = 850000,
  pu_tolerance = 0.05,
  limit_to_mainland = FALSE,
  iso3,
  background_fill = NA,
  output_path = NULL
)
```

## Arguments

- boundary_proj:

  A projected `sf` object representing the boundary of the planning
  region. Must be in a projected CRS with linear units (typically
  meters). Use
  [`make_boundary()`](https://elsa-undp.github.io/elsar/reference/make_boundary.md)
  to generate this.

- pu_size:

  Optional numeric. Length in meters of each square planning unit. If
  `NULL`, the function estimates a size that keeps the total number of
  PUs under `pu_threshold`.

- pu_threshold:

  Numeric. Maximum number of planning units allowed (default is
  850,000). A soft threshold of `pu_threshold * (1 + pu_tolerance)` is
  used during optimization.

- pu_tolerance:

  Numeric. Fractional tolerance (default is `0.05`, or 5%) above
  `pu_threshold` allowed when estimating PU size automatically.

- limit_to_mainland:

  Logical. Reserved for future use. If `TRUE`, limits planning units to
  mainland regions only.

- iso3:

  Character. ISO3 country code used to name the output raster (e.g.,
  "KEN", "BRA").

- background_fill:

  The value to apply to all pixels outside the boundary_proj. This
  default to NA (e.g., nodata). You should have a good reason for
  wanting to use a different value.

- output_path:

  Optional character. Directory path to save the resulting raster. If
  `NULL`, the raster is not saved.

## Value

A single-layer `SpatRaster` object from the `terra` package. All
non-zero cells represent valid planning units.

## Examples

``` r
# Automatically estimate PU size to stay under 850,000 PUs
boundary_proj <- make_boundary(boundary_in = boundary_data, iso3 = "ZMB", iso3_column = "iso3cd")
#> Error: object 'boundary_data' not found
pu_raster <- make_planning_units(boundary_proj, iso3 = "ZMB")
#> [2025-11-28 05:57] pu_size not provided: estimating size to target <= 850000 PUs (allowing 5% tolerance).
#> Error: object 'boundary_proj' not found

# Use a fixed PU size (e.g., 250 meters)
pu_raster <- make_planning_units(boundary_proj, pu_size = 250, iso3 = "ZMB")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'ext': error in evaluating the argument 'x' in selecting a method for function 'ext': object 'boundary_proj' not found
```

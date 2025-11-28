# Function to extend the spatial extend around a raster file for plotting

When plotting (e.g. with
[`elsar_plot_static_raster_c()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_static_raster_c.md),
the extent of the main data will be used. `elsar_extend()` allows to
extract background data to be plotted with an extent that is a bit
greater than the extent of the main data.

## Usage

``` r
elsar_extend(raster_main = NULL, raster_to_crop = NULL, extend_by = 0.05)
```

## Arguments

- raster_main:

  A `SpatRaster` file of the main data of the original plot. The extent
  of this file will be used as the baseline for the new plot extent.

- raster_to_crop:

  A `SpatRaster` file of the data that the will be cropped using the new
  extent.

- extend_by:

  A numerical value that allows to extend the background beyond the
  extent of `raster_in`. If extend_background \<= 1, the lat and lon
  extend will be extended by the ratio provided (e.g. 0.05 will extend
  it by 5%). If extend_background \> 1 all sides will be extended by the
  absolute value provided.

## Value

A `SpatRaster` file with the new dimensions.

## Examples

``` r
boundary_proj <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd"
)

pus <- make_planning_units(
  boundary_proj = boundary_proj,
  pu_size = NULL,
  pu_threshold = 8.5e5,
  limit_to_mainland = FALSE
)
#> [2025-11-28 06:05] pu_size not provided: estimating size to target <= 850000 PUs (allowing 5% tolerance).
#> [2025-11-28 06:05] Iteration 1: 741036 PUs at resolution 450 m
#> [2025-11-28 06:05] Iteration 2: 830662 PUs at resolution 425 m
#> [2025-11-28 06:05] Iteration 3: 937494 PUs at resolution 400 m
#> [2025-11-28 06:05] Exceeded soft threshold (892500); using best previous result.
#> [2025-11-28 06:05] Final PU layer: 830662 PUs at 425 m resolution.
wad_dat <- get_wad_data()

wadOut <- make_normalised_raster(
  raster_in = wad_dat,
  pus = pus,
  iso3 = "NPL"
)
wad_dat <- terra::project(wad_dat, terra::crs(wadOut))

bckgrnd_dat <- elsar_extend(
  raster_main = wadOut,
  raster_to_crop = wad_dat,
  extend_by = 0.05
)
#> Extend based on ratio.
```

# Plot a single feature raster within planning unit outlines

Creates a ggplot2 map of a single feature represented by a `SpatRaster`,
with optional planning unit outlines, customizable color palette
direction, and optional saving to disk.

## Usage

``` r
elsar_plot_feature(
  raster_in,
  pus,
  legend_title = NULL,
  color_map = "viridis",
  invert_palette = FALSE,
  figure_path = NULL,
  iso3 = NULL,
  no_legend = FALSE,
  custom_resolution = 200
)
```

## Arguments

- raster_in:

  SpatRaster. The raster to be plotted.

- pus:

  SpatVector. Planning units used as an outline for the raster.

- legend_title:

  character, optional. Title for the legend. Defaults to NULL.

- color_map:

  character. Name of the viridis palette to use. Defaults to "viridis".

- invert_palette:

  logical. If TRUE, reverses the palette direction. Defaults to FALSE.

- figure_path:

  character, optional. If provided, path to save the figure as a PNG.

- iso3:

  iso3 code of the country of interest.

- no_legend:

  logical. If TRUE, suppresses the legend. Defaults to FALSE.

- custom_resolution:

  A value that is used in ggsave() for dpi. Default is 200.

## Value

A `ggplot` object displaying the raster with optional planning unit
outlines.

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
#> [2025-11-28 05:56] pu_size not provided: estimating size to target <= 850000 PUs (allowing 5% tolerance).
#> [2025-11-28 05:56] Iteration 1: 741036 PUs at resolution 450 m
#> [2025-11-28 05:56] Iteration 2: 830662 PUs at resolution 425 m
#> [2025-11-28 05:56] Iteration 3: 937494 PUs at resolution 400 m
#> [2025-11-28 05:56] Exceeded soft threshold (892500); using best previous result.
#> [2025-11-28 05:56] Final PU layer: 830662 PUs at 425 m resolution.

wad_dat <- get_wad_data()

wadOut <- make_normalised_raster(
  raster_in = wad_dat,
  pus = pus,
  iso3 = "NPL"
)

elsar_plot_feature(
  raster_in = wadOut,
  pus = pus,
  legend_title = "WAD",
  color_map = "magma",
  invert_palette = TRUE,
  figure_path = here::here(),
  no_legend = FALSE,
  iso3 = "test",
  custom_resolution = 400
)
#> <SpatRaster> resampled to 500507 cells.
```

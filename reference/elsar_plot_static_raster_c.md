# Function to create a static plot from raster data with continuous data

`elsar_plot_static_raster_c()` allows to plot continuous data from a
`SpatRaster` in `ggplot` or `tmap`. It can be combined with
[`elsar_plot_optics()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_optics.md)
and
[`elsar_plot_extra_data()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_extra_data.md)
to create reproducible plots.

## Usage

``` r
elsar_plot_static_raster_c(
  raster_in,
  type = "ggplot_vector",
  background = NULL,
  extend_background = 0.05,
  custom_palette = NULL,
  plot_title = "",
  legend_title = NULL,
  color_map = "viridis",
  expand_plot = FALSE,
  raster_df_out = FALSE
)
```

## Arguments

- raster_in:

  The `SpatRaster` file to be plotted.

- type:

  A character of the plot type. Either "ggplot_vector", "ggplot_raster"
  or "tmap".

- background:

  Requires a `SpatRaster` input (preferably with the same data as
  `raster_in`) to plot the background data.

- extend_background:

  A numerical value that allows to extent the background beyond the
  extent of `raster_in`. If extend_background \<= 1, the lat and lon
  extend will be extended by the ratio provided (e.g. 0.05 will extend
  it by 5%). If extend_background \> 1 all sides will be extended by the
  absolute value provided.

- custom_palette:

  An optional custom palette for plotting. Default uses the `viridis`
  package.

- plot_title:

  An optional plot title.

- legend_title:

  An optional legend title.

- color_map:

  The name of the `viridis` palette to be used. Default is "viridis".

- expand_plot:

  Logical. Whether to expand the plot, so there is no space between
  border and plot.

- raster_df_out:

  Logical. Whether to return only the plot (FALSE) or also the data
  frame behind the plot (TRUE). Needed for
  [`elsar_plot_distribution()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_distribution.md).

## Value

A `ggplot` or `tmap` object.

## Details

This is a convenience wrapper around
[`elsar_plot_static_raster()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_static_raster.md)
with `data_type = "continuous"`.

## See also

[`elsar_plot_static_raster()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_static_raster.md)
for the unified plotting function

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
#> [2025-11-28 05:57] pu_size not provided: estimating size to target <= 850000 PUs (allowing 5% tolerance).
#> [2025-11-28 05:57] Iteration 1: 741036 PUs at resolution 450 m
#> [2025-11-28 05:57] Iteration 2: 830662 PUs at resolution 425 m
#> [2025-11-28 05:57] Iteration 3: 937494 PUs at resolution 400 m
#> [2025-11-28 05:57] Exceeded soft threshold (892500); using best previous result.
#> [2025-11-28 05:57] Final PU layer: 830662 PUs at 425 m resolution.
wad_dat <- get_wad_data()

wadOut <- make_normalised_raster(
  raster_in = wad_dat,
  pus = pus,
  iso3 = "NPL"
)
(wad_plot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
))
#> Adding background layer.
#> Extend based on ratio.
```

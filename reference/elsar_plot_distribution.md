# Function to create basic histogram with categorical data

Function to create basic histogram with categorical data

## Usage

``` r
elsar_plot_distribution(
  type = "ggplot",
  categorical = TRUE,
  raster_df = NULL,
  col_interest = NULL,
  bin_number = 10,
  custom_palette = NULL,
  show_legend = FALSE,
  color_map = "viridis"
)
```

## Arguments

- type:

  A character denoting whether "ggplot" or "tmap" is being used.

- categorical:

  logical. Whether data is categorical (TRUE) or continuous (FALSE).

- raster_df:

  A `df` object of the data to include in the histogram.

- col_interest:

  A character of the column of interest. Uses "interval" if nothing is
  provided.

- bin_number:

  An integer value of how many bins to split the data into (default =
  10).

- custom_palette:

  An optional custom palette for plotting. Default uses the `viridis`
  package.

- show_legend:

  logical. Whether to include legend in plot or not. Default is FALSE.

- color_map:

  The name of the `viridis` palette to be used. Default is "viridis".

## Value

A `list` of a `ggplot` or `tmap` object and a `SpatRaster` with the new
background data.

## Examples

``` r
boundary_proj <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  custom_projection = TRUE
)

pus <- make_planning_units(boundary_proj = boundary_proj,
                           pu_size = NULL,
                           pu_threshold = 8.5e5,
                           limit_to_mainland = FALSE)
#> [2025-11-28 05:56] pu_size not provided: estimating size to target <= 850000 PUs (allowing 5% tolerance).
#> [2025-11-28 05:56] Iteration 1: 741036 PUs at resolution 450 m
#> [2025-11-28 05:56] Iteration 2: 830662 PUs at resolution 425 m
#> [2025-11-28 05:56] Iteration 3: 937494 PUs at resolution 400 m
#> [2025-11-28 05:56] Exceeded soft threshold (892500); using best previous result.
#> [2025-11-28 05:56] Final PU layer: 830662 PUs at 425 m resolution.
wad_dat <- get_wad_data()

wadOut <- make_normalised_raster(raster_in = wad_dat,
                                 pus = pus,
                                 iso3 = "NPL")

plot_cat <- elsar_plot_static_raster_d(
raster_in = wadOut, type = "ggplot_raster",
categorical = FALSE,
background = wad_dat,
number_categories = 10,
data_layer = "wad_final_cog",
raster_df_out = TRUE,
legend_title = "WAD"
)
#> Plotting input data that is continuous and will be split into categories.
#> Error in hist.default(raster_df[[data_layer]], breaks = seq(min(raster_df[[data_layer]]),     max(raster_df[[data_layer]]), length.out = number_categories +         1), plot = FALSE): 'x' must be numeric

plot_wad_cat <- plot_cat[[1]]
#> Error: object 'plot_cat' not found
raster_df_cat <- plot_cat[[2]]
#> Error: object 'plot_cat' not found


plot_dist_cat <- elsar_plot_distribution(
  raster_df = raster_df_cat,
  categorical = TRUE
)
#> Error: object 'raster_df_cat' not found

(plot_wad_cat_inset <- patchwork::wrap_plots((plot_wad_cat +
  elsar_plot_optics()) +
  patchwork::inset_element(
    (plot_dist_cat +
      elsar_plot_optics(include_text = FALSE) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
    ),
    left = 0.1, # needs to be changed depending on plot dimensions
    bottom = 0.08, # needs to be changed depending on plot dimensions
    right = 0.35, # needs to be changed depending on plot dimensions
    top = 0.4, # needs to be changed depending on plot dimensions
    align_to = "plot"
  )))
#> Error: object 'plot_wad_cat' not found
```

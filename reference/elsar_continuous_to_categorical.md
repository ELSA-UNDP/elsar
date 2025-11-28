# Function to create categories for plotting out of continuous data

Function to create categories for plotting out of continuous data

## Usage

``` r
elsar_continuous_to_categorical(
  raster_in,
  data_layer,
  number_categories,
  manual_breaks = NULL,
  hist_breaks_out = TRUE
)
```

## Arguments

- raster_in:

  The `SpatRaster` file to be plotted.

- data_layer:

  The data layer with continuous data to be converted into categories.

- number_categories:

  Number of categories to create from continuous data

- manual_breaks:

  A vector with breaks to be used as categories.

- hist_breaks_out:

  logical. If TRUE (default), returns the breaks used to categorise
  data.

## Value

A list with a `df` that has categories and their interval labels, as
well as the interval breaks for background data.

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

wad_cat <- elsar_continuous_to_categorical(
 wadOut,
 data_layer = "wad_final_cog",
 number_categories = 10
)
#> Error in hist.default(raster_df[[data_layer]], breaks = seq(min(raster_df[[data_layer]]),     max(raster_df[[data_layer]]), length.out = number_categories +         1), plot = FALSE): 'x' must be numeric
```

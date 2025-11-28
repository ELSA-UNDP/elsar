# Function to easily adapt the visual appearance of a plot

`elsar_plot_optics()` allows to use specific themes for plotting and add
north arrows and scales if wanted.

## Usage

``` r
elsar_plot_optics(
  type = "ggplot",
  theme = "default",
  include_north_scale = FALSE,
  include_logo = FALSE,
  logo_path = "man/figures/elsaR_hex_sticker.png",
  logo_dim = c(60, 60),
  logo_pos,
  include_text = TRUE,
  text_to_display = "UNBL | www.unbiodiversitylab.org",
  text_location = "bottom_right",
  move_horizontal = NULL,
  move_vertical = NULL
)
```

## Arguments

- type:

  A character denoting whether "ggplot" or "tmap" is being used. Needs
  to match the main plot

- theme:

  For `ggplot` this allows to use a pre-defined "deafult" plot theme or
  provide a custom one as a `list`.

- include_north_scale:

  Logical. Determines whether a north arrow and scale should be added.

- include_logo:

  Logical. Whether to include a logo (.png image) in the plot

- logo_path:

  A path to where the `png` file of the logo is saved (e.g.
  "man/figures/elsaR_hex_sticker.png")

- logo_dim:

  A vector with desired width and height of the image. Default is c(60,
  60).

- logo_pos:

  A vector of the desired position of the image (xmin, xmax, ymin,
  ymax).

- include_text:

  Logical. Whether to include text in the plot (e.g. website).

- text_to_display:

  The text to display. Dafult is "UNBL \| www.unbiodiversitylab.org",

- text_location:

  The location of the text. Options are "bottom_right" (default),
  "top_right", "top_left" and "bottom_left".

- move_horizontal:

  After setting a text location, this attribute allows to move the text
  block horizontally if needed.

- move_vertical:

  After setting a text location, this attribute allows to move the text
  block vertically if needed.

## Value

For `ggplot` returns a list that can simply be added to a `ggplot`, e.g.
[`elsar_plot_static_raster_c()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_static_raster_c.md).

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
wad_plot <- testPlot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
) + elsar_plot_optics()
#> Adding background layer.
#> Extend based on ratio.
```

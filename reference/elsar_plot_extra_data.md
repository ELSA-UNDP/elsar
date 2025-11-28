# Function to add additional data to an existing plot

`elsar_plot_extra_data()` allows to add extra data to an existing
`ggplot` or `tmap` plot, e.g. protected areas generated with
[`make_protected_areas()`](https://elsa-undp.github.io/elsar/reference/make_protected_areas.md).
Function idea taken from
[spatialplanr](https://mathmarecol.github.io/spatialplanr/) package.

## Usage

``` r
elsar_plot_extra_data(
  plot_type = "ggplot",
  include_pas = NULL,
  pas_look = "contours",
  alpha_pa = 0.5,
  color_pa = "black",
  legend_pa = "",
  label_pa = "PAs"
)
```

## Arguments

- plot_type:

  A character denoting whether "ggplot" or "tmap" is being used. Needs
  to match the main plot

- include_pas:

  An `sf` object with the PAs in the area, e.g. created with the
  [`make_protected_areas()`](https://elsa-undp.github.io/elsar/reference/make_protected_areas.md)
  function with the setting return_sf = TRUE.

- pas_look:

  A character denoting whether to show "contours" or "area" of PAs.

- alpha_pa:

  A value (0-1) for the opacity of the locked in areas when plotted on
  top of other plots.

- color_pa:

  A color value for the locked in areas.

- legend_pa:

  A character value for the title of the legend of the locked in areas.
  Can be empty ("").

- label_pa:

  The legend label of the locked in area (e.g. MPAs)

## Value

For `ggplot` returns a list that can simply be added to a `ggplot`, e.g.
[`elsar_plot_static_raster_c()`](https://elsa-undp.github.io/elsar/reference/elsar_plot_static_raster_c.md).

## Examples

``` r
if (FALSE) { # \dontrun{
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
wad_dat <- get_wad_data()

wadOut <- make_normalised_raster(
  raster_in = wad_dat,
  pus = pus,
  iso3 = "NPL"
)

current_pas <- make_protected_areas(
  iso3 = "NPL",
  download_path = here::here(),
  buffer_points = TRUE,
  return_sf = TRUE,
  pus = pus
)

wad_plot <- testPlot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
) +
  elsar_plot_extra_data(include_pas = current_pas, color_pa = "red") +
  elsar_plot_optics()

wad_plot <- testPlot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
) +
  elsar_plot_extra_data(include_pas = current_pas, pas_look = "area", color_pa = "grey") +
  elsar_plot_optics()
} # }
```

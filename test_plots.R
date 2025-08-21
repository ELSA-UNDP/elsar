

# Overview

## Introduction


## Get started

library(elsar)
library(prioritizr)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)

# set seed for reproducibility
set.seed(500)


## Planning Region

# create boundary for Nepal
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

(wad_plot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
))





ggplot() +
  geom_sf(data = boundary)


boundary_proj <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  do_project = TRUE
)

# create custom projection and apply it to boundary
wkt <- make_custom_projection(boundary = boundary, iso3 = "NPL")
boundary_proj2 <- sf::st_transform(boundary, crs = sf::st_crs(wkt))
```

We can test whether the two crs of the boundaries match.

```{r}
# test crs
print(sf::st_crs(boundary_proj) == sf::st_crs(boundary_proj2))
```

## Planning Units

For defining a conservation planning problem, we need to subdivide the planning region into smaller planning units which can either be selected or not in the downstream prioritization and which the feature information can be attributed to. The function has an option to add a desired planning unit size. If none is provided, the function aims to make planning units as small as possible whilst still being computationally efficient. The default for this is set to 850,000 planning units, which is based on previous experience with run time.

```{r}
# make planning units
pus <- make_planning_units(
  boundary_proj = boundary_proj,
  pu_size = NULL,
  pu_threshold = 8.5e5,
  limit_to_mainland = FALSE
)
```

## Features

Biodiversity features are an integral part of conservation planning. Features can range from species distributions, to more general over-arching datasets such as Key Biodiversity Areas (ref). In our example, we will use only one dataset, the World Atlas of Desertification (WAD). Again, input data can first be loaded using the  `elsar_load_data()` function (here we have the dataset already saved in the package; change later).

```{r}
# get feature data
wad_dat <- get_wad_data()
```

The data then needs to be re-projected and re-sampled to the previously created planning units. We also normalise the data to only have values between 0-1.

```{r, fig.width = w, fig.height = h}
# crop and normalise data
wadOut <- make_normalised_raster(
  raster_in = wad_dat,
  pus = pus,
  iso3 = "NPL"
)

terra::plot(wadOut)
```

Instead of only having a quick look at the data by using the `terra` package, we can also create a plot using the plotting options integrated in the `elsar` package, such as `elsar_plot_static_raster_c()`, which allows to plot continuous raster data with `ggplot` or `tmap`.

```{r, fig.width = 7, fig.height = 4}
(plot_wad <- elsar_plot_static_raster_c(
  raster_in = wadOut, type = "ggplot_raster",
  legend_title = "WAD"
))
```

We can also add background data to the plot and add additional properties to change the appearance of the plot by using the `elsar_plot_optics()` function that allows to specify a specifc `ggplot` them. We can also change the colour palette to be something other than the default "viridis".


custom_p <- ggplot2::scale_fill_distiller(
  name = "WAD",
  palette = "YlGnBu",
  aesthetics = c("fill"),
  oob = scales::squish
)

(wad_plot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  custom_palette = custom_p,
  background = wad_dat,
  legend_title = "wad"
) +
    elsar_plot_optics())

# NA values not transparent
custom_p <- ggplot2::scale_fill_distiller(
  name = "WAD",
  palette = "YlGnBu",
  aesthetics = c("fill"),
  oob = scales::squish,
  na.value = "transparent"
)

(wad_plot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  custom_palette = custom_p,
  background = wad_dat,
  legend_title = "wad"
) +
    elsar_plot_optics())

#topRight
x_min <- terra::ext(wadOut)[1][[1]]
x_max <- terra::ext(wadOut)[2][[1]]
y_min <- terra::ext(wadOut)[3][[1]]
y_max <- terra::ext(wadOut)[4][[1]]

img_xmax <- x_max + x_max * 0.09
img_xmin <- img_xmax * 0.8
img_ymax <- y_max + y_max * 0.102
img_ymin <- img_ymax * 0.8

img_pos <- c(img_xmin, img_xmax, img_ymin, img_ymax)

(wad_plot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
) +
    elsar_plot_optics(include_logo = TRUE,
                      logo_path = "vignettes/figures/elsaR_hex_sticker.png", #change to logo later
                      logo_pos = img_pos,
                      logo_dim = c(50, 60)))

plot_out <- elsar_plot_static_raster_c(
  raster_in = wadOut, type = "ggplot_raster",
  background = wad_dat,
  raster_df_out = TRUE,
  legend_title = "WAD",
  expand_plot = TRUE
)

plot_wad <- plot_out[[1]]
raster_df <- plot_out[[2]]

plot_dist <- elsar_plot_distribution(
  raster_df = raster_df,
  categorical = FALSE,
  col_interest = "Planning.Units"
)

(plot_wad_inset  <- patchwork::wrap_plots((plot_wad +
                                             elsar_plot_optics()) +
                                            patchwork::inset_element(
                                              (plot_dist +
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

```
We can also transform the data into categories and plot the main plot and the inset that way.

```{r, fig.width = 8, fig.height = 6}
plot_cat <- elsar_plot_static_raster_d(
  raster_in = wadOut, type = "ggplot_raster",
  categorical = FALSE,
  background = wad_dat,
  number_categories = 10,
  data_layer = "Planning.Units",
  raster_df_out = TRUE,
  legend_title = "WAD",
  expand_plot = TRUE
)

plot_wad_cat <- plot_cat[[1]]
raster_df_cat <- plot_cat[[2]]


plot_dist_cat <- elsar_plot_distribution(
  raster_df = raster_df_cat,
  categorical = TRUE
)

browser()

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
```



## Existing Protected Area

ADD TEXT LATER

```{r, fig.width = 7, fig.height = 5}
current_pas <- current_pas_sf

(plot_wad_pas <- elsar_plot_static_raster_c(
  raster_in = wadOut, type = "ggplot_raster",
  background = wad_dat,
  legend_title = "WAD",
  expand_plot = TRUE
) +
    elsar_plot_extra_data(include_pas = current_pas, color_pa = "red") +
    elsar_plot_optics())
```


```{r, fig.width = 7, fig.height = 5}
(plot_wad_pas2 <- elsar_plot_static_raster_c(
  raster_in = wadOut, type = "ggplot_raster",
  background = wad_dat,
  legend_title = "WAD",
  expand_plot = TRUE
) +
    elsar_plot_extra_data(include_pas = current_pas, pas_look = "area", color_pa = "grey") +
    elsar_plot_optics(include_north_scale = TRUE))


# plotting with vector
#### plot vector
raster_sf <- mangroves_hti

col_interest <- colnames(raster_sf)[[1]]

if (!is.null(background)) {
  plot_out <- gg_background +
    ggplot2::geom_sf(
      data = raster_sf,
      ggplot2::aes(colour = .data[[col_interest]]),
      size = 0.0001
    ) +
    ggplot2::coord_sf(
      xlim = c(min(background_dat$x), max(background_dat$x)),
      ylim = c(min(background_dat$y), max(background_dat$y))
    )
} else {
  plot_out <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = raster_sf,
      ggplot2::aes(colour = .data[[col_interest]]),
      size = 0.0001
    )
}
}

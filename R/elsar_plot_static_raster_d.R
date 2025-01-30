#' Function to create a static plot from raster data with discrete data
#'
#' `elsar_plot_static_raster_d()` allows to plot discrete data from a `SpatRaster` in `ggplot` or `tmap`. It can be combined with [elsar_plot_optics()] and [elsar_plot_extra_data()] to create reproducible plots.
#'
#' @param raster_in The `SpatRaster` file to be plotted.
#' @param type A character of the plot type. Either "ggplot_vector", "ggplot_raster" or "tmap".
#' @param categorical logical. if data is categorical (TRUE), convert to factor (if not yet) and use the number of categories given.
#' @param number_categories If data does not have pre-defined categories, how many categories to split the continuous data into
#' @param data_layer A character of the data layer used.
#' @param background Requires a `SpatRaster` input (preferably with the same data as `raster_in`) to plot the background data.
#' @param extend_background A numerical value that allows to extent the background beyond the extent of `raster_in`. If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%). If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param plot_title An optional plot title.
#' @param legend_title An optional legend title.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param expand_plot logical. Whether to expand the plot, so there is not space betwen border and plot.
#' @param raster_df_out logical. Whether to return only the plot (FALSE) or also the data frame behind the plot (TRUE). Needed for `elsar_plot_distribution()`
#'
#' @return Either a `ggplot` object or a list with the `ggplot` and the `df` created.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#'
#' (plot_cat <- elsar_plot_static_raster_d(raster_in = wadOut,
#' type = "ggplot_raster",
#' categorical = FALSE,
#' number_categories = 10,
#' background = wad_dat,
#' data_layer = "wad_final_cog",
#' raster_df_out = TRUE)
#' )
elsar_plot_static_raster_d <- function(raster_in,
                                       type = "ggplot_vector", # "ggplot_vector" or ggplot_raster or tmap
                                       categorical = FALSE, # if data is categorical, convert to factor (if not yet) and use the number of categories given (if not yet)
                                       number_categories = 10,
                                       data_layer = NULL,
                                       background = NULL,
                                       extend_background = 0.05,
                                       custom_palette = NULL, # "YlGnBu",
                                       plot_title = "",
                                       legend_title = NULL,
                                       color_map = "viridis",
                                       expand_plot = FALSE,
                                       raster_df_out = FALSE) {
  # Assertions
  assertthat::assert_that(
    inherits(raster_in, "SpatRaster"),
    msg = "Input must be `SpatRaster`"
  )

  if (categorical) {
    message("Plotting input data that is already categorical.")

    assertthat::assert_that(
      terra::is.factor(raster_in),
      msg = "Input is not a factor."
    )

    # get number of categories
    number_categories <- length(terra::levels(raster_in)[[1]][[1]])

    raster_df <- as.data.frame(raster_in, xy = TRUE) %>%
      stats::na.omit() %>%
      dplyr::rename(interval = .data[[data_layer]]) %>%
      dplyr::mutate(interval = as.factor(.data$interval))
  } else {
    message("Plotting input data that is continuous and will be split into categories.")

    assertthat::assert_that(
      !is.null(number_categories),
      msg = "Provide a valid number of categories to split your data into."
    )

    raster_cat <- elsar_continuous_to_categorical(
      raster_in = raster_in,
      data_layer = data_layer,
      number_categories = number_categories
    )

    # hist_breaks <- raster_cat[[2]] #currently not needed, but might be needed later

    raster_df <- raster_cat[[1]] %>%
      dplyr::mutate(
        category = as.factor(.data$category),
        interval = as.factor(.data$interval)
      )
  }

  if (grepl("[ggplot]", type)) {
    # create background plot
    if (inherits(background, "SpatRaster")) {
      data_layer <- terra::names(background)

      gg_background <- elsar_plot_background_d(
        background_dat = background,
        main_data = raster_in,
        increase_extend = extend_background,
        number_categories = number_categories,
        data_layer = data_layer
      )

      background_dat <- gg_background[[2]]
      gg_background <- gg_background[[1]]
    }

    if (grepl("[raster]", type)) {
      if (!is.null(background)) {
        plot_out <- gg_background +
          ggplot2::geom_tile(data = raster_df, ggplot2::aes(
            y = .data$y, x = .data$x,
            fill = .data$interval
          )) +
          ggplot2::coord_sf(
            xlim = c(min(background_dat$x), max(background_dat$x)),
            ylim = c(min(background_dat$y), max(background_dat$y))
          )
      } else {
        plot_out <- ggplot2::ggplot() +
          ggplot2::geom_tile(data = raster_df, ggplot2::aes(
            y = .data$y, x = .data$x,
            fill = .data$interval
          ))
      }
    } else if (grepl("[vector]", type)) {
      raster_sf <- raster_df %>%
        sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(raster_in)) %>%
        sf::st_make_valid()

      if (!is.null(background)) {
        plot_out <- gg_background +
          ggplot2::geom_sf(
            data = raster_sf,
            ggplot2::aes(colour = .data$interval),
            size = 0.0001
          ) +
          ggplot2::coord_sf(
            xlim = c(min(background_dat$x), max(background_dat$x)),
            ylim = c(min(background_dat$y), max(background_dat$y)),
            expand = TRUE
          ) +
          ggplot2::scale_x_continuous(expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = c(0, 0))
      } else {
        plot_out <- ggplot2::ggplot() +
          ggplot2::geom_sf(
            data = raster_sf,
            ggplot2::aes(colour = .data$interval),
            size = 0.0001
          )
      }
    }

    if (is.null(custom_palette)) {
      plot_out <- plot_out +
        ggplot2::scale_colour_viridis_d(
          name = legend_title,
          option = color_map,
          expand = c(0, 0)
        ) +
        ggplot2::scale_fill_viridis_d(
          name = legend_title,
          option = color_map,
          expand = c(0, 0)
        )
    } else {
      plot_out <- plot_out + custom_palette
    }

    if (expand_plot) {
      plot_out <- plot_out +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0))
    }
  } else if (type == "tmap") {
    message("TBA")
  }

  if (raster_df_out) {
    return(list(plot_out, raster_df))
  } else {
    return(plot_out)
  }
}

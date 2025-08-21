#' Plot vector
#'
#' @param sf_in An`sf` object containing the data to be plotted.
#' @param data_type Whether the data is discrete ("d") or continuous ("c")
#' @param col_interest A list of column names to include in the plot. If specified, only these columns will be used to colour the plot.
#' @param background Requires a `SpatRaster` input (preferably with the same data as `raster_in`) to plot the background data.
#' @param extend_background A numerical value that allows to extent the background beyond the extent of `raster_in`. If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%). If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param plot_title An optional plot title.
#' @param legend_title An optional legend title.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param expand_plot logical. Whether to expand the plot, so there is not space betwen border and plot.
#' @param legend_labels A vector of strings containing the labels to use for legend values.
#'
#' @return A ggplot object.
#'
#' @export
elsar_plot_static_vector <- function(sf_in,
                                     data_type = "d",
                                     col_interest = NULL,
                                     custom_palette = NULL,
                                     color_map = "viridis",
                                     background = NULL,
                                     extend_background = 0.05,
                                     plot_title = "",
                                     legend_title = "",
                                     legend_labels = NULL,
                                     expand_plot = FALSE) {
  # Assertions
  assertthat::assert_that(
    inherits(sf_in, "sf"),
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(sf::st_bbox(sf_in))),
    data_type == "d" | data_type == "D" | data_type == "c" | data_type == "C"
  )

  if (data_type == "D" | data_type == "d") {
    if (!is.factor(sf_in[[col_interest]])) {
      sf_in[[col_interest]] <- as.factor(sf_in[[col_interest]])
    }
  }

  if (is.null(col_interest)) {
    col_interest <- colnames(sf_in)[[1]]
    message(paste0("Column name used is ", col_interest, ". This is because the first column is used as default when no other column names are provided."))
  }

  if (!is.null(background)) {
    plot_out <- gg_background +
      ggplot2::geom_sf(
        data = sf_in,
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
        data = sf_in,
        ggplot2::aes(colour = .data[[col_interest]], fill = .data[[col_interest]]),
        size = 0.0001
      )
  }

  # plot
  if (data_type == "D" | data_type == "d") {
    if (is.null(custom_palette)) {
      plot_out <- plot_out +
        ggplot2::scale_colour_viridis_d(
          name = legend_title,
          option = color_map,
          labels = legend_labels,
          expand = c(0, 0)
        ) +
        ggplot2::scale_fill_viridis_d(
          name = legend_title,
          option = color_map,
          labels = legend_labels,
          expand = c(0, 0)
        )
    } else {
      plot_out <- plot_out + custom_palette
    }
  } else if (data_type == "C" | data_type == "c") {
    if (is.null(custom_palette)) {
      plot_out <- plot_out +
        ggplot2::scale_colour_viridis_c(
          name = legend_title,
          option = color_map,
          expand = c(0, 0)
        ) +
        ggplot2::scale_fill_viridis_c(
          name = legend_title,
          option = color_map,
          expand = c(0, 0)
        )
    } else {
      plot_out <- plot_out + custom_palette
    }
  }

  if (expand_plot) {
    plot_out <- plot_out +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0))
  }

  return(plot_out)
}

#' Function to create a static plot from raster data with continuous data
#'
#' `elsar_plot_static_raster_c()` allows to plot continuous data from a `SpatRaster` in `ggplot` or `tmap`. It can be combined with [elsar_plot_optics()] and [elsar_plot_extra_data()] to create reproducible plots.
#'
#' @param raster_in The `SpatRaster` file to be plotted.
#' @param type A character of the plot type. Either "ggplot_vector", "ggplot_raster" or "tmap".
#' @param background Requires a `SpatRaster` input (preferably with the same data as `raster_in`) to plot the background data.
#' @param extend_background A numerical value that allows to extent the background beyond the extent of `raster_in`. If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%). If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param plot_title An optional plot title.
#' @param legend_title An optional legend title.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param expand_plot logical. Whether to expand the plot, so there is not space betwen border and plot.
#' @param raster_df_out logical. Whether to return only the plot (FALSE) or also the data frame behind the plot (TRUE). Needed for `elsar_plot_distribution()`
#'
#' @return A `ggplot` or `tmap` object.
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
#' (wad_plot <- elsar_plot_static_raster_c(
#'   raster_in = wadOut,
#'   type = "ggplot_raster",
#'   background = wad_dat,
#'   legend_title = "wad"
#' ))
elsar_plot_static_raster_c <- function(raster_in,
                                       type = "ggplot_vector", # "ggplot_vector" or ggplot_raster or tmap
                                       # tmap_interactive = FALSE, #when tmap: tmap::tmap_mode("view") #or tmap::tmap_mode("plot")
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

  # if (!is.null(ggplot_extras)) {
  #   assertthat::assert_that(is.list(ggplot_extras),
  #     msg = "ggplot_extras either needs to be NULL (uses the default settings) or a list of ggplot objects."
  #   )
  # }

  assertthat::assert_that(
    ((grepl("[ggplot]", type)) || (type == "tmap")),
    msg = "Mapping a raster is only supported in tmap or ggplot."
  )

  if (grepl("[ggplot]", type)) {
    # create background plot
    if (inherits(background, "SpatRaster")) {
      gg_background <- elsar_plot_background_c(
        background_dat = background,
        main_data = raster_in,
        increase_extend = extend_background
      )
      background_dat <- gg_background[[2]]
      gg_background <- gg_background[[1]]
    }

    if (grepl("[raster]", type)) {
      raster_df <- as.data.frame(raster_in, xy = TRUE) %>%
        stats::na.omit()

      col_interest <- terra::names(raster_in)

      if (!is.null(background)) {
        plot_out <- gg_background +
          ggplot2::geom_tile(data = raster_df, ggplot2::aes(
            y = .data$y, x = .data$x,
            fill = .data[[col_interest]]
          )) +
          ggplot2::coord_sf(
            xlim = c(min(background_dat$x), max(background_dat$x)),
            ylim = c(min(background_dat$y), max(background_dat$y))
          )
      } else {
        plot_out <- ggplot2::ggplot() +
          ggplot2::geom_tile(data = raster_df, ggplot2::aes(
            y = .data$y, x = .data$x,
            fill = .data[[col_interest]]
          ))
      }
    } else if (grepl("[vector]", type)) {
      raster_df <- as.data.frame(raster_in, xy = TRUE) %>%
        stats::na.omit()

      raster_sf <- raster_df %>%
        sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(raster_in)) %>%
        sf::st_make_valid()

      col_interest <- terra::names(raster_in)

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

    if (is.null(custom_palette)) {
      plot_out <- plot_out +
        ggplot2::scale_colour_viridis_c(
          name = legend_title,
          option = color_map
        ) +
        ggplot2::scale_fill_viridis_c(
          name = legend_title,
          option = color_map
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



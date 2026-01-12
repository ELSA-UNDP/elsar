#' Function to create a static plot from raster data
#'
#' `elsar_plot_static_raster()` allows plotting both continuous and discrete data from a `SpatRaster`
#' in `ggplot` or `tmap`. It can be combined with [elsar_plot_optics()] and [elsar_plot_extra_data()]
#' to create reproducible plots.
#'
#' @param raster_in The `SpatRaster` file to be plotted.
#' @param data_type Character. Either "continuous" (default) or "discrete" to specify how data should be displayed.
#' @param type A character of the plot type. Either "ggplot_vector", "ggplot_raster" or "tmap".
#' @param categorical Logical. Only used when `data_type = "discrete"`. If TRUE, data is already categorical.
#' @param number_categories Integer. Only used when `data_type = "discrete"`. Number of categories to split continuous data into. Default is 10.
#' @param data_layer Character. Only used when `data_type = "discrete"`. The data layer name for categorization.
#' @param background Requires a `SpatRaster` input (preferably with the same data as `raster_in`) to plot the background data.
#' @param extend_background A numerical value that allows to extent the background beyond the extent of `raster_in`.
#'   If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%).
#'   If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param plot_title An optional plot title.
#' @param legend_title An optional legend title.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param expand_plot Logical. Whether to expand the plot, so there is no space between border and plot.
#' @param raster_df_out Logical. Whether to return only the plot (FALSE) or also the data frame behind the plot (TRUE).
#'   Needed for `elsar_plot_distribution()`.
#'
#' @return A `ggplot` or `tmap` object, or a list with the plot and the data frame if `raster_df_out = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   iso3 = "NPL",
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
#' # Continuous data plot
#' (wad_plot <- elsar_plot_static_raster(
#'   raster_in = wadOut,
#'   type = "ggplot_raster",
#'   background = wad_dat,
#'   legend_title = "wad"
#' ))
#'
#' # Discrete data plot
#' (wad_plot_d <- elsar_plot_static_raster(
#'   raster_in = wadOut,
#'   data_type = "discrete",
#'   type = "ggplot_raster",
#'   number_categories = 10,
#'   background = wad_dat,
#'   data_layer = "wad_final_cog"
#' ))
#' }
elsar_plot_static_raster <- function(raster_in,
                                     data_type = c("continuous", "discrete"),
                                     type = "ggplot_vector",
                                     categorical = FALSE,
                                     number_categories = 10,
                                     data_layer = NULL,
                                     background = NULL,
                                     extend_background = 0.05,
                                     custom_palette = NULL,
                                     plot_title = "",
                                     legend_title = NULL,
                                     color_map = "viridis",
                                     expand_plot = FALSE,
                                     raster_df_out = FALSE) {
  data_type <- match.arg(data_type)

  # Input validation
  assertthat::assert_that(
    inherits(raster_in, "SpatRaster"),
    msg = "'raster_in' must be a SpatRaster object."
  )

  assertthat::assert_that(
    grepl("ggplot", type) || type == "tmap",
    msg = "'type' must be 'ggplot_vector', 'ggplot_raster', or 'tmap'."
  )

  assertthat::assert_that(
    is.logical(categorical),
    msg = "'categorical' must be TRUE or FALSE."
  )

  assertthat::assert_that(
    is.numeric(number_categories) && number_categories > 0,
    msg = "'number_categories' must be a positive integer."
  )

  assertthat::assert_that(
    is.logical(expand_plot),
    msg = "'expand_plot' must be TRUE or FALSE."
  )

  assertthat::assert_that(
    is.logical(raster_df_out),
    msg = "'raster_df_out' must be TRUE or FALSE."
  )

  if (!is.null(background)) {
    assertthat::assert_that(
      inherits(background, "SpatRaster"),
      msg = "'background' must be a SpatRaster object when provided."
    )
  }

  log_message("Creating static raster plot ({data_type}, {type})...")

  # Process discrete data
  if (data_type == "discrete") {
    if (categorical) {
      log_message("Processing categorical data...")

      assertthat::assert_that(
        terra::is.factor(raster_in),
        msg = "'raster_in' must be a factor raster when categorical = TRUE."
      )

      number_categories <- length(terra::levels(raster_in)[[1]][[1]])

      raster_df <- as.data.frame(raster_in, xy = TRUE) %>%
        stats::na.omit() %>%
        dplyr::rename(interval = .data[[data_layer]]) %>%
        dplyr::mutate(interval = as.factor(.data$interval))
    } else {
      log_message("Converting continuous data to {number_categories} discrete categories...")

      assertthat::assert_that(
        !is.null(number_categories),
        msg = "Provide a valid number of categories to split your data into."
      )

      raster_cat <- elsar_continuous_to_categorical(
        raster_in = raster_in,
        data_layer = data_layer,
        number_categories = number_categories
      )

      raster_df <- raster_cat[[1]] %>%
        dplyr::mutate(
          category = as.factor(.data$category),
          interval = as.factor(.data$interval)
        )
    }
  }

  if (grepl("[ggplot]", type)) {
    # create background plot
    if (inherits(background, "SpatRaster")) {
      if (data_type == "discrete") {
        bg_data_layer <- terra::names(background)
        gg_background <- elsar_plot_background(
          background_dat = background,
          main_data = raster_in,
          data_type = "discrete",
          increase_extend = extend_background,
          number_categories = number_categories,
          data_layer = bg_data_layer
        )
      } else {
        gg_background <- elsar_plot_background(
          background_dat = background,
          main_data = raster_in,
          data_type = "continuous",
          increase_extend = extend_background
        )
      }
      background_dat <- gg_background[[2]]
      gg_background <- gg_background[[1]]
    }

    if (grepl("[raster]", type)) {
      if (data_type == "continuous") {
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
      } else {
        # Discrete
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
      }
    } else if (grepl("[vector]", type)) {
      if (data_type == "continuous") {
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
      } else {
        # Discrete vector
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
    }

    # Apply color scales
    if (is.null(custom_palette)) {
      if (data_type == "continuous") {
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
      }
    } else {
      plot_out <- plot_out + custom_palette
    }

    if (expand_plot) {
      plot_out <- plot_out +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0))
    }
  } else if (type == "tmap") {
    log_message("tmap support not yet implemented.")
  }

  if (raster_df_out) {
    return(list(plot_out, raster_df))
  } else {
    return(plot_out)
  }
}

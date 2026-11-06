#' Function to create a static plot from raster data
#'
#' `elsar_plot_static_raster()` allows plotting both continuous and discrete data from a `SpatRaster`
#' in `ggplot` or `tmap`. It can be combined with [elsar_plot_optics()] and [elsar_plot_extra_data()]
#' to create reproducible plots.
#'
#' In the default `"modern"` style the data are drawn over a neutral
#' context: the `background` raster in light greys and, if given, the
#' `boundary` with a soft halo and a thin outline. Continuous data get a long,
#' thin colourbar; binned data a stepped legend. Add a scale bar and north
#' arrow with [elsar_map_furniture()] and the attribution with
#' [elsar_map_caption()] (or use [elsar_plot_optics()]).
#'
#' In the modern style `type = "ggplot_vector"` and `"ggplot_raster"` give the
#' same map: cells are always drawn as a raster, which is faster and sharper.
#'
#' @param raster_in The `SpatRaster` file to be plotted.
#' @param data_type Character. Either "continuous" (default) or "discrete" to specify how data should be displayed.
#' @param type A character of the plot type. Either "ggplot_vector", "ggplot_raster" or "tmap".
#' @param categorical Logical. Only used when `data_type = "discrete"`. If TRUE, data is already categorical.
#' @param number_categories Integer. Only used when `data_type = "discrete"`. Number of categories to split continuous data into. Default is 10.
#' @param data_layer Character. Only used when `data_type = "discrete"`. The data layer name for categorization.
#'   Defaults to the first layer of `raster_in`.
#' @param background Requires a `SpatRaster` input (preferably with the same data as `raster_in`) to plot the background data.
#' @param extend_background A numerical value that allows to extent the background beyond the extent of `raster_in`.
#'   If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%).
#'   If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param custom_palette An optional ggplot2 scale (or list of scales) that replaces the default colour scale,
#'   e.g. [scale_fill_elsar_c()] or [ggplot2::scale_fill_distiller()].
#' @param plot_title An optional plot title (modern style).
#' @param legend_title An optional legend title.
#' @param color_map The name of the viridis palette to be used, e.g. "viridis" (default), "mako" or "rocket".
#' @param expand_plot Logical. Whether to expand the plot, so there is no space between border and plot.
#' @param raster_df_out Logical. Whether to return only the plot (FALSE) or also the data frame behind the plot (TRUE).
#'   Needed for `elsar_plot_distribution()`.
#' @param boundary Optional study-area outline (`sf`, `SpatVector` or
#'   `SpatRaster`), drawn as a context layer under the data and a thin outline
#'   on top. Modern style only.
#' @param style Character. `"modern"` (default) draws the map in the
#'   current elsar style ([elsar_theme_map()], [elsar_palette()] scales,
#'   neutral context); `"classic"` reproduces the look of elsar 0.4 and
#'   earlier. Set `options(elsar.plot_style = "classic")` to change the
#'   default for a whole session.
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
#'   boundary = boundary_proj,
#'   legend_title = "wad",
#'   plot_title = "Land under pressure"
#' ) +
#'   elsar_map_furniture() +
#'   elsar_map_caption())
#'
#' # The look of elsar 0.4 and earlier
#' elsar_plot_static_raster(wadOut, background = wad_dat, style = "classic")
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
                                     raster_df_out = FALSE,
                                     boundary = NULL,
                                     style = getOption("elsar.plot_style", "modern")) {
  data_type <- match.arg(data_type)
  style <- .resolve_style(style)

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

  if (is.null(data_layer)) data_layer <- terra::names(raster_in)[1]

  log_message("Creating static raster plot ({data_type}, {type}, {style} style)...")

  if (style == "modern") {
    return(.plot_static_raster_modern(
      raster_in = raster_in, data_type = data_type, type = type,
      categorical = categorical, number_categories = number_categories,
      data_layer = data_layer, background = background,
      extend_background = extend_background, custom_palette = custom_palette,
      plot_title = plot_title, legend_title = legend_title,
      color_map = color_map, expand_plot = expand_plot,
      raster_df_out = raster_df_out, boundary = boundary
    ))
  }

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

# Modern style ------------------------------------------------------------------

.plot_static_raster_modern <- function(raster_in, data_type, type, categorical,
                                       number_categories, data_layer,
                                       background, extend_background,
                                       custom_palette, plot_title,
                                       legend_title, color_map, expand_plot,
                                       raster_df_out, boundary) {
  if (type == "tmap") {
    log_message("tmap support not yet implemented.")
    return(NULL)
  }

  layer <- if (data_layer %in% terra::names(raster_in)) data_layer else 1
  r <- raster_in[[layer]]
  raster_df <- NULL

  if (data_type == "continuous") {
    plot_r <- r
    if (raster_df_out) {
      raster_df <- stats::na.omit(as.data.frame(raster_in, xy = TRUE))
    }
  } else if (categorical) {
    assertthat::assert_that(
      terra::is.factor(r),
      msg = "'raster_in' must be a factor raster when categorical = TRUE."
    )
    plot_r <- r
    raster_df <- as.data.frame(raster_in, xy = TRUE) %>%
      stats::na.omit() %>%
      dplyr::rename(interval = .data[[data_layer]]) %>%
      dplyr::mutate(interval = as.factor(.data$interval))
  } else {
    raster_cat <- elsar_continuous_to_categorical(
      raster_in = raster_in,
      data_layer = data_layer,
      number_categories = number_categories
    )
    breaks <- raster_cat[[2]]
    raster_df <- raster_cat[[1]] %>%
      dplyr::mutate(
        category = as.factor(.data$category),
        interval = as.factor(.data$interval)
      )
    n_bins <- length(breaks) - 1
    labels <- paste0(
      round(utils::head(breaks, -1), 2), "\u2013", round(utils::tail(breaks, -1), 2)
    )
    rcl <- cbind(utils::head(breaks, -1), utils::tail(breaks, -1), seq_len(n_bins))
    plot_r <- terra::classify(r, rcl, include.lowest = TRUE, right = TRUE, others = NA)
    levels(plot_r) <- data.frame(id = seq_len(n_bins), interval = make.unique(labels))
  }

  plot_out <- ggplot2::ggplot()
  lims <- NULL

  if (!is.null(background)) {
    bg <- terra::project(background[[1]], terra::crs(raster_in))
    bg <- rescale_raster(bg)
    if (!is.null(extend_background)) {
      bg <- elsar_extend(raster_main = raster_in, raster_to_crop = bg, extend_by = extend_background)
    }
    lims <- as.vector(terra::ext(bg))
    plot_out <- plot_out +
      tidyterra::geom_spatraster(data = bg, maxcell = 5e5) +
      scale_fill_elsar_c(palette = "context", guide = "none") +
      ggnewscale::new_scale_fill()
  }

  if (!is.null(boundary)) {
    plot_out <- plot_out + elsar_map_context(boundary, halo = is.null(background))
  }

  plot_out <- plot_out + tidyterra::geom_spatraster(data = plot_r, maxcell = 1e6)

  if (!is.null(boundary)) {
    plot_out <- plot_out + elsar_map_outline(boundary)
  }

  if (!is.null(custom_palette)) {
    plot_out <- plot_out + custom_palette
  } else if (data_type == "continuous") {
    plot_out <- plot_out +
      scale_fill_elsar_c(option = color_map, name = legend_title)
  } else {
    n_levels <- nrow(terra::levels(plot_r)[[1]])
    pal <- if (categorical && n_levels <= length(.elsar_categorical)) "categorical" else "sequential"
    plot_out <- plot_out +
      scale_fill_elsar_d(
        palette = pal, option = color_map, name = legend_title,
        na.translate = FALSE
      )
  }

  crs <- sf::st_crs(terra::crs(raster_in))
  plot_out <- plot_out +
    if (is.null(lims)) {
      ggplot2::coord_sf(crs = crs, datum = NA, expand = !expand_plot)
    } else {
      ggplot2::coord_sf(
        crs = crs, datum = NA, expand = FALSE,
        xlim = lims[1:2], ylim = lims[3:4]
      )
    }

  if (!is.null(plot_title) && any(nzchar(plot_title))) {
    plot_out <- plot_out + ggplot2::labs(title = plot_title)
  }
  plot_out <- plot_out + elsar_theme_map()

  if (raster_df_out) {
    return(list(plot_out, raster_df))
  }
  plot_out
}

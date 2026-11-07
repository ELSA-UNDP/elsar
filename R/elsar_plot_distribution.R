#' Function to create basic histogram with categorical data
#'
#' @param type A character denoting whether "ggplot" or "tmap" is being used.
#' @param categorical logical. Whether data is categorical (TRUE) or continuous (FALSE).
#' @param raster_df A `df` object of the data to include in the histogram.
#' @param col_interest A character of the column of interest. If `NULL`, uses "interval" for categorical data and
#'   the first column other than `x` and `y` for continuous data.
#' @param bin_number An integer value of how many bins to split the data into (default = 10).
#' @param show_legend logical. Whether to include legend in plot or not. Default is FALSE.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param style Character. `"modern"` (default) uses [elsar_theme()]
#'   and [elsar_palette()] scales; `"classic"` reproduces the look of elsar 0.4 and
#'   earlier. Set `options(elsar.plot_style = "classic")` to change the
#'   default for a whole session.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd",
#'   custom_projection = TRUE
#' )
#'
#' pus <- make_planning_units(boundary_proj = boundary_proj,
#'                            iso3 = "NPL",
#'                            pu_size = NULL,
#'                            pu_threshold = 8.5e5,
#'                            limit_to_mainland = FALSE)
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(raster_in = wad_dat,
#'                                  pus = pus,
#'                                  iso3 = "NPL")
#'
#' plot_cat <- elsar_plot_static_raster_d(
#' raster_in = wadOut, type = "ggplot_raster",
#' categorical = FALSE,
#' background = wad_dat,
#' number_categories = 10,
#' data_layer = "wad_final_cog",
#' raster_df_out = TRUE,
#' legend_title = "WAD"
#' )
#'
#' plot_wad_cat <- plot_cat[[1]]
#' raster_df_cat <- plot_cat[[2]]
#'
#'
#' plot_dist_cat <- elsar_plot_distribution(
#'   raster_df = raster_df_cat,
#'   categorical = TRUE
#' )
#'
#' (plot_wad_cat_inset <- patchwork::wrap_plots((plot_wad_cat +
#'   elsar_plot_optics()) +
#'   patchwork::inset_element(
#'     (plot_dist_cat +
#'       elsar_plot_optics(include_text = FALSE) +
#'       ggplot2::theme(
#'         axis.title = ggplot2::element_blank(),
#'         axis.text.x = ggplot2::element_blank(),
#'         axis.ticks.x = ggplot2::element_blank()
#'       )
#'     ),
#'     left = 0.1, # needs to be changed depending on plot dimensions
#'     bottom = 0.08, # needs to be changed depending on plot dimensions
#'     right = 0.35, # needs to be changed depending on plot dimensions
#'     top = 0.4, # needs to be changed depending on plot dimensions
#'     align_to = "plot"
#'   )))
#' }
elsar_plot_distribution <- function(type = "ggplot",
                                    categorical = TRUE,
                                    raster_df = NULL,
                                    col_interest = NULL,
                                    bin_number = 10,
                                    custom_palette = NULL, # needs to match main palette
                                    show_legend = FALSE,
                                    color_map = "viridis",
                                    style = getOption("elsar.plot_style", "modern")) {
  style <- .resolve_style(style)
  # Input validation
  assertthat::assert_that(
    type %in% c("ggplot", "tmap"),
    msg = "'type' must be either 'ggplot' or 'tmap'."
  )

  assertthat::assert_that(
    is.logical(categorical),
    msg = "'categorical' must be TRUE or FALSE."
  )

  assertthat::assert_that(
    is.data.frame(raster_df),
    msg = "'raster_df' must be a data frame."
  )

  assertthat::assert_that(
    is.numeric(bin_number) && bin_number > 0,
    msg = "'bin_number' must be a positive integer."
  )

  assertthat::assert_that(
    is.logical(show_legend),
    msg = "'show_legend' must be TRUE or FALSE."
  )

  log_message("Creating distribution plot ({ifelse(categorical, 'categorical', 'continuous')})...")

  if (!categorical && is.null(col_interest)) {
    col_interest <- setdiff(names(raster_df), c("x", "y"))[1]
  }

  if (style == "modern" && type == "ggplot") {
    return(.plot_distribution_modern(
      categorical = categorical, raster_df = raster_df,
      col_interest = col_interest, custom_palette = custom_palette,
      show_legend = show_legend, color_map = color_map
    ))
  }

  if (type == "ggplot") {
    if (categorical) {
      if (is.null(col_interest)) {
        plot_hist <- raster_df %>%
          dplyr::select("interval")
      } else {
        plot_hist <- raster_df %>%
          dplyr::rename(interval = dplyr::all_of(col_interest)) %>%
          dplyr::select("interval")
      }

      plot_hist <- plot_hist %>%
        dplyr::group_by(.data$interval) %>%
        dplyr::summarise(N = dplyr::n()) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$interval, y = .data$N, fill = .data$interval)) +
        ggplot2::geom_bar(stat = "identity", color = "black", show.legend = show_legend) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )

      if (is.null(custom_palette)) {
        plot_hist <- plot_hist +
          ggplot2::scale_fill_viridis_d(
            option = color_map
          )
      } else {
        plot_hist <- plot_hist + custom_palette
      }
    } else {
      raster_df <- raster_df %>%
        dplyr::mutate(group = 1)

      plot_hist <- ggplot2::ggplot() +
        ggridges::geom_density_ridges_gradient(
          data = raster_df,
          ggplot2::aes(
            x = .data[[col_interest]],
            y = .data$group,
            fill = ggplot2::after_stat(.data$x)
          ), show.legend = show_legend # , scale = 1
        ) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )

      if (is.null(custom_palette)) {
        plot_hist <- plot_hist +
          ggplot2::scale_fill_viridis_c(
            option = color_map
          )
      } else {
        plot_hist <- plot_hist + custom_palette
      }
    }
  }

  return(plot_hist)
}

# Modern style ------------------------------------------------------------------

.plot_distribution_modern <- function(categorical, raster_df, col_interest,
                                      custom_palette, show_legend, color_map) {
  ink <- elsar_inks()
  if (categorical) {
    counts <- raster_df %>%
      dplyr::rename(interval = dplyr::all_of(col_interest %||% "interval")) %>%
      dplyr::count(.data$interval, name = "N")

    p <- ggplot2::ggplot(counts, ggplot2::aes(x = .data$interval, y = .data$N, fill = .data$interval)) +
      ggplot2::geom_col(width = 0.92, colour = NA, show.legend = show_legend) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
      elsar_theme(grid = "y") +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )
    p <- p + (custom_palette %||% scale_fill_elsar_d(palette = "sequential", option = color_map, name = NULL))
  } else {
    raster_df <- dplyr::mutate(raster_df, group = 1)
    p <- ggplot2::ggplot() +
      ggridges::geom_density_ridges_gradient(
        data = raster_df,
        ggplot2::aes(
          x = .data[[col_interest]],
          y = .data$group,
          fill = ggplot2::after_stat(.data$x)
        ),
        colour = ink[["ink_secondary"]], linewidth = 0.3,
        show.legend = show_legend
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
      elsar_theme(grid = "none") +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      )
    p <- p + (custom_palette %||% scale_fill_elsar_c(option = color_map, name = NULL))
  }
  p
}

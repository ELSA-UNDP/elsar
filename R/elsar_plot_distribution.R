#' Function to create basic histogram with categorical data
#'
#' @param type A character denoting whether "ggplot" or "tmap" is being used.
#' @param categorical logical. Whether data is categorical (TRUE) or continuous (FALSE).
#' @param raster_df A `df` object of the data to include in the histogram.
#' @param col_interest A character of the column of interest. Uses "interval" if nothing is provided.
#' @param bin_number An integer value of how many bins to split the data into (default = 10).
#' @param show_legend logical. Whether to include legend in plot or not. Default is FALSE.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#'
#' @return A `list` of a `ggplot` or `tmap` object and a `SpatRaster` with the new background data.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd",
#'   custom_projection = TRUE
#' )
#'
#' pus <- make_planning_units(boundary_proj = boundary_proj,
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
#' data_layer = "Planning.Units",
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
elsar_plot_distribution <- function(type = "ggplot",
                                    categorical = TRUE,
                                    raster_df = NULL,
                                    col_interest = NULL,
                                    bin_number = 10,
                                    custom_palette = NULL, # needs to match main palette
                                    show_legend = FALSE,
                                    color_map = "viridis") {
  if (type == "ggplot") {
    if (categorical) {
      if (is.null(col_interest)) {
        plot_hist <- raster_df %>%
          dplyr::select("interval")
      } else {
        plot_hist <- raster_df %>%
          dplyr::rename(interval = .data[[col_interest]])
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

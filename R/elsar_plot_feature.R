#' Create a map of individual features
#'
#' @param raster_in The `SpatRaster` file to be plotted.
#' @param pus `SpatVector` The planning units (PUs) to use as outline
#' @param legend_title An optional legend title.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param figure_path An optional path to save the produced figures
#' @param no_legend Logical. TRUE means no legend will be shown
#'
#' @return A ggplot object with a map of the feature
#' @export
#'
#' @examples
#' #' boundary_proj <- make_boundary(
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
#' (wad_plot <- elsar_plot_feature(
#'   raster_in = wadOut,
#'   pus = pus,
#'   legend_title = "wad"
#' ))
elsar_plot_feature <- function(raster_in,
                               pus,
                               legend_title = NULL,
                               color_map = "viridis", #"rocket",
                               invert_palette = FALSE,
                               figure_path = NULL,
                               no_legend = FALSE) {
  # Prep outline
  outlines <- terra::as.polygons(pus) %>%
    # And convert to lines
    terra::as.lines()

  if (invert_palette) {
    palette_direction <- -1
  } else {
    palette_direction <- 1
  }

  # Create Plot
  gg_feature <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = raster_in) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_viridis_c(
      name = legend_title,
      option = color_map,
      direction = palette_direction,
      guide = guide_colorbar(
        label = TRUE,
        frame.colour = "black",
        barwidth = 11,
        barheight = 1.1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    tidyterra::geom_spatvector(
      data = outlines,
      color = alpha("white", 0.7),
      linewidth = 0.18
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.25,
      height = unit(0.25, "cm")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      text = ggplot2::element_text(size = 10, colour = "black"),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  if (no_legend) {
    gg_feature <-   gg_feature +
      ggplot2::theme(legend.position="none")
  }

  if (!is.null(figure_path)) {
    ggsave(file.path(glue::glue("{figure_path}/{legend_title}_{iso3}.png")),
      plot = gg_feature,
      device = "png",
      width = 8, height = 6, dpi = 200
    )
  }

  return(gg_feature)
}

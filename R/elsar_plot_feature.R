#' Plot a single feature raster within planning unit outlines
#'
#' @description
#' Creates a ggplot2 map of a single feature represented by a `SpatRaster`, with optional
#' planning unit outlines, customizable color palette direction, and optional saving to disk.
#'
#' @param raster_in SpatRaster. The raster to be plotted.
#' @param pus SpatVector. Planning units used as an outline for the raster.
#' @param legend_title character, optional. Title for the legend. Defaults to NULL.
#' @param color_map character. Name of the viridis palette to use. Defaults to "viridis".
#' @param invert_palette logical. If TRUE, reverses the palette direction. Defaults to FALSE.
#' @param figure_path character, optional. If provided, path to save the figure as a PNG.
#' @param no_legend logical. If TRUE, suppresses the legend. Defaults to FALSE.
#' @param iso3 The iso3 country code (character) of the country of interest.
#'
#' @return A `ggplot` object displaying the raster with optional planning unit outlines.
#'
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
#'
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#'
#' elsar_plot_feature(
#'   raster_in = wadOut,
#'   pus = pus,
#'   legend_title = "WAD",
#'   color_map = "magma",
#'   invert_palette = TRUE,
#'   no_legend = FALSE,
#'   iso3 = "NPL"
#' )
elsar_plot_feature <- function(raster_in,
                               pus,
                               legend_title = NULL,
                               color_map = "viridis", #"rocket",
                               invert_palette = FALSE,
                               figure_path = NULL,
                               no_legend = FALSE,
                               iso3 = NULL) {
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
      guide = ggplot2::guide_colorbar(
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
      color = scales::alpha("white", 0.7),
      linewidth = 0.18
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.25,
      height = grid::unit(0.25, "cm")
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
    ggplot2::ggsave(file.path(glue::glue("{figure_path}/{legend_title}_{iso3}.png")),
      plot = gg_feature,
      device = "png",
      width = 8, height = 6, dpi = 200
    )
  }

  return(gg_feature)
}

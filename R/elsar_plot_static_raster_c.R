#' Function to create a static plot from raster data with continuous data
#'
#' `elsar_plot_static_raster_c()` allows to plot continuous data from a
#' `SpatRaster` in `ggplot` or `tmap`. It can be combined with
#' [elsar_plot_optics()] and [elsar_plot_extra_data()] to create
#' reproducible plots.
#'
#' This is a convenience wrapper around [elsar_plot_static_raster()] with
#' `data_type = "continuous"`.
#'
#' @inheritParams elsar_plot_static_raster
#'
#' @return A `ggplot` or `tmap` object.
#' @export
#' @seealso [elsar_plot_static_raster()] for the unified plotting function
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
                                       type = "ggplot_vector",
                                       background = NULL,
                                       extend_background = 0.05,
                                       custom_palette = NULL,
                                       plot_title = "",
                                       legend_title = NULL,
                                       color_map = "viridis",
                                       expand_plot = FALSE,
                                       raster_df_out = FALSE) {
  elsar_plot_static_raster(
    raster_in = raster_in,
    data_type = "continuous",
    type = type,
    background = background,
    extend_background = extend_background,
    custom_palette = custom_palette,
    plot_title = plot_title,
    legend_title = legend_title,
    color_map = color_map,
    expand_plot = expand_plot,
    raster_df_out = raster_df_out
  )
}

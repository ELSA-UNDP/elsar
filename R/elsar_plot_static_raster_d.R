#' Function to create a static plot from raster data with discrete data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `elsar_plot_static_raster_d()` is deprecated. Please use
#' [elsar_plot_static_raster()] with `data_type = "discrete"` instead.
#'
#' @inheritParams elsar_plot_static_raster
#'
#' @return Either a `ggplot` object or a list with the `ggplot` and the `df`.
#' @export
#' @seealso [elsar_plot_static_raster()] for the unified plotting function
#' @keywords internal
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
  .Deprecated("elsar_plot_static_raster")
  elsar_plot_static_raster(
    raster_in = raster_in,
    data_type = "discrete",
    type = type,
    categorical = categorical,
    number_categories = number_categories,
    data_layer = data_layer,
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

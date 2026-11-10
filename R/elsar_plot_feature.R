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
#' @param figure_path character, optional. If provided, path to save the figure as a PNG. The file name is built
#'   from `legend_title` (or the layer name) and `iso3`.
#' @param no_legend logical. If TRUE, suppresses the legend. Defaults to FALSE.
#' @param custom_resolution A value that is used in ggsave() for dpi. Default is 200.
#' @param iso3 iso3 code of the country of interest.
#' @param plot_title Character, optional. Plot title (modern style).
#' @param binary_labels Character, length 2. Modern style: legend labels for
#'   the 0 and 1 classes when the layer only holds 0 and 1 (zones, lock-ins,
#'   presence layers), which get a two-class legend instead of a colour bar.
#'   Default `c("Outside", "Inside")`, in the language set with the
#'   `elsar.language` option (see [elsar_labels()]).
#' @param legend_position Character. Modern style: `"auto"` (default) puts the
#'   legend on the right for maps clearly taller than wide and on top
#'   otherwise. Or any `legend_position` of [elsar_theme_map()].
#' @param graticule Logical. Modern style: frame the map with a box and a
#'   latitude/longitude graticule labelled in degrees (default `TRUE`).
#' @param style Character. `"modern"` (default) draws the map in the
#'   current elsar style ([elsar_theme_map()], [elsar_palette()] scales,
#'   neutral context); `"classic"` reproduces the look of elsar 0.4 and
#'   earlier. Set `options(elsar.plot_style = "classic")` to change the
#'   default for a whole session.
#'
#' @return A `ggplot` object displaying the raster with optional planning unit outlines.
#'
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
#'   figure_path = here::here(),
#'   no_legend = FALSE,
#'   iso3 = "test",
#'   custom_resolution = 400
#' )
#' }
elsar_plot_feature <- function(raster_in,
                               pus,
                               legend_title = NULL,
                               color_map = "viridis", #"rocket",
                               invert_palette = FALSE,
                               figure_path = NULL,
                               iso3 = NULL,
                               no_legend = FALSE,
                               custom_resolution = 200,
                               plot_title = NULL,
                               binary_labels = c(.txt("outside"), .txt("inside")),
                               legend_position = "auto",
                               graticule = TRUE,
                               style = getOption("elsar.plot_style", "modern")) {
  style <- .resolve_style(style)
  # Input validation
  assertthat::assert_that(
    inherits(raster_in, "SpatRaster"),
    msg = "'raster_in' must be a SpatRaster object."
  )

  assertthat::assert_that(
    inherits(pus, c("SpatRaster", "SpatVector")),
    msg = "'pus' must be a SpatRaster or SpatVector object."
  )

  assertthat::assert_that(
    is.logical(invert_palette),
    msg = "'invert_palette' must be TRUE or FALSE."
  )

  assertthat::assert_that(
    is.logical(no_legend),
    msg = "'no_legend' must be TRUE or FALSE."
  )

  assertthat::assert_that(
    is.numeric(custom_resolution) && custom_resolution > 0,
    msg = "'custom_resolution' must be a positive number."
  )


  # Build descriptive message for logging

  plot_label <- if (!is.null(legend_title) && nzchar(legend_title)) {
    legend_title
  } else if (!is.null(names(raster_in)) && nzchar(names(raster_in)[1])) {
    names(raster_in)[1]
  } else {
    "unnamed"
  }
  log_message("Creating feature plot: {plot_label}...")

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
  if (style == "modern") {
    gg_feature <- .plot_feature_modern(
      raster_in = raster_in, pus = pus, outlines = outlines, legend_title = legend_title,
      color_map = color_map, palette_direction = palette_direction,
      no_legend = no_legend, plot_title = plot_title,
      binary_labels = binary_labels, legend_position = legend_position,
      graticule = graticule
    )
  } else {
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
  }

  if (!is.null(figure_path)) {
    # Clean filename: replace spaces/special chars, collapse underscores, lowercase
    clean_title <- gsub("[^a-zA-Z0-9_-]", "_", plot_label)
    clean_title <- gsub("-", "_", clean_title)
    clean_title <- gsub("_+", "_", clean_title)
    clean_title <- gsub("^_|_$", "", clean_title)
    clean_title <- tolower(clean_title)

    clean_iso3 <- if (!is.null(iso3)) tolower(iso3) else "unknown"

    output_file <- file.path(figure_path, paste0(clean_title, "_", clean_iso3, ".png"))
    log_message("Saving plot to '{output_file}'...")
    ggplot2::ggsave(output_file,
      plot = gg_feature,
      device = "png",
      width = 8, height = 6,
      dpi = custom_resolution,
      bg = if (style == "modern") elsar_inks()[["surface"]] else NULL
    )
  }

  log_message("Feature plot created successfully.")
  return(gg_feature)
}

# Modern style ------------------------------------------------------------------

.plot_feature_modern <- function(raster_in, pus, outlines, legend_title, color_map,
                                 palette_direction, no_legend, plot_title,
                                 binary_labels = c(.txt("outside"), .txt("inside")),
                                 legend_position = "auto", graticule = TRUE) {
  ink <- elsar_inks()
  r <- raster_in[[1]]
  if (.is_binary_raster(r)) {
    # A 0/1 layer (zone, lock-in, presence): two named classes, not a colour bar
    r <- terra::as.factor(r)
    levels(r) <- data.frame(id = c(0, 1), class = binary_labels)
    fill <- .scale_fill_binary(binary_labels, legend_title, color_map)
    if (no_legend) fill$guide <- "none"
  } else {
    fill <- scale_fill_elsar_c(
      option = color_map,
      direction = palette_direction,
      name = legend_title,
      guide = if (no_legend) "none" else guide_elsar_colourbar()
    )
  }
  p <- ggplot2::ggplot() +
    elsar_map_context(pus) +
    tidyterra::geom_spatraster(data = r, maxcell = 1e6) +
    fill +
    tidyterra::geom_spatvector(
      data = outlines,
      colour = ink[["ink_secondary"]],
      linewidth = 0.25
    ) +
    elsar_map_furniture(north_arrow = FALSE) +
    ggplot2::coord_sf(crs = sf::st_crs(terra::crs(raster_in)), datum = .map_datum(graticule)) +
    .degree_scales(graticule) +
    elsar_theme_map(
      legend_position = .map_legend_position(raster_in, legend_position),
      graticule = graticule
    )
  if (!is.null(plot_title) && any(nzchar(plot_title))) {
    p <- p + ggplot2::labs(title = plot_title)
  }
  p
}

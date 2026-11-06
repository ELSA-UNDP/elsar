# elsar visual style: inks, palettes, themes, scales and guides.
#
# Every map and chart elsar draws takes its colours, type and legend design
# from the functions in this file, so a change here restyles the whole package.

# Inks and surfaces ------------------------------------------------------------

#' Neutral inks and surfaces of the elsar style
#'
#' @description
#' `elsar_inks()` returns the neutral colours the elsar style is built from:
#' text inks, rules, the plotting surface and the muted land tones used for
#' map context. Use them when adding your own annotations so they match the
#' rest of the plot. Text always uses these neutral inks, never a data colour.
#'
#' @param dark Logical. If `TRUE`, returns the dark-mode set (light inks on a
#'   near-black surface).
#'
#' @return A named character vector of hex colours:
#'   * `ink`: primary text (titles, values).
#'   * `ink_secondary`: subtitles, legend text.
#'   * `ink_muted`: captions, attribution, scale bar text.
#'   * `rule`: grid lines and hairlines.
#'   * `surface`: plot background.
#'   * `land`: fill for context land (neighbouring areas, the study area
#'     under the data).
#'   * `land_line`: outline of context land.
#'   * `halo`: soft halo around the study area.
#'   * `shadow`: drop shadow under the study area.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' elsar_inks()
#' elsar_inks(dark = TRUE)["ink"]
elsar_inks <- function(dark = FALSE) {
  assertthat::assert_that(assertthat::is.flag(dark), !is.na(dark),
    msg = "'dark' must be TRUE or FALSE."
  )
  if (dark) {
    c(
      ink = "#F0EFEC", ink_secondary = "#C3C2B7", ink_muted = "#8E8D86",
      rule = "#383835", surface = "#1A1A19", land = "#2A2A28",
      land_line = "#5A5955", halo = "#3A3A37", shadow = "#000000"
    )
  } else {
    c(
      ink = "#1F1F1E", ink_secondary = "#52514E", ink_muted = "#7A7974",
      rule = "#E3E2DE", surface = "#FFFFFF", land = "#EEEDE9",
      land_line = "#B5B4AE", halo = "#DCE6EA", shadow = "#3A3A37"
    )
  }
}

# Palettes ---------------------------------------------------------------------

# ELSA action colours. These are the established ELSA colours and match the
# defaults of elsar_action_scheme().
.elsar_action_colours <- c(
  "Protect" = "#06C723",
  "Restore" = "#8803FC",
  "Manage" = "#FFEA00",
  "Urban Greening" = "#FF0000",
  "Do Nothing" = "#E5E5E5"
)

# Okabe-Ito hues, reordered and without yellow and black. The first four
# slots stay distinguishable for every pair under deutan, protan and tritan
# simulation, which is what a map legend needs.
.elsar_categorical <- c(
  "#0072B2", "#E69F00", "#009E73", "#D55E00", "#56B4E9", "#CC79A7"
)

# Two-hue diverging ramp (warm brown to teal) around a neutral grey midpoint.
.elsar_diverging <- c("#7A4A12", "#C29A5F", "#F0EFEC", "#5FA9A1", "#0E6A62")

# Neutral ramp for context rasters (background data shown behind the main
# layer).
.elsar_context <- c("#F7F6F3", "#C4C3BD")

.elsar_viridis_options <- c(
  "viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako",
  "A", "B", "C", "D", "E", "F", "G"
)

#' Colour palettes of the elsar style
#'
#' @description
#' `elsar_palette()` returns the colours behind every elsar scale:
#'
#' * `"actions"`: the established ELSA action colours, named by action
#'   (Protect `#06C723`, Restore `#8803FC`, Manage `#FFEA00`, Urban Greening
#'   `#FF0000`) plus the neutral "Do Nothing" grey. They match the defaults of
#'   [elsar_action_scheme()].
#' * `"categorical"`: six colour-blind-safe hues for unordered categories
#'   (Okabe-Ito, reordered, without yellow and black). On a map every
#'   category sits next to every other, so prefer four or fewer: the first
#'   four stay distinct for every pair under all common colour-vision
#'   deficiencies. Fold extra categories into "Other" or use facets rather
#'   than adding hues.
#' * `"sequential"`: a perceptually uniform viridis ramp (choose the family
#'   member with `option`) for ordered magnitudes. Rainbow ramps are never
#'   used for sequential data.
#' * `"diverging"`: two hues (brown and teal) around a neutral grey midpoint,
#'   for data with a meaningful centre (change, difference from a target).
#' * `"context"`: a light neutral grey ramp for background data that should
#'   recede behind the main layer.
#'
#' @param name Character. One of `"actions"`, `"categorical"`,
#'   `"sequential"`, `"diverging"` or `"context"`.
#' @param n Integer. Number of colours. Ignored for `"actions"`. Defaults to
#'   all six hues for `"categorical"` and seven steps for the ramps.
#' @param direction `1` (default) or `-1` to reverse the order.
#' @param option Character. The viridis family member for
#'   `name = "sequential"`, e.g. `"viridis"` (default), `"mako"`,
#'   `"rocket"`, `"magma"` or `"cividis"`.
#'
#' @return A character vector of hex colours (named for `"actions"`).
#'
#' @family elsar style
#' @export
#'
#' @examples
#' elsar_palette("actions")
#' elsar_palette("categorical", n = 3)
#' elsar_palette("sequential", n = 5, option = "mako")
#' elsar_palette("diverging", n = 7)
elsar_palette <- function(name = c("actions", "categorical", "sequential", "diverging", "context"),
                          n = NULL,
                          direction = 1,
                          option = "viridis") {
  name <- match.arg(name)
  assertthat::assert_that(direction %in% c(-1, 1),
    msg = "'direction' must be 1 or -1."
  )
  if (!is.null(n)) {
    assertthat::assert_that(assertthat::is.count(n),
      msg = "'n' must be a positive whole number."
    )
  }

  pal <- switch(name,
    actions = .elsar_action_colours,
    categorical = {
      n <- n %||% length(.elsar_categorical)
      if (n > length(.elsar_categorical)) {
        rlang::abort(c(
          paste0(
            "The categorical palette has ", length(.elsar_categorical),
            " colour-blind-safe hues, but ", n, " were requested."
          ),
          i = "Fold the smallest categories into \"Other\" or use facets."
        ))
      }
      .elsar_categorical[seq_len(n)]
    },
    sequential = {
      .check_viridis_option(option)
      scales::viridis_pal(option = option)(n %||% 7)
    },
    diverging = grDevices::colorRampPalette(.elsar_diverging, space = "Lab")(n %||% 7),
    context = grDevices::colorRampPalette(.elsar_context, space = "Lab")(n %||% 7)
  )
  if (direction == -1) pal <- rev(pal)
  pal
}

.check_viridis_option <- function(option) {
  assertthat::assert_that(
    is.character(option), length(option) == 1, option %in% .elsar_viridis_options,
    msg = paste0(
      "'option' must be a viridis palette: ",
      toString(.elsar_viridis_options[1:7]), "."
    )
  )
  invisible(option)
}

# Fonts ------------------------------------------------------------------------

# Returns a font family that the graphics device can use. "" (the device
# default sans) is always safe. A named family is checked with systemfonts
# when it is installed; unknown families fall back to "" with a warning.
.elsar_font <- function(family = "") {
  assertthat::assert_that(is.character(family), length(family) == 1,
    msg = "'base_family' must be a single character string."
  )
  if (!nzchar(family) || family %in% c("sans", "serif", "mono")) {
    return(family)
  }
  if (rlang::is_installed("systemfonts")) {
    known <- unique(c(
      systemfonts::system_fonts()$family,
      systemfonts::registry_fonts()$family
    ))
    if (!family %in% known) {
      rlang::warn(
        c(
          paste0("Font family \"", family, "\" is not available; using the default sans-serif font."),
          i = "Install the font or register it with `systemfonts::register_font()`."
        ),
        .frequency = "regularly", .frequency_id = paste0("elsar_font_", family)
      )
      return("")
    }
  }
  family
}

# Themes -----------------------------------------------------------------------

#' The elsar map and chart themes
#'
#' @description
#' `elsar_theme_map()` is the default theme of every elsar map. It removes
#' the panel border, axes and graticule so the data carry the page, sets a
#' clear type hierarchy (bold left-aligned title, secondary-ink subtitle,
#' small muted caption for attribution), and lays legends out tidily: a long
#' thin colourbar with the title above it for continuous data, small square
#' keys for categories.
#'
#' `elsar_theme()` is the matching theme for non-map charts (bar charts,
#' distributions). It shares the typography and inks and adds recessive
#' horizontal grid lines.
#'
#' Both are ordinary ggplot2 themes: add them with `+` and refine them with
#' [ggplot2::theme()].
#'
#' @param base_size Numeric. Base font size in points. Default `10`.
#' @param base_family Character. Font family. The default `""` uses the
#'   graphics device's sans-serif font. When the \pkg{systemfonts} package is
#'   installed, an unavailable family falls back to the default with a
#'   warning.
#' @param legend_position Character. Legend position: `"top"` (default for
#'   maps; the legend sits under the title, left-aligned), `"bottom"`,
#'   `"right"`, `"left"`, `"inside"` or `"none"`.
#' @param dark Logical. If `TRUE`, uses light inks on a dark surface.
#' @param grid Character. For `elsar_theme()`: which major grid lines to
#'   draw, `"y"` (default), `"x"`, `"xy"` or `"none"`.
#'
#' @return A [ggplot2::theme()] object.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' r <- terra::rast(system.file("extdata", "wad_subset.tif", package = "elsar"))
#' r <- terra::aggregate(r, 20)
#' ggplot2::ggplot() +
#'   tidyterra::geom_spatraster(data = r) +
#'   scale_fill_elsar_c(name = "Convergence of pressures") +
#'   ggplot2::labs(title = "Land under pressure", caption = elsar_attribution()) +
#'   elsar_theme_map()
#'
#' df <- data.frame(action = c("Protect", "Restore", "Manage"), area = c(30, 12, 18))
#' ggplot2::ggplot(df, ggplot2::aes(action, area)) +
#'   ggplot2::geom_col(fill = elsar_inks()[["ink_secondary"]], width = 0.6) +
#'   elsar_theme()
elsar_theme_map <- function(base_size = 10,
                            base_family = "",
                            legend_position = "top",
                            dark = FALSE) {
  .check_theme_args(base_size, legend_position)
  ink <- elsar_inks(dark)
  family <- .elsar_font(base_family)
  horizontal <- legend_position %in% c("top", "bottom")

  ggplot2::theme_void(base_size = base_size, base_family = family) +
    .elsar_theme_core(base_size, family, ink, legend_position) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.justification = if (horizontal) "left" else "top"
    )
}

#' @rdname elsar_theme_map
#' @export
elsar_theme <- function(base_size = 10,
                        base_family = "",
                        legend_position = "top",
                        dark = FALSE,
                        grid = c("y", "x", "xy", "none")) {
  .check_theme_args(base_size, legend_position)
  grid <- match.arg(grid)
  ink <- elsar_inks(dark)
  family <- .elsar_font(base_family)
  horizontal <- legend_position %in% c("top", "bottom")
  rule <- ggplot2::element_line(colour = ink[["rule"]], linewidth = 0.35)

  ggplot2::theme_minimal(base_size = base_size, base_family = family) +
    .elsar_theme_core(base_size, family, ink, legend_position) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "xy")) rule else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "xy")) rule else ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = ink[["ink_secondary"]], size = ggplot2::rel(0.85)),
      axis.title = ggplot2::element_text(colour = ink[["ink_secondary"]], size = ggplot2::rel(0.85)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.6), inherit.blank = TRUE),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size * 0.6), angle = 90, inherit.blank = TRUE),
      axis.ticks = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = ink[["ink_muted"]], linewidth = 0.4),
      legend.justification = if (horizontal) "left" else "top"
    )
}

.check_theme_args <- function(base_size, legend_position) {
  assertthat::assert_that(assertthat::is.number(base_size), base_size > 0,
    msg = "'base_size' must be a positive number."
  )
  assertthat::assert_that(
    is.character(legend_position), length(legend_position) == 1,
    legend_position %in% c("top", "bottom", "right", "left", "inside", "none"),
    msg = "'legend_position' must be one of 'top', 'bottom', 'right', 'left', 'inside' or 'none'."
  )
}

# Shared typography, surfaces and legend layout.
.elsar_theme_core <- function(base_size, family, ink, legend_position) {
  horizontal <- legend_position %in% c("top", "bottom")
  half <- base_size / 2
  ggplot2::theme(
    text = ggplot2::element_text(family = family, colour = ink[["ink"]], size = base_size),
    plot.background = ggplot2::element_rect(fill = ink[["surface"]], colour = NA),
    panel.background = ggplot2::element_rect(fill = ink[["surface"]], colour = NA),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.35), face = "bold", colour = ink[["ink"]],
      hjust = 0, margin = ggplot2::margin(b = base_size * 0.35)
    ),
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1), colour = ink[["ink_secondary"]],
      hjust = 0, lineheight = 1.15, margin = ggplot2::margin(b = base_size * 0.9)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.75), colour = ink[["ink_muted"]],
      hjust = 1, lineheight = 1.1, margin = ggplot2::margin(t = base_size * 0.9)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(base_size, base_size, half, base_size),
    strip.text = ggplot2::element_text(
      size = ggplot2::rel(0.95), face = "bold", colour = ink[["ink"]],
      hjust = 0, margin = ggplot2::margin(b = half)
    ),
    legend.position = legend_position,
    legend.location = "plot",
    legend.direction = if (horizontal) "horizontal" else "vertical",
    legend.title = ggplot2::element_text(
      size = ggplot2::rel(0.85), face = "bold", colour = ink[["ink_secondary"]]
    ),
    legend.title.position = "top",
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = ink[["ink_secondary"]]),
    legend.key = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(base_size * 1.05, "pt"),
    legend.key.spacing.x = ggplot2::unit(base_size * 0.8, "pt"),
    legend.key.spacing.y = ggplot2::unit(base_size * 0.3, "pt"),
    legend.background = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing = ggplot2::unit(base_size * 0.8, "pt"),
    legend.spacing = ggplot2::unit(base_size * 1.6, "pt"),
    legend.box.just = "left"
  )
}

# Guides -----------------------------------------------------------------------

#' Colourbar guide of the elsar style
#'
#' @description
#' A [ggplot2::guide_colourbar()] that is long and thin whatever its
#' direction: `length` along the bar, `thickness` across it. Horizontal bars
#' (legends at the top or bottom) get their title above the bar. The default
#' guide of [scale_fill_elsar_c()].
#'
#' @param length Numeric. Bar length, in multiples of the font's line height.
#'   Default `12`.
#' @param thickness Numeric. Bar thickness, in lines. Default `0.55`.
#' @param ... Passed to [ggplot2::guide_colourbar()], e.g. `direction` or
#'   `reverse`.
#'
#' @return A ggplot2 guide.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' ggplot2::ggplot(faithful, ggplot2::aes(waiting, eruptions, colour = eruptions)) +
#'   ggplot2::geom_point() +
#'   scale_colour_elsar_c(guide = guide_elsar_colourbar(length = 8)) +
#'   elsar_theme()
guide_elsar_colourbar <- function(length = 12, thickness = 0.55, ...) {
  assertthat::assert_that(
    assertthat::is.number(length), length > 0,
    assertthat::is.number(thickness), thickness > 0,
    msg = "'length' and 'thickness' must be positive numbers."
  )
  args <- list(...)
  args$nbin <- args$nbin %||% 300
  args$theme <- args$theme %||% ggplot2::theme(legend.ticks = ggplot2::element_blank())
  guide <- do.call(ggplot2::guide_colourbar, args)

  ggplot2::ggproto(NULL, guide,
    setup_elements = function(params, elements, theme) {
      along <- grid::unit(length / 5, "lines")
      across <- grid::unit(thickness, "lines")
      if (identical(params$direction, "horizontal")) {
        theme$legend.key.width <- along
        theme$legend.key.height <- across
      } else {
        theme$legend.key.height <- along
        theme$legend.key.width <- across
      }
      ggplot2::GuideColourbar$setup_elements(params, elements, theme)
    }
  )
}

#' Legend guide of the elsar style
#'
#' @description
#' A [ggplot2::guide_legend()] with small square keys, laid out in rows for
#' legends at the top or bottom of the plot. The default guide of
#' [scale_fill_elsar_d()].
#'
#' @param ... Passed to [ggplot2::guide_legend()], e.g. `nrow` or `ncol`.
#'
#' @return A ggplot2 guide.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' guide_elsar_legend(nrow = 1)
guide_elsar_legend <- function(...) {
  ggplot2::guide_legend(...)
}

#' Stepped legend for ordered classes
#'
#' @description
#' A [ggplot2::guide_legend()] drawn as a single row of touching colour
#' steps with each class label centred underneath, so ordered classes (e.g.
#' binned values) read like a stepped colour bar. The default guide of
#' [scale_fill_elsar_d()] for the `"sequential"` and `"diverging"`
#' palettes.
#'
#' @param key_width Numeric. Width of each step, in lines. Default `3.6`.
#' @param thickness Numeric. Height of the steps, in lines. Default `0.55`.
#' @param ... Passed to [ggplot2::guide_legend()].
#'
#' @return A ggplot2 guide.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' guide_elsar_steps(key_width = 3)
guide_elsar_steps <- function(key_width = 3.6, thickness = 0.55, ...) {
  assertthat::assert_that(
    assertthat::is.number(key_width), key_width > 0,
    assertthat::is.number(thickness), thickness > 0,
    msg = "'key_width' and 'thickness' must be positive numbers."
  )
  args <- list(...)
  args$nrow <- args$nrow %||% 1
  args$theme <- ggplot2::theme(
    legend.key.width = grid::unit(key_width, "lines"),
    legend.key.height = grid::unit(thickness, "lines"),
    legend.key.spacing.x = grid::unit(1, "pt"),
    legend.text.position = "bottom",
    legend.text = ggplot2::element_text(hjust = 0.5)
  ) + (args$theme %||% ggplot2::theme())
  do.call(ggplot2::guide_legend, args)
}

# Scales -----------------------------------------------------------------------

#' Colour and fill scales of the elsar style
#'
#' @description
#' Continuous (`_c`) and discrete (`_d`) scales using [elsar_palette()].
#'
#' * `scale_fill_elsar_c(palette = "sequential")` (default): viridis family
#'   ramp with a long, thin colourbar ([guide_elsar_colourbar()]).
#' * `scale_fill_elsar_c(palette = "diverging", midpoint = 0)`: brown-grey-teal
#'   ramp centred on `midpoint`.
#' * `scale_fill_elsar_d(palette = "actions")`: the ELSA action colours,
#'   matched by name (Protect, Restore, Manage, Urban Greening, Do Nothing).
#'   Pass `values` to recolour a custom scheme.
#' * `scale_fill_elsar_d(palette = "categorical")`: colour-blind-safe hues
#'   for unordered categories.
#' * `scale_fill_elsar_d(palette = "sequential")`: ordered classes (e.g.
#'   binned values) from the viridis ramp.
#'
#' @param palette Character. Palette name, see [elsar_palette()].
#' @param option Character. viridis family member for sequential palettes.
#' @param direction `1` (default) or `-1` to reverse the palette.
#' @param midpoint Numeric. Data value at the neutral centre of a diverging
#'   palette. Default `0`.
#' @param na.value Colour for missing values. Default transparent.
#' @param guide Legend guide. Defaults to [guide_elsar_colourbar()] for
#'   continuous scales. For discrete scales the default (`NULL`) is
#'   [guide_elsar_steps()] for ordered palettes (`"sequential"`,
#'   `"diverging"`) and [guide_elsar_legend()] otherwise.
#' @param values For discrete scales: an optional named vector of colours
#'   that replaces the palette (e.g. the `colour` column of a custom
#'   [elsar_action_scheme()]).
#' @param aesthetics Character. The aesthetics the scale applies to.
#' @param ... Passed on to [ggplot2::continuous_scale()] or
#'   [ggplot2::discrete_scale()], e.g. `name`, `limits`, `labels` or
#'   `breaks`.
#'
#' @return A ggplot2 scale.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' df <- terra::as.data.frame(ex$actions, xy = TRUE)
#' df$action <- factor(df$action,
#'   levels = 1:5,
#'   labels = c("Protect", "Restore", "Manage", "Urban Greening", "Do Nothing")
#' )
#' ggplot2::ggplot(df, ggplot2::aes(x, y, fill = action)) +
#'   ggplot2::geom_raster() +
#'   ggplot2::coord_equal() +
#'   scale_fill_elsar_d(palette = "actions", name = NULL) +
#'   elsar_theme_map()
#'
#' ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density - 0.01)) +
#'   ggplot2::geom_raster() +
#'   scale_fill_elsar_c(palette = "diverging", midpoint = 0, name = "Difference") +
#'   elsar_theme()
scale_fill_elsar_c <- function(palette = c("sequential", "diverging", "context"),
                               option = "viridis",
                               direction = 1,
                               midpoint = 0,
                               na.value = NA,
                               guide = guide_elsar_colourbar(),
                               aesthetics = "fill",
                               ...) {
  palette <- match.arg(palette)
  cols <- elsar_palette(palette, n = if (palette == "sequential") 256 else 11,
    direction = direction, option = option
  )
  rescaler <- if (palette == "diverging") {
    assertthat::assert_that(assertthat::is.number(midpoint),
      msg = "'midpoint' must be a single number."
    )
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to = to, from = from, mid = midpoint)
    }
  } else {
    scales::rescale
  }
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    palette = scales::gradient_n_pal(cols),
    na.value = na.value,
    guide = guide,
    rescaler = rescaler,
    ...
  )
}

#' @rdname scale_fill_elsar_c
#' @export
scale_colour_elsar_c <- function(palette = c("sequential", "diverging", "context"),
                                 option = "viridis",
                                 direction = 1,
                                 midpoint = 0,
                                 na.value = NA,
                                 guide = guide_elsar_colourbar(),
                                 aesthetics = "colour",
                                 ...) {
  scale_fill_elsar_c(
    palette = palette, option = option, direction = direction,
    midpoint = midpoint, na.value = na.value, guide = guide,
    aesthetics = aesthetics, ...
  )
}

#' @rdname scale_fill_elsar_c
#' @export
scale_fill_elsar_d <- function(palette = c("categorical", "actions", "sequential", "diverging"),
                               option = "viridis",
                               direction = 1,
                               values = NULL,
                               na.value = NA,
                               guide = NULL,
                               aesthetics = "fill",
                               ...) {
  palette <- match.arg(palette)
  if (is.null(guide)) {
    guide <- if (palette %in% c("sequential", "diverging")) guide_elsar_steps() else guide_elsar_legend()
  }
  if (is.null(values) && palette == "actions") {
    values <- elsar_palette("actions")
  }
  if (!is.null(values)) {
    return(ggplot2::scale_fill_manual(
      values = values, na.value = na.value, guide = guide,
      aesthetics = aesthetics, ...
    ))
  }
  if (palette == "sequential") .check_viridis_option(option)
  pal <- function(n) elsar_palette(palette, n = n, direction = direction, option = option)
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = pal,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @rdname scale_fill_elsar_c
#' @export
scale_colour_elsar_d <- function(palette = c("categorical", "actions", "sequential", "diverging"),
                                 option = "viridis",
                                 direction = 1,
                                 values = NULL,
                                 na.value = NA,
                                 guide = NULL,
                                 aesthetics = "colour",
                                 ...) {
  scale_fill_elsar_d(
    palette = palette, option = option, direction = direction,
    values = values, na.value = na.value, guide = guide,
    aesthetics = aesthetics, ...
  )
}

# Style switch -----------------------------------------------------------------

.elsar_styles <- c("modern", "classic")

# Resolves the `style` argument of the plotting functions. The default comes
# from getOption("elsar.plot_style", "modern").
.resolve_style <- function(style) {
  style <- style %||% "modern"
  assertthat::assert_that(
    is.character(style), length(style) == 1, style %in% .elsar_styles,
    msg = "'style' must be \"modern\" or \"classic\"."
  )
  style
}

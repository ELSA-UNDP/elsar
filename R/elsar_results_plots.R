# Plots for ELSA results ---------------------------------------------------------
#
# All plots take their labels and colours from the action scheme stored in the
# prepared action map or summary, and return ggplot / patchwork objects that can
# be themed further (e.g. `+ elsar_plot_optics()` on maps).

#' Map ELSA actions
#'
#' Draws the action map from [elsar_prepare_actions()], with optional region
#' outlines and existing protected-area boundaries.
#'
#' @param x A prepared action map from [elsar_prepare_actions()].
#' @param regions Optional `sf` or `SpatVector` of regions to outline.
#' @param show_no_action Logical. Draw no-action cells (in their scheme colour,
#'   light grey by default) so the planning area is visible. If `FALSE`, they
#'   are left blank. Default `TRUE`.
#' @param show_pas Logical. Outline existing protected areas, if `x` has a
#'   protected-area layer. Default `TRUE`.
#' @param pa_threshold Numeric. Cells with at least this protected-area cover
#'   are outlined as protected. Default `0.5`.
#' @param pa_colour Colour of the protected-area outline.
#' @param pa_label Legend label of the protected-area outline.
#' @param title Optional plot title.
#' @param legend Logical. Show the legend. Default `TRUE`.
#' @param maxcell Integer. Maximum number of cells drawn; larger rasters are
#'   resampled for display only (see [tidyterra::geom_spatraster()]).
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object.
#' @seealso [elsar_plot_action_bars()], [elsar_plot_region()],
#'   [elsar_plot_optics()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' elsar_plot_actions(x, regions = ex$regions)
elsar_plot_actions <- function(x,
                               regions = NULL,
                               show_no_action = TRUE,
                               show_pas = TRUE,
                               pa_threshold = 0.5,
                               pa_colour = "#005000",
                               pa_label = "Existing PAs",
                               title = NULL,
                               legend = TRUE,
                               maxcell = 5e5,
                               base_size = 10) {
  scheme <- .scheme_from_actions(x)
  shown <- if (show_no_action) scheme else scheme[scheme$is_action, ]
  fill_values <- stats::setNames(scheme$colour, scheme$label)
  if (!show_no_action) fill_values[!scheme$is_action] <- "transparent"

  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = x[["action"]], maxcell = maxcell)
  # geom_spatraster() adds its own fill scale for categorical rasters; replace it quietly
  p <- suppressMessages(p + ggplot2::scale_fill_manual(
      values = fill_values,
      breaks = shown$label,
      labels = .wrap(shown$label),
      na.value = "transparent",
      na.translate = FALSE,
      name = NULL,
      drop = FALSE
    ))

  if (show_pas && "pa_fraction" %in% names(x)) {
    pa_sf <- .pa_outline(x[["pa_fraction"]], pa_threshold)
    if (!is.null(pa_sf)) {
      p <- p +
        ggplot2::geom_sf(
          data = pa_sf, ggplot2::aes(colour = pa_label),
          fill = NA, linewidth = 0.4, inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_manual(values = stats::setNames(pa_colour, pa_label), name = NULL)
    }
  }

  if (!is.null(regions)) {
    regions <- .regions_as_sf(regions, x)
    p <- p + ggplot2::geom_sf(
      data = regions, fill = NA, colour = "grey25", linewidth = 0.3,
      inherit.aes = FALSE
    )
  }

  p +
    ggplot2::coord_sf(crs = terra::crs(x), datum = NA, expand = FALSE) +
    ggplot2::labs(title = title) +
    .results_theme(base_size) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    if (legend) {
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 1, nrow = 1),
        colour = ggplot2::guide_legend(order = 2, override.aes = list(fill = NA))
      )
    } else {
      ggplot2::guides(fill = "none", colour = "none")
    }
}

#' Stacked bar charts of ELSA actions
#'
#' Plots an action summary from [elsar_summarise_actions()] as stacked bars,
#' either comparing regions or showing each action. The part of each action
#' inside existing protected areas is drawn as its own segment at the base of
#' the bar, so with protected areas locked into Protect the Protect bar shows
#' existing and new protection together.
#'
#' Actions sharing a `group` in the action scheme are stacked in one bar.
#'
#' @param summary A tibble from [elsar_summarise_actions()].
#' @param metric `"percent"` (share of each region's planning area, default)
#'   or `"area"`.
#' @param by `"region"` (default) draws one bar per region, stacked by action,
#'   to compare regions. `"action"` draws one bar per action group, with a
#'   panel per region if there are several.
#' @param orientation `"vertical"` (default) or `"horizontal"` bars. Long or
#'   many category labels are wrapped, and tilted when vertical bars get
#'   crowded; horizontal bars suit many regions with long names.
#' @param split_pa Logical. Draw the part of each action inside existing
#'   protected areas as a separate segment. Default `TRUE` when the summary
#'   has protected-area information.
#' @param pa_colour,pa_label Colour and legend label of that segment.
#' @param title Optional plot title.
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object.
#' @seealso [elsar_summarise_actions()], [elsar_plot_protection_gap()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
#'
#' # Compare regions
#' elsar_plot_action_bars(s)
#'
#' # Area of each action, one panel per region
#' elsar_plot_action_bars(s, metric = "area", by = "action")
elsar_plot_action_bars <- function(summary,
                                   metric = c("percent", "area"),
                                   by = c("region", "action"),
                                   split_pa = isTRUE(attr(summary, "has_pa")),
                                   pa_colour = "#005000",
                                   pa_label = "Existing PAs",
                                   orientation = c("vertical", "horizontal"),
                                   title = NULL,
                                   base_size = 10) {
  .check_summary(summary)
  metric <- match.arg(metric)
  by <- match.arg(by)
  orientation <- match.arg(orientation)
  if (split_pa && !isTRUE(attr(summary, "has_pa"))) {
    warning("The summary has no protected-area information; 'split_pa' ignored.", call. = FALSE)
    split_pa <- FALSE
  }

  d <- .bar_data(summary, split_pa, pa_label)
  colours <- .segment_colours(summary, split_pa, pa_colour, pa_label)
  .bar_plot(d, colours, metric, by, attr(summary, "units"), title, base_size,
            orientation = orientation)
}

#' Plot current and proposed protection against a target
#'
#' Draws the output of [elsar_protection_gap()] as one bar per region:
#' existing protected areas plus the new protection proposed by ELSA, with a
#' dashed line at the target. The national bar is set apart from the regions.
#'
#' @param gap A tibble from [elsar_protection_gap()].
#' @param metric `"percent"` (default) or `"area"`. The target line is only
#'   drawn for `"percent"`.
#' @param sort `"total"` (default) orders regions from most to least total
#'   protection; `"none"` keeps their order in `gap`. The national bar always
#'   comes last.
#' @param orientation `"vertical"` (default) or `"horizontal"` bars.
#' @param national_label Name of the national row in `gap`.
#' @param colours Named character vector with colours for `current` and `new`
#'   protection.
#' @param labels Named character vector with legend labels for `current` and
#'   `new` protection.
#' @param title Optional plot title.
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object.
#' @seealso [elsar_protection_gap()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
#' g <- elsar_protection_gap(s)
#' elsar_plot_protection_gap(g)
#' elsar_plot_protection_gap(g, orientation = "horizontal", sort = "none")
elsar_plot_protection_gap <- function(gap,
                                      metric = c("percent", "area"),
                                      sort = c("total", "none"),
                                      orientation = c("vertical", "horizontal"),
                                      national_label = attr(gap, "national_label") %||% "National",
                                      colours = c(current = "#005000", new = "#06C723"),
                                      labels = c(current = "Existing PAs", new = "New protection (ELSA)"),
                                      title = NULL,
                                      base_size = 10) {
  needed <- c("region", "current", "new", "pct_current", "pct_new", "pct_total")
  assertthat::assert_that(
    is.data.frame(gap), all(needed %in% names(gap)),
    msg = "'gap' must be created with elsar_protection_gap()."
  )
  assertthat::assert_that(
    all(c("current", "new") %in% names(colours)),
    all(c("current", "new") %in% names(labels)),
    msg = "'colours' and 'labels' must be named vectors with 'current' and 'new'."
  )
  metric <- match.arg(metric)
  sort <- match.arg(sort)
  orientation <- match.arg(orientation)
  vertical <- orientation == "vertical"
  units <- attr(gap, "units") %||% "ha"

  pct <- metric == "percent"
  v_current <- if (pct) gap$pct_current else gap$current
  v_new <- if (pct) gap$pct_new else gap$new
  v_total <- if (pct) gap$pct_total else gap$total

  # Order: regions (optionally by total), then the national bar
  regions <- as.character(gap$region)
  is_national <- regions == national_label
  ord <- if (sort == "total") order(-v_total) else seq_along(regions)
  ord <- c(ord[!is_national[ord]], which(is_national))
  region_levels <- regions[ord]
  if (!vertical) region_levels <- rev(region_levels)
  panel <- factor(ifelse(is_national, "national", "regions"), levels = c("regions", "national"))

  long <- data.frame(
    region = factor(rep(regions, 2), levels = region_levels),
    panel = rep(panel, 2),
    component = factor(rep(c(labels[["current"]], labels[["new"]]), each = length(regions)),
                       levels = c(labels[["current"]], labels[["new"]])),
    value = c(v_current, v_new)
  )
  totals <- data.frame(
    region = factor(regions, levels = region_levels), panel = panel, value = v_total,
    label = if (pct) {
      scales::label_percent(scale = 1, accuracy = 0.1)(v_total)
    } else {
      scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)(v_total)
    }
  )

  aes_bar <- if (vertical) {
    ggplot2::aes(x = .data$region, y = .data$value)
  } else {
    ggplot2::aes(x = .data$value, y = .data$region)
  }
  p <- ggplot2::ggplot(long, aes_bar) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data$component),
      position = ggplot2::position_stack(reverse = TRUE),
      width = 0.65, colour = "white", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = totals, ggplot2::aes(label = .data$label),
      hjust = if (vertical) 0.5 else -0.15, vjust = if (vertical) -0.6 else 0.5,
      size = base_size * 0.3, colour = "grey20"
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(c(colours[["current"]], colours[["new"]]),
                               c(labels[["current"]], labels[["new"]])),
      name = NULL
    ) +
    .category_scale(vertical) +
    ggplot2::labs(title = title)
  if (!(pct && "target_pct" %in% names(gap))) {
    p <- p + .value_scale(metric, axis = if (vertical) "y" else "x")
  }
  value_title <- .metric_title(metric, units, "of planning area")
  p <- p + if (vertical) ggplot2::labs(x = NULL, y = value_title) else ggplot2::labs(x = value_title, y = NULL)

  if (pct && "target_pct" %in% names(gap)) {
    target <- unique(gap$target_pct)[1]
    p <- p +
      .value_scale(metric, axis = if (vertical) "y" else "x", target = target) +
      if (vertical) {
        ggplot2::geom_hline(yintercept = target, linetype = "dashed", colour = "grey20", linewidth = 0.4)
      } else {
        ggplot2::geom_vline(xintercept = target, linetype = "dashed", colour = "grey20", linewidth = 0.4)
      }
  }

  # Set the national bar apart in its own narrow panel
  if (any(is_national) && !all(is_national)) {
    p <- p + if (vertical) {
      ggplot2::facet_grid(cols = ggplot2::vars(.data$panel), scales = "free_x", space = "free_x")
    } else {
      ggplot2::facet_grid(rows = ggplot2::vars(.data$panel), scales = "free_y", space = "free_y")
    }
  }

  p +
    ggplot2::coord_cartesian(clip = "off") +
    .results_theme(base_size) +
    .category_axis_theme(vertical, length(regions), regions) +
    ggplot2::theme(
      strip.text = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1.2, "lines"),
      plot.margin = ggplot2::margin(18, 10, 5, 5)
    )
}

#' Regional summary panel
#'
#' Combines a map of one region's actions with bar charts of the share and
#' area of each action, in a single figure ready to save or put in a report.
#' [elsar_plot_regions()] makes one panel per region.
#'
#' @inheritParams elsar_summarise_actions
#' @param summary A tibble from [elsar_summarise_actions()] computed with the
#'   same `regions` and `region_col`.
#' @param regions An `sf` or `SpatVector` of regions.
#' @param region_col Character. Column in `regions` holding region names.
#' @param region Character. Name of the region to plot.
#' @param metrics Which bar charts to show next to the map: any of
#'   `"percent"` and `"area"`. Default both.
#' @param orientation `"vertical"` (default) or `"horizontal"` bars.
#' @param map_width Numeric between 0 and 1. Share of the figure width taken by
#'   the map. Default `0.45`.
#' @param title Plot title. Defaults to the region name.
#' @param ... Passed to [elsar_plot_actions()] (e.g. `show_pas`,
#'   `pa_threshold`, `maxcell`).
#' @param pa_colour,pa_label Colour and label of existing protected areas, used
#'   by both the map and the bars.
#' @param base_size Base font size in points.
#'
#' @return A `patchwork` object; for `elsar_plot_regions()`, a named list of
#'   them, one per region.
#' @seealso [elsar_save_plots()], [elsar_export_results()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
#' elsar_plot_region(x, s, ex$regions, "region_name", "Eastern Delta")
elsar_plot_region <- function(x,
                              summary,
                              regions,
                              region_col,
                              region,
                              metrics = c("percent", "area"),
                              orientation = c("vertical", "horizontal"),
                              map_width = 0.45,
                              title = region,
                              ...,
                              pa_colour = "#005000",
                              pa_label = "Existing PAs",
                              base_size = 10) {
  .check_summary(summary)
  metrics <- match.arg(metrics, c("percent", "area"), several.ok = TRUE)
  orientation <- match.arg(orientation)
  assertthat::assert_that(
    is.numeric(map_width), map_width > 0, map_width < 1,
    msg = "'map_width' must be between 0 and 1."
  )
  regions <- .prepare_regions(regions, region_col, x)
  assertthat::assert_that(
    region %in% regions[[region_col]],
    msg = paste0("Region '", region, "' not found in column '", region_col, "'.")
  )
  assertthat::assert_that(
    region %in% summary$region,
    msg = paste0("Region '", region, "' not found in 'summary'.")
  )

  poly <- regions[regions[[region_col]] == region, ]
  poly <- sf::st_sf(geometry = sf::st_union(poly))
  v <- terra::vect(poly)
  x_region <- terra::mask(terra::crop(x, v, snap = "out"), v)

  p_map <- elsar_plot_actions(
    x_region, regions = poly, legend = FALSE,
    pa_colour = pa_colour, pa_label = pa_label, base_size = base_size, ...
  )

  region_summary <- summary[summary$region == region, ]
  attributes_to_keep <- c("units", "has_pa")
  for (a in attributes_to_keep) attr(region_summary, a) <- attr(summary, a)
  split_pa <- isTRUE(attr(summary, "has_pa"))

  # The bars carry the shared legend, including the no-action key for the map
  d <- .bar_data(region_summary, split_pa, pa_label)
  colours <- .segment_colours(region_summary, split_pa, pa_colour, pa_label, include_no_action = TRUE)
  # Zero-length rows so no-action values (drawn on the map) get a legend key
  no_action <- setdiff(names(colours), unique(d$segment))
  if (length(no_action) > 0) {
    d <- rbind(d, data.frame(
      region = d$region[1], group = d$group[1], action = no_action,
      segment = no_action, area = 0, percent = 0
    ))
  }
  bars <- lapply(seq_along(metrics), function(i) {
    b <- .bar_plot(d, colours, metrics[i], "action", attr(summary, "units"), NULL, base_size,
                   legend_breaks = names(colours), orientation = orientation)
    # Horizontal bars share the action labels, so only the first chart shows
    # them; the right margin keeps the last axis label clear of the next chart
    if (i > 1 && orientation == "horizontal") {
      b <- b + ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
    b + ggplot2::theme(plot.margin = ggplot2::margin(5, 18, 5, 5))
  })

  right <- patchwork::wrap_plots(bars, nrow = 1)
  (p_map | right) +
    patchwork::plot_layout(widths = c(map_width, 1 - map_width), guides = "collect") +
    patchwork::plot_annotation(
      title = title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = base_size * 1.4, face = "bold")
      )
    ) &
    ggplot2::theme(legend.position = "bottom")
}

#' @rdname elsar_plot_region
#' @export
elsar_plot_regions <- function(x, summary, regions, region_col, ...) {
  .check_summary(summary)
  names_in_summary <- levels(summary$region) %||% unique(as.character(summary$region))
  plots <- lapply(names_in_summary, function(r) {
    elsar_plot_region(x, summary, regions, region_col, r, ...)
  })
  stats::setNames(plots, names_in_summary)
}

# Internal plotting helpers -------------------------------------------------------

.results_theme <- function(base_size) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = "grey30"),
      axis.title = ggplot2::element_text(colour = "grey30", size = base_size * 0.9),
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )
}

.wrap <- function(x, width = 14) stringr::str_wrap(x, width = width)

.metric_title <- function(metric, units, what = "of region") {
  if (metric == "percent") {
    paste("%", what)
  } else {
    paste0("Area (", c(ha = "ha", km2 = "km\u00b2", m2 = "m\u00b2")[[units]], ")")
  }
}

.value_scale <- function(metric, axis = c("x", "y"), target = NULL) {
  axis <- match.arg(axis)
  labels <- if (metric == "percent") {
    scales::label_percent(scale = 1)
  } else {
    scales::label_number(scale_cut = scales::cut_short_scale())
  }
  scale <- if (axis == "x") ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
  # A target is labelled on the opposite axis, where it never overlaps a bar
  sec <- if (is.null(target)) {
    ggplot2::waiver()
  } else {
    ggplot2::dup_axis(breaks = target, labels = paste0("Target\n", target, "%"), name = NULL)
  }
  scale(labels = labels, expand = ggplot2::expansion(mult = c(0, 0.12)), sec.axis = sec)
}

.category_scale <- function(vertical, width = 10) {
  if (vertical) {
    ggplot2::scale_x_discrete(labels = function(x) .wrap(x, width))
  } else {
    ggplot2::scale_y_discrete(labels = function(x) .wrap(x, width + 12))
  }
}

# Recessive grid along the value axis only; tilt crowded vertical labels
.category_axis_theme <- function(vertical, n, labels) {
  if (!vertical) {
    return(ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()))
  }
  crowded <- n > 6 || (n > 4 && max(nchar(as.character(labels)), 0) > 12)
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.x = if (crowded) {
      ggplot2::element_text(angle = 40, hjust = 1, vjust = 1)
    } else {
      ggplot2::element_text()
    }
  )
}

# Long data for stacked bars: one row per region x segment
.bar_data <- function(summary, split_pa, pa_label) {
  s <- summary[summary$is_action, ]
  group_levels <- unique(s$group[order(as.integer(s$action))])
  base <- data.frame(
    region = s$region,
    group = factor(s$group, levels = group_levels),
    action = as.character(s$action)
  )
  if (split_pa) {
    share_in <- ifelse(s$area > 0, s$area_in_pa / s$area, 0)
    rbind(
      transform(base, segment = pa_label, area = s$area_in_pa,
                percent = s$pct_of_region * share_in),
      transform(base, segment = base$action, area = s$area_outside_pa,
                percent = s$pct_of_region * (1 - share_in))
    )
  } else {
    transform(base, segment = base$action, area = s$area, percent = s$pct_of_region)
  }
}

.segment_colours <- function(summary, split_pa, pa_colour, pa_label, include_no_action = FALSE) {
  sch <- unique(summary[, c("action", "colour", "is_action")])
  sch <- sch[order(as.integer(sch$action)), ]
  if (!include_no_action) sch <- sch[sch$is_action, ]
  cols <- stats::setNames(sch$colour, as.character(sch$action))
  if (split_pa) cols <- c(stats::setNames(pa_colour, pa_label), cols)
  cols
}

.bar_plot <- function(d, colours, metric, by, units, title, base_size,
                      legend_breaks = NULL, orientation = "vertical") {
  vertical <- orientation == "vertical"
  d$segment <- factor(d$segment, levels = names(colours))
  d$value <- d[[metric]]
  multi_region <- length(unique(d$region)) > 1
  breaks <- legend_breaks %||% intersect(names(colours), as.character(unique(d$segment)))

  # Category axis: regions, or action groups (with a panel per region)
  cat_levels <- if (by == "region") levels(factor(d$region)) else levels(d$group)
  d$category <- factor(if (by == "region") as.character(d$region) else as.character(d$group),
                       levels = if (vertical) cat_levels else rev(cat_levels))

  mapping <- if (vertical) {
    ggplot2::aes(x = .data$category, y = .data$value, fill = .data$segment)
  } else {
    ggplot2::aes(x = .data$value, y = .data$category, fill = .data$segment)
  }
  value_title <- .metric_title(metric, units)

  p <- ggplot2::ggplot(d, mapping) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width = 0.7, colour = "white", linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(
      values = colours, breaks = breaks, labels = .wrap(breaks),
      name = NULL, drop = FALSE
    ) +
    .category_scale(vertical) +
    .value_scale(metric, axis = if (vertical) "y" else "x") +
    ggplot2::labs(title = title) +
    (if (vertical) ggplot2::labs(x = NULL, y = value_title) else ggplot2::labs(x = value_title, y = NULL))

  if (by == "action" && multi_region) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$region))
  }

  p +
    .results_theme(base_size) +
    .category_axis_theme(vertical, length(cat_levels), cat_levels) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
}

.pa_outline <- function(pa, threshold) {
  m <- terra::ifel(pa >= threshold, 1, NA)
  if (all(is.na(terra::minmax(m)))) return(NULL)
  sf::st_as_sf(terra::as.polygons(m, dissolve = TRUE))
}

.regions_as_sf <- function(regions, x) {
  if (inherits(regions, "SpatVector")) regions <- sf::st_as_sf(regions)
  assertthat::assert_that(inherits(regions, "sf"), msg = "'regions' must be an sf or SpatVector object.")
  sf::st_transform(regions, terra::crs(x))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    regions <- .as_sf(regions, x)
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
#' @param by `"region"` (default) draws one horizontal bar per region, stacked
#'   by action, to compare regions. `"action"` draws one bar per action group,
#'   with a panel per region if there are several.
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
                                   title = NULL,
                                   base_size = 10) {
  .check_summary(summary)
  metric <- match.arg(metric)
  by <- match.arg(by)
  if (split_pa && !isTRUE(attr(summary, "has_pa"))) {
    warning("The summary has no protected-area information; 'split_pa' ignored.", call. = FALSE)
    split_pa <- FALSE
  }

  d <- .bar_data(summary, split_pa, pa_label)
  colours <- .segment_colours(summary, split_pa, pa_colour, pa_label)
  .bar_plot(d, colours, metric, by, attr(summary, "units"), title, base_size)
}

#' Plot current and proposed protection against a target
#'
#' Draws the output of [elsar_protection_gap()] as one horizontal bar per
#' region: existing protected areas plus the new protection proposed by ELSA,
#' with a dashed line at the target.
#'
#' @param gap A tibble from [elsar_protection_gap()].
#' @param metric `"percent"` (default) or `"area"`. The target line is only
#'   drawn for `"percent"`.
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
#' elsar_plot_protection_gap(elsar_protection_gap(s))
elsar_plot_protection_gap <- function(gap,
                                      metric = c("percent", "area"),
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
  units <- attr(gap, "units") %||% "ha"

  region_levels <- levels(factor(gap$region))
  long <- rbind(
    data.frame(region = gap$region, component = labels[["current"]],
               value = if (metric == "percent") gap$pct_current else gap$current),
    data.frame(region = gap$region, component = labels[["new"]],
               value = if (metric == "percent") gap$pct_new else gap$new)
  )
  long$region <- factor(long$region, levels = region_levels)
  long$component <- factor(long$component, levels = c(labels[["current"]], labels[["new"]]))

  totals <- data.frame(
    region = factor(gap$region, levels = region_levels),
    value = if (metric == "percent") gap$pct_total else gap$total
  )
  totals$label <- if (metric == "percent") {
    scales::label_percent(scale = 1, accuracy = 0.1)(totals$value)
  } else {
    scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)(totals$value)
  }

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$region)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data$component),
      position = ggplot2::position_stack(reverse = TRUE),
      width = 0.65, colour = "white", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = totals, ggplot2::aes(label = .data$label),
      hjust = -0.15, size = base_size * 0.3, colour = "grey20"
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(c(colours[["current"]], colours[["new"]]),
                               c(labels[["current"]], labels[["new"]])),
      name = NULL
    ) +
    ggplot2::scale_y_discrete(limits = rev(region_levels), labels = .wrap) +
    ggplot2::labs(x = .metric_title(metric, units, "of planning area"), y = NULL, title = title)

  if (metric == "percent" && "target_pct" %in% names(gap)) {
    target <- unique(gap$target_pct)[1]
    p <- p +
      ggplot2::geom_vline(xintercept = target, linetype = "dashed",
                          colour = "grey20", linewidth = 0.4) +
      ggplot2::annotate(
        "text", x = target, y = Inf, label = paste0("Target ", target, "%"),
        vjust = -0.5, hjust = 0.5, size = base_size * 0.3, colour = "grey20"
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }

  p +
    .value_scale(metric, max(c(totals$value, if (metric == "percent") gap$target_pct), na.rm = TRUE)) +
    .results_theme(base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
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
                              map_width = 0.45,
                              title = region,
                              ...,
                              pa_colour = "#005000",
                              pa_label = "Existing PAs",
                              base_size = 10) {
  .check_summary(summary)
  metrics <- match.arg(metrics, c("percent", "area"), several.ok = TRUE)
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
                   legend_breaks = names(colours))
    # Only the first chart needs the action labels; the right margin keeps the
    # last axis label clear of the next chart
    if (i > 1) b <- b + ggplot2::theme(axis.text.y = ggplot2::element_blank())
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

.value_scale <- function(metric, max_value = NA) {
  labels <- if (metric == "percent") {
    scales::label_percent(scale = 1)
  } else {
    scales::label_number(scale_cut = scales::cut_short_scale())
  }
  ggplot2::scale_x_continuous(labels = labels, expand = ggplot2::expansion(mult = c(0, 0.12)))
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

.bar_plot <- function(d, colours, metric, by, units, title, base_size, legend_breaks = NULL) {
  d$segment <- factor(d$segment, levels = names(colours))
  d$value <- d[[metric]]
  multi_region <- length(unique(d$region)) > 1
  breaks <- legend_breaks %||% intersect(names(colours), as.character(unique(d$segment)))

  if (by == "region") {
    region_levels <- levels(factor(d$region))
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$value, y = .data$region, fill = .data$segment)) +
      ggplot2::scale_y_discrete(limits = rev(region_levels), labels = .wrap) +
      ggplot2::labs(x = .metric_title(metric, units), y = NULL)
    grid_off <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$value, y = .data$group, fill = .data$segment)) +
      ggplot2::scale_y_discrete(limits = rev(levels(d$group)), labels = .wrap) +
      ggplot2::labs(x = .metric_title(metric, units), y = NULL)
    if (multi_region) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$region))
    grid_off <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  p +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width = 0.7, colour = "white", linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(
      values = colours, breaks = breaks, labels = .wrap(breaks),
      name = NULL, drop = FALSE
    ) +
    .value_scale(metric) +
    ggplot2::labs(title = title) +
    .results_theme(base_size) +
    grid_off +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
}

.pa_outline <- function(pa, threshold) {
  m <- terra::ifel(pa >= threshold, 1, NA)
  if (all(is.na(terra::minmax(m)))) return(NULL)
  sf::st_as_sf(terra::as.polygons(m, dissolve = TRUE))
}

.as_sf <- function(regions, x) {
  if (inherits(regions, "SpatVector")) regions <- sf::st_as_sf(regions)
  assertthat::assert_that(inherits(regions, "sf"), msg = "'regions' must be an sf or SpatVector object.")
  sf::st_transform(regions, terra::crs(x))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

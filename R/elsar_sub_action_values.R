# What lies inside each action, and how robust sub-actions are ----------------------
#
# Value summaries: how the area of each action or sub-action splits across the
# classes of a context layer (e.g. crop suitability within sustainable
# agriculture). Sensitivity: how each sub-action's area changes when one
# threshold, Protect setting or layer is changed at a time.

#' Summarise the values of layers within each action
#'
#' Splits the area of each action (or sub-action) into classes of one or more
#' layers, nationally or per region: for example, how much sustainable
#' agriculture lies on land with low, medium or high crop suitability, or
#' which ecoregions forest restoration falls in.
#'
#' Numeric layers are cut into classes at `breaks`. Categorical layers (e.g.
#' ecoregions as a factor raster) keep their own categories. Cells where a
#' layer has no value are reported in a "No data" class, so each action's
#' classes always add up to its area.
#'
#' @section Choosing breaks:
#' Fixed breaks (`breaks = c(0, 0.25, 0.5, 0.75, 1)`) mean the same thing in
#' every run and country, so prefer them when results will be compared. A
#' single number asks for that many classes with equal numbers of action cells
#' (quantiles of the layer over all cells with an action), which suits layers
#' with no natural scale. Quantiles are computed once for all actions, so
#' every action is split by the same classes. The lowest and highest classes
#' also take any values below and above the breaks.
#'
#' @inheritParams elsar_summarise_actions
#' @param x A prepared action map, e.g. from [elsar_assign_sub_actions()] or
#'   [elsar_prepare_actions()].
#' @param layers The layers to summarise, as a `SpatRaster` (or path) with
#'   named layers, or a named list of `SpatRaster` objects. Rasters on another
#'   grid are resampled to the action grid.
#' @param breaks A single number of quantile classes (default 4), or a numeric
#'   vector of class limits; or a named list giving either per layer.
#'
#' @return A tibble with one row per region, layer, action and class, and
#'   columns `region`, `layer`, `action`, `group`, `colour`, `is_action`,
#'   `class` (a factor in class order, "No data" last), `lower` and `upper`
#'   (the class limits; `NA` for categories and "No data"), `area` (in
#'   `units`) and `pct_of_action` (the share of the action's area in the region
#'   that falls in the class).
#' @seealso [elsar_plot_values()], [elsar_summarise_overlap()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions)
#' suitability <- terra::init(x[["action"]], "y") / 60000
#' v <- elsar_summarise_values(x, c(suitability = suitability), breaks = c(0, 0.25, 0.5, 0.75, 1))
#' v[v$action == "Manage", c("class", "area", "pct_of_action")]
elsar_summarise_values <- function(x,
                                   layers,
                                   breaks = 4,
                                   regions = NULL,
                                   region_col = NULL,
                                   units = c("ha", "km2", "m2"),
                                   national_label = .txt("national")) {
  scheme <- .scheme_from_actions(x)
  units <- match.arg(units)
  unit_factor <- c(m2 = 1, ha = 1e-4, km2 = 1e-6)[[units]]
  layers <- .condition_layers(layers, x)
  layer_names <- names(layers)
  assertthat::assert_that(length(layers) > 0, msg = "Give at least one layer in 'layers'.")
  breaks <- .breaks_per_layer(breaks, layer_names)

  action <- x[["action"]]
  levels(action) <- NULL
  is_action_value <- scheme$value[scheme$is_action]
  action_cells <- terra::values(action, mat = FALSE) %in% is_action_value
  cell_area <- .cell_area(x)

  weights <- list()
  classes <- list()
  for (nm in layer_names) {
    cl <- .classify_layer(layers[[nm]], breaks[[nm]], action_cells, nm)
    classes[[nm]] <- cl$classes
    for (k in seq_len(nrow(cl$classes))) {
      w <- cell_area * terra::ifel(cl$raster == k, 1, 0)
      names(w) <- paste0(".v", match(nm, layer_names), "_", k)
      weights[[length(weights) + 1]] <- w
    }
    no_data <- cell_area * terra::ifel(is.na(cl$raster), 1, 0)
    names(no_data) <- paste0(".v", match(nm, layer_names), "_na")
    weights[[length(weights) + 1]] <- no_data
  }
  weights <- terra::rast(weights)

  zs <- .action_zonal_sums(x, weights, regions, region_col, national_label)
  grid <- expand.grid(region = zs$region_names, value = scheme$value, stringsAsFactors = FALSE)
  wide <- dplyr::left_join(grid, zs$sums, by = c("region", "value"))

  out <- lapply(layer_names, function(nm) {
    i <- match(nm, layer_names)
    cls <- classes[[nm]]
    ids <- c(paste0(".v", i, "_", seq_len(nrow(cls))), paste0(".v", i, "_na"))
    labels <- c(cls$label, .txt("no_data"))
    rows <- lapply(seq_along(ids), function(k) {
      data.frame(
        region = wide$region, value = wide$value, layer = nm,
        class = labels[k],
        lower = if (k <= nrow(cls)) cls$lower[k] else NA_real_,
        upper = if (k <= nrow(cls)) cls$upper[k] else NA_real_,
        area = dplyr::coalesce(wide[[ids[k]]], 0) * unit_factor,
        stringsAsFactors = FALSE
      )
    })
    d <- do.call(rbind, rows)
    d$class <- factor(d$class, levels = labels)
    d
  })
  out <- do.call(rbind, out) |>
    dplyr::left_join(scheme, by = "value") |>
    dplyr::group_by(.data$region, .data$layer, .data$value) |>
    dplyr::mutate(pct_of_action = .safe_pct(.data$area, sum(.data$area))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      region = factor(.data$region, levels = zs$region_names),
      layer = factor(.data$layer, levels = layer_names),
      action = factor(.data$label, levels = scheme$label)
    ) |>
    dplyr::arrange(.data$region, .data$layer, .data$action, .data$class) |>
    dplyr::select("region", "layer", "action", "group", "colour", "is_action", "class",
                  "lower", "upper", "area", "pct_of_action") |>
    tibble::as_tibble()
  attr(out, "units") <- units
  out
}

#' Plot the values of a layer within each action
#'
#' Draws the output of [elsar_summarise_values()] as one 100% bar per action,
#' split by the classes of a layer.
#'
#' @param values A tibble from [elsar_summarise_values()].
#' @param layer Name of the layer to plot. Defaults to the first.
#' @param show_no_action Logical. Include no-action values. Default `FALSE`.
#' @param palette A palette name for [grDevices::hcl.colors()] for the
#'   classes; "No data" is always grey.
#' @param title Optional plot title. Defaults to the layer name.
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object, with a panel per region if there are several.
#' @seealso [elsar_summarise_values()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions)
#' suitability <- terra::init(x[["action"]], "y") / 60000
#' v <- elsar_summarise_values(x, c(suitability = suitability))
#' elsar_plot_values(v)
elsar_plot_values <- function(values,
                              layer = NULL,
                              show_no_action = FALSE,
                              palette = "YlGnBu",
                              title = NULL,
                              base_size = 11) {
  assertthat::assert_that(
    is.data.frame(values), all(c("layer", "action", "class", "pct_of_action") %in% names(values)),
    msg = "'values' must be created with elsar_summarise_values()."
  )
  layers <- levels(values$layer)
  layer <- layer %||% layers[1]
  assertthat::assert_that(
    assertthat::is.string(layer), layer %in% layers,
    msg = paste0("'layer' must be one of: ", toString(layers), ".")
  )
  d <- values[values$layer == layer, ]
  if (!show_no_action) d <- d[d$is_action, ]
  # Actions with no area have no shares to draw
  totals <- tapply(d$area, as.character(d$action), sum)
  d <- d[as.character(d$action) %in% names(totals)[totals > 0], ]
  d <- droplevels(d)
  d$action <- factor(d$action, levels = rev(levels(d$action)))

  class_levels <- levels(d$class)
  n <- length(class_levels) - 1
  fills <- c(grDevices::hcl.colors(max(n, 1), palette, rev = TRUE)[seq_len(n)], "grey80")
  names(fills) <- class_levels

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$pct_of_action, y = .data$action, fill = .data$class)) +
    ggplot2::geom_col(width = 0.7, colour = "white", linewidth = 0.2,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = fills, name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = .label_pct(), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_y_discrete(labels = function(z) .wrap(z, 24)) +
    ggplot2::labs(x = .txt("pct_of_action"), y = NULL, title = title %||% layer) +
    .results_theme(base_size) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = .legend_rows(length(class_levels))))
  if (length(unique(d$region)) > 1) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$region))
  p
}

#' Test how sensitive sub-actions are to their settings
#'
#' Changes one setting at a time and reports how much each sub-action's area
#' changes from the baseline. Use it to say how robust each figure is, e.g.
#' "riparian restoration is 27,000 km² with a 1 km riparian zone, and
#' 18,000–41,000 km² with 0.5–2 km".
#'
#' @section What can be varied:
#' Each element of `vary` is one setting, varied while the others stay at
#' their baseline:
#' \describe{
#'   \item{A named threshold}{A name in `params` that conditions use, with
#'     the values to try, e.g. `list(riparian_min = c(0.25, 0.75))` for a rule
#'     `riparian > riparian_min`.}
#'   \item{A Protect setting}{`pa_threshold`, `expansion_km` or
#'     `min_new_km2` of [add_protect_sub_actions()], applied to every Protect
#'     split in `rules`.}
#'   \item{A layer}{A name in `layers` with a named list of alternative
#'     layers, e.g. `list(riparian = list("500 m" = rip_500, "2 km" = rip_2k))`.}
#' }
#'
#' @inheritParams elsar_assign_sub_actions
#' @param vary A named list of settings to vary (see below).
#' @param units Character. Area units: `"ha"` (default), `"km2"` or `"m2"`.
#'
#' @return A tibble with one row per scenario and sub-action, and columns
#'   `parameter`, `value` (the setting tried, as text), `is_baseline`,
#'   `parent`, `sub_action`, `area`, `baseline_area`, `change` (`area -
#'   baseline_area`) and `pct_change`. The baseline appears once, with
#'   `parameter` "Baseline".
#' @seealso [elsar_plot_sensitivity()], [elsar_sub_action_diagnostics()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' riparian <- 1 - abs(terra::init(x[["action"]], "y") - 15000) / 15000
#' rules <- elsar_sub_actions("Restore", sub_action("Riparian restoration", riparian > riparian_min)) |>
#'   add_protect_sub_actions()
#' s <- elsar_sub_action_sensitivity(
#'   x, rules, c(riparian = riparian),
#'   params = list(riparian_min = 0.5),
#'   vary = list(riparian_min = c(0.25, 0.75), expansion_km = c(2, 5)),
#'   units = "km2"
#' )
#' s[, c("parameter", "value", "sub_action", "area", "pct_change")]
elsar_sub_action_sensitivity <- function(x,
                                         rules,
                                         layers = NULL,
                                         vary,
                                         params = list(),
                                         units = c("ha", "km2", "m2")) {
  units <- match.arg(units)
  .check_sub_action_rules(rules)
  assertthat::assert_that(
    is.list(vary), length(vary) > 0, !is.null(names(vary)), all(nzchar(names(vary))),
    !anyDuplicated(names(vary)),
    msg = "'vary' must be a named list of settings, e.g. list(riparian_min = c(0.25, 0.75))."
  )
  # Convert the layers once, not per scenario
  layers <- .condition_layers(layers, x)
  protect_settings <- c("pa_threshold", "expansion_km", "min_new_km2")

  scenarios <- list(list(parameter = .txt("baseline"), value = "", rules = rules,
                         layers = layers, params = params))
  for (nm in names(vary)) {
    v <- vary[[nm]]
    if (nm %in% protect_settings) {
      assertthat::assert_that(
        any(rules$kind == "protect"),
        msg = paste0("'", nm, "' is a Protect setting, but the rules have no Protect split.")
      )
      assertthat::assert_that(
        is.numeric(v), length(v) > 0,
        msg = paste0("'vary$", nm, "' must be numbers.")
      )
      if (nm == "min_new_km2") {
        has_small <- vapply(rules$params[rules$kind == "protect"], function(p) !is.null(p$min_new_km2), logical(1))
        assertthat::assert_that(
          all(has_small),
          msg = "To vary 'min_new_km2', the Protect split must set it (and so report small new areas)."
        )
      }
      for (val in v) {
        r <- rules
        r$params[r$kind == "protect"] <- lapply(r$params[r$kind == "protect"], function(p) {
          p[[nm]] <- val
          p
        })
        scenarios[[length(scenarios) + 1]] <- list(parameter = nm, value = format(val), rules = r,
                                                   layers = layers, params = params)
      }
    } else if (nm %in% names(layers)) {
      assertthat::assert_that(
        is.list(v), !inherits(v, c("sf", "SpatVector")), !is.null(names(v)), all(nzchar(names(v))),
        msg = paste0("'vary$", nm, "' must be a named list of alternative layers.")
      )
      alternatives <- .condition_layers(v, x)
      for (alt in names(alternatives)) {
        l <- layers
        l[[nm]] <- alternatives[[alt]]
        scenarios[[length(scenarios) + 1]] <- list(parameter = nm, value = alt, rules = rules,
                                                   layers = l, params = params)
      }
    } else if (nm %in% names(params)) {
      assertthat::assert_that(
        is.atomic(v), length(v) > 0,
        msg = paste0("'vary$", nm, "' must be a vector of values to try.")
      )
      for (val in v) {
        p <- params
        p[[nm]] <- val
        scenarios[[length(scenarios) + 1]] <- list(parameter = nm, value = format(val), rules = rules,
                                                   layers = layers, params = p)
      }
    } else {
      stop("Cannot vary '", nm, "': it is not a name in 'params', a name in 'layers', or a Protect ",
           "setting (", toString(protect_settings), ").", call. = FALSE)
    }
  }

  parents <- unique(rules$parent)
  results <- lapply(scenarios, function(sc) {
    sub <- elsar_assign_sub_actions(x, sc$rules, sc$layers, params = sc$params)
    s <- elsar_summarise_actions(sub, units = units)
    s <- s[s$is_action & s$group %in% parents, ]
    tibble::tibble(parameter = sc$parameter, value = sc$value,
                   parent = s$group, sub_action = as.character(s$action), area = s$area)
  })
  base <- results[[1]][, c("sub_action", "area")]
  names(base)[2] <- "baseline_area"
  out <- do.call(rbind, results) |>
    dplyr::left_join(base, by = "sub_action") |>
    dplyr::mutate(
      is_baseline = seq_len(dplyr::n()) <= nrow(results[[1]]),
      baseline_area = dplyr::coalesce(.data$baseline_area, 0),
      change = .data$area - .data$baseline_area,
      pct_change = .safe_pct(.data$change, .data$baseline_area)
    ) |>
    dplyr::select("parameter", "value", "is_baseline", "parent", "sub_action",
                  "area", "baseline_area", "change", "pct_change")
  attr(out, "units") <- units
  out
}

#' Plot how sensitive sub-actions are to their settings
#'
#' Draws the output of [elsar_sub_action_sensitivity()]: for each sub-action,
#' its baseline area (black diamond) and its area under each setting tried
#' that changes it, coloured by setting and labelled with the value tried.
#' Settings that leave a sub-action unchanged are not drawn for it, so a
#' sub-action with only a diamond is robust to everything tried. Settings that
#' changed no sub-action at all are named in the subtitle.
#'
#' Areas are drawn, not percentage changes, because a small sub-action can
#' change by a large percentage while moving little land.
#'
#' @param sensitivity A tibble from [elsar_sub_action_sensitivity()].
#' @param title Optional plot title.
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object, with a panel per parent action.
#' @seealso [elsar_sub_action_sensitivity()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' s <- elsar_sub_action_sensitivity(x, add_protect_sub_actions(),
#'                                   vary = list(expansion_km = c(2, 5, 10)), units = "km2")
#' elsar_plot_sensitivity(s)
elsar_plot_sensitivity <- function(sensitivity, title = NULL, base_size = 11) {
  assertthat::assert_that(
    is.data.frame(sensitivity),
    all(c("parameter", "value", "is_baseline", "sub_action", "area", "change") %in% names(sensitivity)),
    msg = "'sensitivity' must be created with elsar_sub_action_sensitivity()."
  )
  units <- attr(sensitivity, "units") %||% "ha"
  sub_levels <- unique(sensitivity$sub_action)
  base <- sensitivity[sensitivity$is_baseline, ]
  alt <- sensitivity[!sensitivity$is_baseline & abs(sensitivity$change) > 1e-9, ]
  base$sub_action <- factor(base$sub_action, levels = rev(sub_levels))
  alt$sub_action <- factor(alt$sub_action, levels = rev(sub_levels))
  tried <- unique(sensitivity$parameter[!sensitivity$is_baseline])
  unchanged <- setdiff(tried, unique(alt$parameter))
  alt$parameter <- factor(alt$parameter, levels = setdiff(tried, unchanged))
  # Plain text: PDF devices cannot draw every symbol (e.g. a diamond character)
  subtitle <- if (length(unchanged) > 0) paste0(toString(unchanged), ": no change") else NULL
  base$key <- .txt("baseline")
  dodge <- ggplot2::position_dodge(width = 0.5)

  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$area, y = .data$sub_action)) +
    ggplot2::geom_point(data = base, ggplot2::aes(shape = .data$key), size = 3.6, colour = "black") +
    ggplot2::scale_shape_manual(values = 18, name = NULL) +
    ggplot2::geom_point(data = alt, ggplot2::aes(colour = .data$parameter, group = .data$parameter),
                        size = 2.2, position = dodge) +
    ggplot2::geom_text(data = alt, ggplot2::aes(label = .data$value, colour = .data$parameter,
                                                group = .data$parameter),
                       size = base_size * 0.25, vjust = -0.9, position = dodge, show.legend = FALSE) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL) +
    ggplot2::scale_x_continuous(labels = .label_area(), expand = ggplot2::expansion(mult = c(0.02, 0.08)),
                                limits = c(0, NA)) +
    ggplot2::scale_y_discrete(labels = function(z) .wrap(z, 24)) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$parent), scales = "free_y", space = "free_y") +
    ggplot2::labs(x = .metric_title("area", units), y = NULL, title = title,
                  subtitle = subtitle) +
    .results_theme(base_size)
}

# Internal helpers ---------------------------------------------------------------

.breaks_per_layer <- function(breaks, layer_names) {
  check_one <- function(b, nm) {
    ok <- is.numeric(b) && !anyNA(b) &&
      ((length(b) == 1 && b >= 2 && b == round(b)) || (length(b) >= 2 && !is.unsorted(b, strictly = TRUE)))
    assertthat::assert_that(
      ok,
      msg = paste0("'breaks' for '", nm, "' must be a whole number of classes (2 or more) ",
                   "or increasing class limits.")
    )
    b
  }
  if (is.list(breaks)) {
    assertthat::assert_that(
      !is.null(names(breaks)), all(layer_names %in% names(breaks)),
      msg = "A list of 'breaks' must name every layer."
    )
    return(stats::setNames(lapply(layer_names, function(nm) check_one(breaks[[nm]], nm)), layer_names))
  }
  stats::setNames(lapply(layer_names, function(nm) check_one(breaks, nm)), layer_names)
}

# Class raster (1..k, NA = no data) and the class table of one layer
.classify_layer <- function(l, breaks, action_cells, name) {
  if (terra::is.factor(l)) {
    ct <- terra::cats(l)[[1]]
    label_col <- which(vapply(ct, is.character, logical(1)))[1]
    if (is.na(label_col)) label_col <- 2
    codes <- ct[[1]]
    levels(l) <- NULL
    # Unique raw codes (unique() on a factor raster gives its labels)
    present <- intersect(codes, terra::unique(l)[[1]])
    ct <- ct[codes %in% present, , drop = FALSE]
    r <- terra::classify(l, cbind(ct[[1]], seq_len(nrow(ct))), others = NA)
    return(list(
      raster = r,
      classes = data.frame(label = as.character(ct[[label_col]]), lower = NA_real_, upper = NA_real_)
    ))
  }
  if (length(breaks) == 1) {
    v <- terra::values(l, mat = FALSE)[action_cells]
    v <- v[!is.na(v)]
    assertthat::assert_that(
      length(v) > 0,
      msg = paste0("Layer '", name, "' has no values in cells with an action, so quantiles cannot be computed.")
    )
    q <- unique(stats::quantile(v, probs = seq(0, 1, length.out = breaks + 1), names = FALSE, type = 7))
    if (length(q) < breaks + 1) {
      message("Layer '", name, "': ", breaks, " quantile classes collapse to ", max(length(q) - 1, 1),
              " because many cells share a value.")
    }
    if (length(q) == 1) q <- c(q, q + 1)
    breaks <- q
  }
  k <- length(breaks) - 1
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  edges <- c(-Inf, breaks[2:k], Inf)
  if (k == 1) edges <- c(-Inf, Inf)
  r <- terra::classify(l, cbind(edges[-length(edges)], edges[-1], seq_len(k)), right = FALSE)
  fmt <- function(z) formatC(signif(z, 3), format = "fg", digits = 3, flag = "")
  labels <- paste0(trimws(fmt(lower)), "–", trimws(fmt(upper)))
  list(raster = r, classes = data.frame(label = labels, lower = lower, upper = upper))
}

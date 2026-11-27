# Representation of features by an ELSA run ---------------------------------------

# Font list for interactive (browser) charts
.elsar_web_font <- 'Roboto, "Helvetica Neue", "Segoe UI", Arial, sans-serif'

#' Plot how well an ELSA run represents its features
#'
#' Draws the representation figures of an ELSA run as one stacked bar per
#' feature, split by action, and grouped by theme. This is the chart
#' shown in the ELSA tool and the national ELSA app.
#'
#' The function plots the figures it is given and computes nothing. Use the
#' run's own outputs: its effective representation (spatial coverage x action
#' impact, which can exceed 100%) or its spatial representation (the share of
#' each feature inside the areas selected). To measure features against the
#' action map alone, including layers that were not in the run, use
#' [elsar_summarise_overlap()] instead.
#'
#' @param representation A data frame of representation figures, in one of two
#'   shapes:
#'   * **wide**, as the ELSA tool and the national ELSA app export it: a
#'     feature column (`feature` or `data`), an optional `theme` column, and
#'     one column per action giving the percentage of the feature represented
#'     by that action. Headers may be in any supported language (e.g. `Datos`,
#'     `Tema`). An overall column (`overall`, `Agregar`, ...) is ignored; the
#'     totals are recomputed from the actions. Exports that carry both
#'     measures, as `"<action> (effective)"` and `"<action> (spatial)"` columns
#'     (suffixes in English or the export's language), are read for the
#'     `measure` chosen.
#'   * **long**, with columns `feature`, `action` and `pct` (percent), and
#'     optionally `theme`. The output of
#'     [prioritizr::eval_feature_representation_summary()] also works: its
#'     `summary` column is read as the action and `relative_held` (a
#'     proportion) is converted to percent. Its `"overall"` rows are dropped.
#'     Note that prioritizr measures each zone against the feature total of
#'     every zone combined, so for multi-zone problems the ELSA run's own
#'     figures are the ones to cite.
#'   * the output of [elsar_summarise_overlap()] for an action map and the
#'     feature layers: no ELSA tool or setup needed. It gives spatial
#'     representation (the share of each feature under each action), and
#'     with protected areas given to [elsar_prepare_actions()] the share
#'     already protected is split out of Protect. With several regions, the
#'     first (national) is drawn.
#' @param measure `"effective"` (default) or `"spatial"`: which columns to
#'   read from an export that has both. Ignored for other inputs.
#' @param scheme A tibble from [elsar_action_scheme()], used for action labels
#'   and colours. Actions are matched to it by name in any supported language
#'   or by the ELSA tool's slugs (e.g. `"reduce-pressures"` matches Manage),
#'   and drawn with the scheme's label. Actions not in the scheme (e.g.
#'   locked-in protection) keep their own name and take colours from
#'   `elsar_palette("categorical")`.
#' @param actions Optional character vector. The actions to draw, in legend
#'   order: column names (wide) or `action` values (long). Defaults to every
#'   action, those in the scheme first, in scheme order. Use it to leave out
#'   per-theme columns of multi-theme runs.
#' @param colours Optional named character vector of colours by action,
#'   overriding the scheme.
#' @param themes Optional named character vector giving each feature's theme,
#'   e.g. `c("Forest cover" = "Biodiversity")`. Used instead of a `theme`
#'   column.
#' @param sort How to order features within each theme: `"weakest"` (default)
#'   puts the least represented feature on top, `"strongest"` the most, and
#'   `"input"` keeps the order of `representation`.
#' @param allocated Optional number. The share of the planning area allocated
#'   to actions (%), drawn as a dashed reference line. A feature short of the
#'   line is represented worse than a plan of the same size would represent it
#'   by chance.
#' @param existing Coverage of each feature by existing protected areas (%),
#'   drawn as its own segment at the start of the Protect bar so the new
#'   protection ELSA adds stands apart. ELSA locks existing protected areas
#'   into Protect, so this share is taken out of Protect and the bar total
#'   is unchanged. One of:
#'   * `NULL` (default): read from the table's lock-in column if it has one,
#'     as the national ELSA app exports it ("Current Protection",
#'     "Protección actual", ..., values possibly in brackets), or an
#'     "Existing PAs" column or action;
#'   * a named numeric vector, feature = percent;
#'   * a data frame with `feature` and `pct` columns;
#'   * the output of [elsar_summarise_overlap()] for the feature layers with
#'     protected areas: each layer's share inside existing protected areas
#'     (national, or the first region) is used. Layer names must match the
#'     feature names;
#'   * `FALSE`: no split, even if the table has a lock-in column.
#'
#'   The rest of Protect is then labelled as new protection. The coverage
#'   should use the same measure as the plot; where it exceeds Protect it is
#'   capped, with a warning.
#' @param config Optional. The run's ELSA tool configuration (`config.json`
#'   path, or the parsed list), with its input layers in the same folder. It
#'   fills in what is not given: `existing` from
#'   [elsar_existing_protection()] (unless the table already has a lock-in
#'   column), and `allocated` as the sum of the action budgets. Platform
#'   (deployment) configurations work too, with their default budgets and
#'   lock-in.
#' @param layers_dir Folder holding the run's input layers named in
#'   `config`, searched recursively (e.g. the build's `layers/` folder).
#'   Defaults to the folder of `config`.
#' @param budgets Optional named numeric vector of each action's budget (% of
#'   the planning area), shown after its name in the legend, e.g.
#'   `c(protect = 30, manage = 5, restore = 18.56)`, giving "Protect (30.0%)". Names are matched to the
#'   actions like the table's columns (by name in any language or by slug).
#'   Filled in from `config` when not given. With existing protected areas
#'   split out, Protect's budget (which includes them) labels the rest of
#'   Protect, as in the ELSA tool.
#' @param pa_colour,pa_label Colour and legend label of the existing
#'   protected-area segment.
#' @param orientation `"horizontal"` (default) or `"vertical"` bars.
#' @param interactive Logical. `TRUE` (default) returns an interactive plotly
#'   chart: hover for each segment's value and the feature's total, and
#'   save a PNG with the camera button.
#'   Needs the plotly package; without it, the ggplot is returned with a
#'   message. `FALSE` returns a `ggplot`, for [ggplot2::ggsave()],
#'   [elsar_save_plots()] or further ggplot2 layers.
#' @param title Optional plot title. None by default.
#' @param label_width Characters per line before feature names wrap. Defaults
#'   to 40 for horizontal bars and 20 for vertical ones.
#' @param base_size Base font size in points.
#' @param font_family Font of the interactive chart, as a CSS font list: the
#'   first font installed on the viewer's computer is used. Defaults to
#'   Roboto, then Helvetica Neue, Segoe UI and Arial.
#'
#' @return A `plotly` htmlwidget, with a boxed panel per theme under a bold
#'   heading and the legend along the top,
#'   or with `interactive = FALSE` a `ggplot` with a panel per theme.
#' @seealso [elsar_summarise_overlap()], [elsar_action_scheme()]
#' @export
#'
#' @examples
#' # Representation as the ELSA tool exports it (percent per action)
#' rep <- data.frame(
#'   feature = c("Forest cover", "Wetlands", "Carbon", "Water provision"),
#'   theme = c("Biodiversity", "Biodiversity", "Climate", "Ecosystem services"),
#'   protect = c(42, 18, 30, 25),
#'   `reduce-pressures` = c(10, 6, 12, 20),
#'   restore = c(15, 30, 8, 9),
#'   check.names = FALSE
#' )
#' elsar_plot_representation(rep, allocated = 30)
#'
#' # A static ggplot, e.g. for ggplot2::ggsave()
#' elsar_plot_representation(rep, allocated = 30, interactive = FALSE)
#'
#' # The same figures in long form
#' long <- data.frame(
#'   feature = rep(rep$feature, 3),
#'   action = rep(c("Protect", "Manage", "Restore"), each = 4),
#'   pct = c(rep$protect, rep$`reduce-pressures`, rep$restore)
#' )
#' elsar_plot_representation(long, sort = "input", orientation = "vertical",
#'                           interactive = FALSE)
elsar_plot_representation <- function(representation,
                                      measure = c("effective", "spatial"),
                                      scheme = elsar_action_scheme(),
                                      actions = NULL,
                                      colours = NULL,
                                      themes = NULL,
                                      sort = c("weakest", "strongest", "input"),
                                      allocated = NULL,
                                      existing = NULL,
                                      config = NULL,
                                      layers_dir = NULL,
                                      budgets = NULL,
                                      pa_colour = "#005000",
                                      pa_label = .txt("existing_pas"),
                                      orientation = c("horizontal", "vertical"),
                                      interactive = TRUE,
                                      title = NULL,
                                      label_width = NULL,
                                      base_size = 10,
                                      font_family = .elsar_web_font) {
  sort <- match.arg(sort)
  measure <- match.arg(measure)
  orientation <- match.arg(orientation)
  vertical <- orientation == "vertical"
  .check_scheme(scheme)
  assertthat::assert_that(assertthat::is.flag(interactive), !is.na(interactive),
                          msg = "'interactive' must be TRUE or FALSE.")
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    message("Install the plotly package for the interactive chart; returning a ggplot.")
    interactive <- FALSE
  }
  assertthat::assert_that(
    is.null(allocated) || (is.numeric(allocated) && length(allocated) == 1 && !is.na(allocated)),
    msg = "'allocated' must be a single number (percent)."
  )
  assertthat::assert_that(
    is.null(themes) || (is.character(themes) && !is.null(names(themes))),
    msg = "'themes' must be a named character vector (feature = theme)."
  )

  d <- .representation_long(representation, actions, measure)
  detected <- attr(d, "existing")
  if (!is.null(config)) {
    cfg <- .read_elsa_config(config, layers_dir)
    if (is.null(allocated)) allocated <- .config_allocated(cfg$config)
    if (is.null(existing) && is.null(detected)) {
      existing <- elsar_existing_protection(cfg$config, dir = cfg$dir, measure = measure)
    }
  }
  if (!is.null(themes)) d$theme <- unname(themes[d$feature])
  has_themes <- !all(is.na(d$theme))
  if (has_themes) d$theme[is.na(d$theme)] <- "\u2014"

  # Labels and colours: scheme actions first, in scheme order
  found <- unique(d$action)
  in_scheme <- match(.label_key(found), .label_key(scheme$label))
  shown <- ifelse(is.na(in_scheme), found, scheme$label[in_scheme])
  seg <- data.frame(action = found, label = shown, scheme_pos = in_scheme,
                    input_pos = seq_along(found), stringsAsFactors = FALSE)
  seg <- if (is.null(actions)) {
    seg[order(is.na(seg$scheme_pos), seg$scheme_pos, seg$input_pos), ]
  } else {
    seg[match(actions, seg$action), ]
  }
  assertthat::assert_that(
    !anyDuplicated(seg$label),
    msg = paste0("Several actions match the same scheme action: ",
                 toString(seg$action[seg$label %in% seg$label[duplicated(seg$label)]]), ".")
  )
  seg$colour <- scheme$colour[seg$scheme_pos]
  unmatched <- which(is.na(seg$colour))
  if (length(unmatched) > 0) {
    seg$colour[unmatched] <- rep_len(elsar_palette("categorical"), length(unmatched))
  }
  pal <- stats::setNames(seg$colour, seg$label)
  if (!is.null(colours)) {
    assertthat::assert_that(
      is.character(colours), !is.null(names(colours)),
      msg = "'colours' must be a named character vector."
    )
    over <- names(colours)
    key <- ifelse(over %in% seg$action, seg$label[match(over, seg$action)], over)
    pal[key[key %in% names(pal)]] <- colours[key %in% names(pal)]
  }
  d$action <- factor(seg$label[match(d$action, seg$action)], levels = seg$label)

  # Existing protected areas, split out of Protect
  existing_pct <- if (isFALSE(existing)) NULL else if (is.null(existing)) detected else .existing_pct(existing)
  protect_label <- NULL
  if (!is.null(existing_pct)) {
    split <- .split_existing(d, pal, existing_pct, pa_colour, pa_label)
    d <- split$d
    pal <- split$pal
    protect_label <- split$protect
  }

  # Budgets after the action names in the legend
  if (is.null(budgets) && !is.null(config)) budgets <- .config_budgets(cfg$config)
  if (!is.null(budgets)) {
    relabelled <- .budget_labels(names(pal), budgets, seg, protect_label, pa_label)
    d$action <- factor(relabelled[as.character(d$action)], levels = unname(relabelled))
    names(pal) <- unname(relabelled)
  }

  # Feature order within themes; themes in order of first appearance
  totals <- stats::aggregate(pct ~ feature + theme, data = transform(d, theme = dplyr::coalesce(d$theme, "")),
                             FUN = sum)
  totals$input_pos <- match(totals$feature, unique(d$feature))
  totals$theme_pos <- match(totals$theme, unique(dplyr::coalesce(d$theme, "")))
  rank <- switch(sort, weakest = totals$pct, strongest = -totals$pct, input = totals$input_pos)
  ord <- totals$feature[order(totals$theme_pos, rank, totals$input_pos)]
  # Horizontal bars read top-down, so the first feature is the last level
  d$feature <- factor(d$feature, levels = if (vertical) ord else rev(ord))
  if (has_themes) d$theme <- factor(d$theme, levels = unique(d$theme))

  label_width <- label_width %||% (if (vertical) 20 else 40)
  assertthat::assert_that(assertthat::is.count(label_width),
                          msg = "'label_width' must be a positive whole number.")
  if (interactive) {
    return(.representation_plotly(d, pal, vertical, allocated, title, has_themes, base_size,
                                 font_family, label_width))
  }

  mapping <- if (vertical) {
    ggplot2::aes(x = .data$feature, y = .data$pct, fill = .data$action)
  } else {
    ggplot2::aes(x = .data$pct, y = .data$feature, fill = .data$action)
  }
  sec <- if (is.null(allocated)) {
    ggplot2::waiver()
  } else {
    ggplot2::dup_axis(breaks = allocated, name = NULL,
                      labels = paste0(.txt("allocated"), "\n", .label_pct()(allocated)))
  }
  value_scale <- (if (vertical) ggplot2::scale_y_continuous else ggplot2::scale_x_continuous)(
    labels = .label_pct(), expand = ggplot2::expansion(mult = c(0, 0.05)), sec.axis = sec
  )
  value_title <- .txt("representation")

  p <- ggplot2::ggplot(d, mapping) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width = 0.7, colour = "white", linewidth = 0.3
    ) +
    ggplot2::scale_fill_manual(values = pal, labels = .wrap(names(pal), 20), name = NULL, drop = FALSE) +
    # Feature names are long; horizontal bars have room for wide labels
    (if (vertical) .category_scale(vertical) else ggplot2::scale_y_discrete(labels = function(x) .wrap(x, label_width))) +
    value_scale +
    ggplot2::labs(title = title) +
    (if (vertical) ggplot2::labs(x = NULL, y = value_title) else ggplot2::labs(x = value_title, y = NULL)) +
    .results_theme(base_size) +
    .category_axis_theme(vertical, nlevels(d$feature), levels(d$feature)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = max(1L, ceiling(length(pal) / 5))))

  if (!is.null(allocated)) {
    p <- p + if (vertical) {
      ggplot2::geom_hline(yintercept = allocated, linetype = "dashed", colour = "grey20", linewidth = 0.4)
    } else {
      ggplot2::geom_vline(xintercept = allocated, linetype = "dashed", colour = "grey20", linewidth = 0.4)
    }
  }
  if (has_themes) {
    strip_labels <- ggplot2::as_labeller(function(x) .wrap(x, 16))
    p <- p + if (vertical) {
      ggplot2::facet_grid(cols = ggplot2::vars(.data$theme), scales = "free_x", space = "free_x",
                          labeller = strip_labels)
    } else {
      ggplot2::facet_grid(rows = ggplot2::vars(.data$theme), scales = "free_y", space = "free_y",
                          switch = "y", labeller = strip_labels)
    }
    p <- p + ggplot2::theme(
      strip.placement = "outside",
      strip.text = ggplot2::element_text(face = "bold", colour = "grey20"),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "bold", colour = "grey20"),
      panel.spacing = ggplot2::unit(0.6, "lines")
    )
  }
  p
}

# Representation figures in long form: feature, theme, action, pct
.representation_long <- function(representation, actions = NULL, measure = "effective") {
  assertthat::assert_that(
    is.data.frame(representation), nrow(representation) > 0,
    msg = "'representation' must be a data frame with at least one row."
  )
  assertthat::assert_that(
    is.null(actions) || (is.character(actions) && length(actions) > 0 && !anyDuplicated(actions)),
    msg = "'actions' must be a character vector of unique action names."
  )
  if (all(c("layer", "action", "is_action", "pct_of_layer") %in% names(representation))) {
    return(.overlap_representation(representation, actions))
  }
  r <- as.data.frame(representation, stringsAsFactors = FALSE)
  r <- .select_measure(r, measure)
  # Lock-in columns of the national app are written in brackets: "(12.3)"
  for (nm in names(r)) {
    v <- r[[nm]]
    if (is.character(v) && any(!is.na(v)) && all(grepl("^\\s*\\(?\\s*-?[0-9.]+\\s*\\)?\\s*$", v[!is.na(v)]))) {
      r[[nm]] <- as.numeric(gsub("[()[:space:]]", "", v))
    }
  }
  find <- function(candidates) {
    hit <- names(r)[.normalise_label(names(r)) %in% .normalise_label(candidates)]
    if (length(hit) > 0) hit[1] else NULL
  }
  # Headers as the ELSA tool and app write them, in each supported language
  feature_col <- find(c("feature", "data", "datos", "donnees", "dados", "\u0434\u0430\u043d\u043d\u044b\u0435"))
  assertthat::assert_that(
    !is.null(feature_col),
    msg = "'representation' needs a feature column ('feature' or 'data', in any supported language)."
  )
  theme_col <- find(c("theme", "tema", "\u0442\u0435\u043c\u0430"))
  overall_col <- find(c("overall", "agregar", "agregat", "geral"))
  action_col <- find(c("action", "summary"))
  pct_col <- find("pct")
  held_col <- find("relative_held")

  if (!is.null(action_col) && (!is.null(pct_col) || !is.null(held_col))) {
    value <- if (!is.null(pct_col)) r[[pct_col]] else r[[held_col]] * 100
    d <- data.frame(
      feature = as.character(r[[feature_col]]),
      theme = if (is.null(theme_col)) NA_character_ else as.character(r[[theme_col]]),
      action = as.character(r[[action_col]]),
      pct = as.numeric(value),
      stringsAsFactors = FALSE
    )
    d <- d[tolower(d$action) != "overall", ]
    lock <- .is_existing_name(d$action)
    existing <- if (any(lock)) stats::setNames(d$pct[lock], d$feature[lock]) else NULL
    d <- d[!lock, ]
    if (is.null(actions)) actions <- unique(d$action)
  } else {
    skip <- c(feature_col, theme_col, overall_col)
    candidates <- setdiff(names(r)[vapply(r, is.numeric, logical(1))], skip)
    lock_col <- candidates[.is_existing_name(candidates)]
    existing <- if (length(lock_col) > 0) {
      stats::setNames(as.numeric(r[[lock_col[1]]]), as.character(r[[feature_col]]))
    }
    candidates <- setdiff(candidates, lock_col)
    if (is.null(actions)) actions <- candidates
    missing <- setdiff(actions, names(r))
    assertthat::assert_that(
      length(missing) == 0,
      msg = paste0("Action column(s) not in 'representation': ", toString(missing), ".")
    )
    assertthat::assert_that(
      length(actions) > 0, all(vapply(r[actions], is.numeric, logical(1))),
      msg = paste0(
        "'representation' has no numeric action columns. Pass a wide table ",
        "(one column per action) or a long one with 'action' and 'pct' columns."
      )
    )
    d <- do.call(rbind, lapply(actions, function(a) {
      data.frame(
        feature = as.character(r[[feature_col]]),
        theme = if (is.null(theme_col)) NA_character_ else as.character(r[[theme_col]]),
        action = a, pct = as.numeric(r[[a]]),
        stringsAsFactors = FALSE
      )
    }))
  }
  missing <- setdiff(actions, d$action)
  assertthat::assert_that(
    length(missing) == 0,
    msg = paste0("Action(s) not in 'representation': ", toString(missing), ".")
  )
  d <- d[d$action %in% actions, ]
  d$pct[is.na(d$pct)] <- 0
  if (any(d$pct < 0)) {
    warning("Negative representation values were drawn as given.", call. = FALSE)
  }
  # One theme per feature
  if (!all(is.na(d$theme))) {
    d$theme <- unname(stats::setNames(d$theme, d$feature)[d$feature])
  }
  if (!is.null(existing)) existing[is.na(existing)] <- 0
  attr(d, "existing") <- existing
  d
}

# Names of existing-protection columns: the national ELSA app's lock-in
# column ("Current Protection", in each language), its slug, and elsar's
# "Existing PAs" label in each language
.is_existing_name <- function(x) {
  known <- c(
    "Current Protection", "Protecci\u00f3n actual", "Protection actuelle",
    "\u0422\u0435\u043a\u0443\u0449\u0430\u044f \u043e\u0445\u0440\u0430\u043d\u0430",
    "Prote\u00e7\u00e3o de corrente", "Prote\u00e7\u00e3o existente", "Protection existante",
    "Existing protection", "current_protect", "Locked-in Protect", "Existing PAs",
    vapply(.elsar_text, function(t) t[["existing_pas"]], character(1))
  )
  .normalise_label(x) %in% .normalise_label(known)
}

# Existing protected-area coverage (%) by feature, from any accepted input
.existing_pct <- function(existing) {
  if (is.numeric(existing)) {
    assertthat::assert_that(!is.null(names(existing)),
                            msg = "A numeric 'existing' must be named by feature.")
    return(existing)
  }
  assertthat::assert_that(
    is.data.frame(existing),
    msg = "'existing' must be NULL, FALSE, a named numeric vector or a data frame."
  )
  if (all(c("layer", "amount", "amount_in_pa") %in% names(existing))) {
    assertthat::assert_that(
      any(!is.na(existing$amount_in_pa)),
      msg = "The overlap summary has no protected-area split; pass 'pas' to elsar_prepare_actions()."
    )
    regions <- unique(as.character(existing$region))
    if (length(regions) > 1) {
      message("Using existing protected-area coverage for '", regions[1], "'.")
    }
    o <- existing[as.character(existing$region) == regions[1], ]
    by_layer <- split(o, as.character(o$layer))
    return(vapply(by_layer, function(l) {
      .safe_pct(sum(l$amount_in_pa, na.rm = TRUE), sum(l$amount, na.rm = TRUE))
    }, numeric(1)))
  }
  assertthat::assert_that(
    all(c("feature", "pct") %in% names(existing)),
    msg = "A data frame 'existing' needs 'feature' and 'pct' columns, or must come from elsar_summarise_overlap()."
  )
  stats::setNames(as.numeric(existing$pct), as.character(existing$feature))
}

# Take existing protection out of the Protect segment, as its own segment
# drawn first
.split_existing <- function(d, pal, existing_pct, pa_colour, pa_label) {
  protect <- names(pal)[.label_key(names(pal)) == "protect"]
  if (length(protect) == 0) {
    warning("No Protect action to split existing protected areas from; drawn without them.", call. = FALSE)
    return(list(d = d, pal = pal))
  }
  protect <- protect[1]
  rows <- which(d$action == protect)
  matched <- d$feature[rows] %in% names(existing_pct)
  if (!any(matched)) {
    warning("No feature names match those of 'existing'; drawn without existing protected areas.",
            call. = FALSE)
    return(list(d = d, pal = pal))
  }
  if (!all(matched)) {
    warning("No existing protected-area coverage for: ", toString(d$feature[rows][!matched]),
            ". Treated as 0.", call. = FALSE)
  }
  e <- unname(existing_pct[d$feature[rows]])
  e[is.na(e)] <- 0
  over <- e > d$pct[rows] + 0.5
  if (any(over)) {
    warning(
      "Existing protected-area coverage is larger than ", protect, " for ", sum(over),
      " feature(s) (", toString(utils::head(d$feature[rows][over], 3)), "); capped at the ",
      protect, " value. Check that both use the same measure.",
      call. = FALSE
    )
  }
  e <- pmin(e, d$pct[rows])
  d$pct[rows] <- d$pct[rows] - e
  pa <- d[rows, ]
  pa$pct <- e
  pa$action <- pa_label
  # What is left of Protect is the protection ELSA adds
  new_label <- .txt("new_protection")
  d$action <- as.character(d$action)
  d$action[d$action == protect] <- new_label
  d <- rbind(pa, d)
  names(pal)[names(pal) == protect] <- new_label
  pal <- c(stats::setNames(pa_colour, pa_label), pal)
  d$action <- factor(d$action, levels = names(pal))
  list(d = d, pal = pal, protect = protect)
}

# Legend labels with each action's budget: "Protect (30.0%)". The new-protection
# segment takes Protect's name and budget, as in the ELSA tool
.budget_labels <- function(labels, budgets, seg, protect_label, pa_label) {
  assertthat::assert_that(
    is.numeric(budgets), !is.null(names(budgets)),
    msg = "'budgets' must be a named numeric vector (action = percent)."
  )
  # Budget names as table actions become their drawn labels
  bnames <- ifelse(names(budgets) %in% seg$action, seg$label[match(names(budgets), seg$action)], names(budgets))
  marks <- .number_marks()
  out <- stats::setNames(labels, labels)
  for (l in labels) {
    if (l == pa_label) next
    base <- if (!is.null(protect_label) && l == .txt("new_protection")) protect_label else l
    j <- match(.label_key(base), .label_key(bnames))
    if (is.na(j) || is.na(budgets[[j]])) next
    out[[l]] <- paste0(base, " (", formatC(budgets[[j]], format = "f", digits = 1,
                                           decimal.mark = marks[["decimal"]]), "%)")
  }
  out
}

# Budgets by action slug from a run or deployment config
.config_budgets <- function(config) {
  b <- vapply(config$actions, function(a) as.numeric(a$budget %||% a$defaultBudget %||% NA), numeric(1))
  names(b) <- vapply(config$actions, function(a) as.character(a$slug %||% a$name), character(1))
  b <- b[!is.na(b)]
  if (length(b) == 0) NULL else b
}

# Keep one measure of an export with paired "<column> (effective)" and
# "<column> (spatial)" columns, and drop the suffix. The suffix is English in
# the ELSA tool's export and in the deployment language in the national app's.
.select_measure <- function(r, measure) {
  suffixes <- list(
    effective = c("effective", "efectiva", "efetiva", "\u044d\u0444\u0444\u0435\u043a\u0442\u0438\u0432\u043d\u0430\u044f"),
    spatial = c("spatial", "espacial", "spatiale",
                "\u043f\u0440\u043e\u0441\u0442\u0440\u0430\u043d\u0441\u0442\u0432\u0435\u043d\u043d\u0430\u044f")
  )
  pattern <- "^(.*\\S)\\s*\\(([^()]+)\\)$"
  has_suffix <- grepl(pattern, names(r))
  suffix <- tolower(sub(pattern, "\\2", names(r)))
  paired <- has_suffix & suffix %in% unlist(suffixes)
  if (!any(paired)) return(r)
  keep <- !paired | suffix %in% suffixes[[measure]]
  assertthat::assert_that(
    any(paired & keep),
    msg = paste0("'representation' has no ", measure, " columns.")
  )
  r <- r[, keep, drop = FALSE]
  names(r)[paired[keep]] <- sub(pattern, "\\1", names(r)[paired[keep]])
  assertthat::assert_that(
    !anyDuplicated(names(r)),
    msg = "Column names clash once the measure suffix is dropped."
  )
  r
}

# The interactive version: one boxed panel per theme under a bold heading,
# stacked bar traces by action and the legend along the top. Panels are laid
# out in pixels (one category axis per panel, a shared value axis), so
# headings and bars keep their size however many features there are.
.representation_plotly <- function(d, pal, vertical, allocated, title, has_themes,
                                   base_size, font_family, label_width) {
  ink <- elsar_inks()
  marks <- .number_marks()
  fmt <- function(v) {
    paste0(formatC(v, format = "f", digits = 1, big.mark = marks[["big"]],
                   decimal.mark = marks[["decimal"]]), "%")
  }
  font <- list(family = font_family, size = base_size * 1.3, color = ink[["ink_secondary"]])
  # Feature names smaller than the legend, which carries the actions
  label_font <- list(family = font_family, size = base_size * 1.4, color = ink[["ink_secondary"]])

  # Every feature in every trace
  themes <- unique(d[, c("feature", "theme")])
  grid <- expand.grid(feature = levels(d$feature), action = levels(d$action), stringsAsFactors = FALSE)
  d <- merge(grid, d[, c("feature", "action", "pct")], all.x = TRUE)
  d$pct[is.na(d$pct)] <- 0
  d$theme <- as.character(themes$theme[match(d$feature, as.character(themes$feature))])
  d$feature <- factor(d$feature, levels = levels(themes$feature))
  d$action <- factor(d$action, levels = names(pal))
  d <- d[order(d$action, d$feature), ]
  total <- tapply(d$pct, d$feature, sum)
  d$hover <- paste0(
    "<b>", d$feature, "</b>", if (has_themes) paste0(" (", d$theme, ")") else "",
    "<br>", d$action, ": ", fmt(d$pct),
    "<br>", .txt("total"), ": ", fmt(total[as.character(d$feature)])
  )
  d$label <- gsub("\n", "<br>", .wrap(as.character(d$feature), label_width))
  label_lines <- max(lengths(regmatches(d$label, gregexpr("<br>", d$label)))) + 1

  # Panels in reading order (top to bottom, or left to right)
  panel_themes <- if (has_themes) {
    th <- unique(as.character(themes$theme[order(themes$feature)]))
    if (vertical) th else rev(th)
  } else {
    NA_character_
  }
  k <- length(panel_themes)
  in_panel <- lapply(panel_themes, function(t) if (is.na(t)) rep(TRUE, nrow(d)) else d$theme == t)
  n_per <- vapply(in_panel, function(w) length(unique(d$feature[w])), numeric(1))

  # Pixel layout
  title_px <- if (is.null(title)) 0 else base_size * 2 + 18
  # the legend, with room above it for plotly's toolbar
  legend_px <- base_size * 1.6 + 50
  heading_px <- if (has_themes) base_size * 1.8 + 14 else 6
  # Rows tall enough for the longest wrapped name
  bar_px <- max(24, label_lines * base_size * 1.4 * 1.15 + 7)
  bottom_px <- 70
  top_px <- title_px + legend_px + (if (vertical) heading_px else 0)
  plot_px <- if (vertical) 420 else sum(n_per) * bar_px + k * heading_px
  height <- plot_px + top_px + bottom_px

  # Panel domains (fractions of the plot area)
  domains <- vector("list", k)
  if (vertical) {
    gap <- if (k > 1) 0.025 else 0
    widths <- (1 - gap * (k - 1)) * n_per / sum(n_per)
    start <- 0
    for (i in seq_len(k)) {
      domains[[i]] <- c(start, start + widths[i])
      start <- start + widths[i] + gap
    }
  } else {
    top <- 1
    for (i in seq_len(k)) {
      top <- top - heading_px / plot_px
      bottom <- top - n_per[i] * bar_px / plot_px
      domains[[i]] <- c(max(0, bottom), top)
      top <- bottom
    }
  }
  axis_id <- function(i) if (i == 1) "" else as.character(i)
  cat_letter <- if (vertical) "x" else "y"
  value_letter <- if (vertical) "y" else "x"

  line_style <- list(color = ink[["ink"]], width = 1.5, dash = "dash")
  p <- plotly::plot_ly(height = height)
  for (i in seq_len(k)) {
    s <- d[in_panel[[i]], ]
    cat_ref <- paste0(cat_letter, axis_id(i))
    for (a in names(pal)) {
      sa <- s[s$action == a, ]
      args <- list(
        p,
        x = if (vertical) sa$label else sa$pct,
        y = if (vertical) sa$pct else sa$label,
        orientation = if (vertical) "v" else "h",
        name = a, showlegend = FALSE,
        hovertext = sa$hover, hoverinfo = "text", textposition = "none",
        marker = list(color = pal[[a]], line = list(color = "white", width = 0.6))
      )
      args[[paste0(cat_letter, "axis")]] <- cat_ref
      p <- do.call(plotly::add_bars, args)
    }
  }

  # Axes: one category axis per panel, one shared value axis
  value_max <- max(c(total, allocated), na.rm = TRUE) * 1.04
  layout_args <- list()
  for (i in seq_len(k)) {
    labels <- unique(d$label[in_panel[[i]]][order(d$feature[in_panel[[i]]])])
    layout_args[[paste0(cat_letter, "axis", axis_id(i))]] <- list(
      domain = domains[[i]], anchor = value_letter,
      categoryorder = "array", categoryarray = labels,
      automargin = TRUE, tickfont = label_font, showgrid = FALSE, zeroline = FALSE,
      # outside ticks in the background colour keep labels off the box
      ticks = "outside", ticklen = 6, tickcolor = ink[["surface"]],
      title = list(text = ""), fixedrange = TRUE
    )
  }
  layout_args[[paste0(value_letter, "axis")]] <- list(
    anchor = paste0(cat_letter, axis_id(if (vertical) 1 else k)),
    range = c(0, value_max), ticksuffix = "%", zeroline = FALSE,
    gridcolor = ink[["rule"]], tickfont = font,
    title = list(text = .txt("representation"), font = font)
  )

  # Boxes, headings and the allocated line, per panel
  shapes <- list()
  annotations <- list()
  for (i in seq_len(k)) {
    cat_dom <- paste(paste0(cat_letter, axis_id(i)), "domain")
    box <- list(type = "rect", line = list(color = ink[["ink_secondary"]], width = 1),
                fillcolor = "rgba(0,0,0,0)", layer = "above", x0 = 0, x1 = 1, y0 = 0, y1 = 1)
    if (vertical) {
      box$xref <- cat_dom
      box$yref <- "paper"
    } else {
      box$xref <- "paper"
      box$yref <- cat_dom
    }
    shapes[[length(shapes) + 1]] <- box
    if (!is.null(allocated)) {
      shapes[[length(shapes) + 1]] <- if (vertical) {
        list(type = "line", xref = cat_dom, x0 = 0, x1 = 1, yref = "y", y0 = allocated, y1 = allocated,
             line = line_style, layer = "above")
      } else {
        list(type = "line", yref = cat_dom, y0 = 0, y1 = 1, xref = "x", x0 = allocated, x1 = allocated,
             line = line_style, layer = "above")
      }
    }
    if (has_themes) {
      heading <- list(
        text = paste0("<b>", panel_themes[i], "</b>"), showarrow = FALSE,
        xanchor = "left", yanchor = "bottom", yshift = 3,
        font = list(family = font_family, size = base_size * 1.8, color = ink[["ink"]])
      )
      if (vertical) {
        heading <- c(heading, list(xref = cat_dom, x = 0, yref = "paper", y = 1))
      } else {
        heading <- c(heading, list(xref = "paper", x = 0, yref = cat_dom, y = 1))
      }
      annotations[[length(annotations) + 1]] <- heading
    }
  }

  # Legend: entries spread across the width of the plot area, the first
  # aligned left and the last right. Drawn as annotations, as plotly's own
  # legend cannot be justified; they are part of the layout, so PNG exports
  # keep them.
  entries <- paste0("<span style='color:", unname(pal), ";font-size:1.5em'>\u25A0</span> ", names(pal))
  if (!is.null(allocated)) {
    entries <- c(entries, paste0("<span style='color:", ink[["ink"]], "'>\u2013 \u2013</span>  ",
                                 .txt("allocated"), " (", fmt(allocated), ")"))
  }
  # Starting positions from estimated text widths on a 1000 px plot area; in
  # the browser, .justify_legend_js measures the entries and spreads them
  # exactly across the plot area's width
  legend_size <- base_size * 1.6
  plain <- gsub("<[^>]+>", "", entries)
  est <- nchar(plain) * legend_size * 0.52
  gap <- max(12, (1000 - sum(est)) / max(1, length(entries) - 1))
  left <- c(0, cumsum(est + gap))[seq_along(entries)] / 1000
  for (j in seq_along(entries)) {
    annotations[[length(annotations) + 1]] <- list(
      name = paste0("elsar-legend-", j),
      text = entries[j], showarrow = FALSE, xref = "paper", yref = "paper", x = min(left[j], 1), y = 1,
      xanchor = "left", yanchor = "bottom", yshift = (if (vertical) heading_px else 0) + 10,
      font = list(family = font_family, size = legend_size, color = ink[["ink"]])
    )
  }

  p <- do.call(plotly::layout, c(list(
    p,
    showlegend = FALSE,
    barmode = "stack", bargap = 0.3, font = font,
    title = if (is.null(title)) NULL else list(
      text = paste0("<b>", title, "</b>"), x = 0.01, xref = "container", xanchor = "left",
      y = 1, yref = "container", yanchor = "top", pad = list(t = 10),
      font = list(family = font_family, size = base_size * 2, color = ink[["ink"]])
    ),
    separators = paste0(marks[["decimal"]], marks[["big"]]),
    shapes = shapes, annotations = annotations,
    margin = list(t = top_px, b = bottom_px, r = 20, l = 10),
    hoverlabel = list(font = list(family = font_family, size = base_size * 1.3)),
    paper_bgcolor = ink[["surface"]], plot_bgcolor = ink[["surface"]]
  ), layout_args))
  p <- plotly::config(
    p, displaylogo = FALSE,
    toImageButtonOptions = list(format = "png", filename = "elsa_representation", scale = 2)
  )
  htmlwidgets::onRender(p, .justify_legend_js)
}

# Spread the legend annotations ("elsar-legend-*") across the plot area:
# measure each entry, fill rows (wrapping when the width runs out), justify
# each full row, and set positions with Plotly.relayout so PNG exports keep
# them. Runs again after every redraw (e.g. a resize); it only relayouts when
# something moves, so it cannot loop.
.justify_legend_js <- "
function(el) {
  var busy = false;
  var base = null;
  function justify() {
    if (busy || !el._fullLayout) return;
    var anns = el.layout.annotations || [];
    var idx = [];
    anns.forEach(function(a, i) {
      if (a.name && a.name.indexOf('elsar-legend-') === 0) idx.push(i);
    });
    if (idx.length === 0) return;
    if (!base) {
      var size = (anns[idx[0]].font && anns[idx[0]].font.size) || 16;
      base = { yshift: anns[idx[0]].yshift || 0, top: el.layout.margin.t, line: size * 1.9 };
    }
    var widths = idx.map(function(i) {
      var node = el.querySelector('.annotation[data-index=\"' + i + '\"]');
      return node ? node.getBoundingClientRect().width : 0;
    });
    var w = el._fullLayout._size.w;
    var minGap = 24;
    // Fill rows left to right
    var rows = [[]];
    var used = 0;
    widths.forEach(function(wd, k) {
      var row = rows[rows.length - 1];
      var need = used + (row.length ? minGap : 0) + wd;
      if (row.length && need > w) {
        rows.push([k]);
        used = wd;
      } else {
        row.push(k);
        used = need;
      }
    });
    var update = {};
    var changed = false;
    function set(key, now, value) {
      if (Math.abs((now || 0) - value) > 0.002) {
        update[key] = value;
        changed = true;
      }
    }
    rows.forEach(function(row, r) {
      var total = row.reduce(function(a, k) { return a + widths[k]; }, 0);
      var last = r === rows.length - 1 && rows.length > 1;
      var gap = row.length > 1 && !last ? (w - total) / (row.length - 1) : minGap;
      var x = 0;
      var yshift = base.yshift + (rows.length - 1 - r) * base.line;
      row.forEach(function(k) {
        var i = idx[k];
        set('annotations[' + i + '].x', anns[i].x, x / w);
        set('annotations[' + i + '].yshift', anns[i].yshift, yshift);
        x += widths[k] + gap;
      });
    });
    set('margin.t', el.layout.margin.t, base.top + (rows.length - 1) * base.line);
    if (changed) {
      busy = true;
      Plotly.relayout(el, update).then(function() { busy = false; });
    }
  }
  justify();
  el.on('plotly_afterplot', justify);
}
"


#' Existing protection of each feature in an ELSA run
#'
#' Measures how much of each feature of an ELSA tool run lies inside the
#' protected areas the run locked into Protect, from the run's configuration
#' and input layers. Pass the result to `existing` in
#' [elsar_plot_representation()] (or pass the configuration to its `config`
#' argument, which calls this function).
#'
#' Features and protected areas are read as the run used them: each layer of
#' the configuration's `layers`, named by its display name, and the lock-in
#' layer of the Protect action. Both kinds of configuration work: the run's
#' own (`lockin` and `budget` per action) and the platform's deployment
#' configuration (`lockinOptions` with a `defaultLockin`, and
#' `defaultBudget`). Spatial coverage is the feature's amount in
#' protected areas (weighted by their cover of each cell) as a percentage of
#' its total amount; effective coverage multiplies it by Protect's impact on
#' the feature, as ELSA's effective representation does.
#'
#' @param config The run's configuration: path to its `config.json`, or the
#'   parsed list.
#' @param dir Folder holding the input layers named in `config`, searched
#'   recursively. Defaults to the folder of `config`.
#' @param measure `"effective"` (default) or `"spatial"`.
#' @param action Slug of the action whose lock-in layer holds the protected
#'   areas. Default `"protect"`.
#' @param lockin For a platform (deployment) configuration, which offers
#'   several lock-in layers: the slug or file name of the one the run used.
#'   Defaults to the configuration's `defaultLockin`. Ignored for a run
#'   configuration, which names its lock-in layer.
#'
#' @return A named numeric vector: the percentage of each feature in existing
#'   protected areas, named by the features' display names.
#' @seealso [elsar_plot_representation()]
#' @export
#'
#' @examples
#' \dontrun{
#' pa <- elsar_existing_protection("inputs/config.json")
#' elsar_plot_representation(rep, existing = pa)
#' }
elsar_existing_protection <- function(config, dir = NULL,
                                      measure = c("effective", "spatial"),
                                      action = "protect",
                                      lockin = NULL) {
  measure <- match.arg(measure)
  cfg <- .read_elsa_config(config, dir)
  config <- cfg$config
  dir <- cfg$dir
  slugs <- vapply(config$actions, function(a) as.character(a$slug %||% ""), character(1))
  i <- match(action, slugs)
  assertthat::assert_that(
    !is.na(i),
    msg = paste0("No action '", action, "' in the configuration (actions: ", toString(slugs), ").")
  )
  lock_file <- .config_lockin_file(config$actions[[i]], lockin)
  assertthat::assert_that(
    !is.null(lock_file) && nzchar(lock_file),
    msg = paste0("The '", action, "' action has no lock-in layer, so the run had no existing protected areas.")
  )
  layers <- config$layers
  assertthat::assert_that(length(layers) > 0, msg = "The configuration has no 'layers'.")
  files <- vapply(layers, function(l) as.character(l$fileName %||% NA), character(1))
  paths <- .find_inputs(c(lock_file, files), dir)

  lock <- terra::rast(paths[[1]])[[1]]
  out <- vapply(seq_along(layers), function(j) {
    f <- terra::rast(paths[[j + 1]])[[1]]
    l <- lock
    if (!terra::compareGeom(l, f, stopOnError = FALSE)) {
      l <- terra::project(l, f, method = "average")
    }
    l <- terra::ifel(is.na(l), 0, l)
    held <- terra::global(f * l, "sum", na.rm = TRUE)[[1]]
    total <- terra::global(f, "sum", na.rm = TRUE)[[1]]
    pct <- .safe_pct(held, total)
    if (measure == "effective") {
      impact <- layers[[j]]$actionImpacts[[action]]
      if (!is.null(impact)) pct <- pct * as.numeric(impact)
    }
    pct
  }, numeric(1))
  stats::setNames(out, vapply(layers, function(l) as.character(l$name), character(1)))
}

# A parsed ELSA config and the folder of its inputs
.read_elsa_config <- function(config, dir = NULL) {
  if (is.character(config)) {
    assertthat::assert_that(
      length(config) == 1, file.exists(config),
      msg = paste0("Config file not found: ", config[1])
    )
    dir <- dir %||% dirname(config)
    config <- jsonlite::read_json(config)
  }
  assertthat::assert_that(
    is.list(config), length(config$actions) > 0,
    msg = "'config' must be an ELSA tool config with an 'actions' list."
  )
  assertthat::assert_that(
    !is.null(dir) && dir.exists(dir),
    msg = "Give 'dir', the folder holding the run's input layers."
  )
  list(config = config, dir = dir)
}

# The lock-in file of an action: a run config names it; a deployment config
# offers options, of which `lockin` (slug or file name) or the default is used
.config_lockin_file <- function(a, lockin = NULL) {
  if (!is.null(a$lockin$fileName)) return(a$lockin$fileName)
  options <- a$lockinOptions
  if (length(options) == 0) return(NULL)
  pick <- lockin %||% a$defaultLockin
  if (is.null(pick)) return(NULL)
  slugs <- vapply(options, function(o) as.character(o$slug %||% ""), character(1))
  files <- vapply(options, function(o) as.character(o$fileName %||% ""), character(1))
  j <- match(pick, slugs)
  if (is.na(j)) j <- match(pick, files)
  assertthat::assert_that(
    !is.na(j),
    msg = paste0("Lock-in '", pick, "' is not one of the options: ", toString(slugs), ".")
  )
  if (nzchar(files[j])) files[j] else NULL
}

# Share of the planning area allocated: the sum of the action budgets (%)
.config_allocated <- function(config) {
  budgets <- vapply(config$actions, function(a) as.numeric(a$budget %||% a$defaultBudget %||% NA), numeric(1))
  if (all(is.na(budgets))) NULL else sum(budgets, na.rm = TRUE)
}

# Find input files by name anywhere under `dir`
.find_inputs <- function(files, dir) {
  all_files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  found <- all_files[match(files, basename(all_files))]
  missing <- files[is.na(found)]
  assertthat::assert_that(
    length(missing) == 0,
    msg = paste0(
      length(missing), " of ", length(files), " input layer(s) not found under '", dir, "': ",
      toString(utils::head(missing, 3)), if (length(missing) > 3) ", ..." else "", ". ",
      "Point to the folder holding the run's layers (searched recursively) with ",
      "`layers_dir` in elsar_plot_representation() or `dir` in elsar_existing_protection()."
    )
  )
  found
}

# Spatial representation from an elsar_summarise_overlap() table: each
# layer's share under each action, and its share in existing protected areas
# within Protect
.overlap_representation <- function(overlap, actions = NULL) {
  regions <- unique(as.character(overlap$region))
  if (length(regions) > 1) {
    message("Plotting representation for '", regions[1], "'; filter the overlap summary for another region.")
  }
  o <- overlap[as.character(overlap$region) == regions[1] & overlap$is_action, ]
  d <- data.frame(
    feature = as.character(o$layer), theme = NA_character_,
    action = as.character(o$action), pct = as.numeric(o$pct_of_layer),
    stringsAsFactors = FALSE
  )
  if (!is.null(actions)) {
    missing <- setdiff(actions, d$action)
    assertthat::assert_that(length(missing) == 0,
                            msg = paste0("Action(s) not in 'representation': ", toString(missing), "."))
    d <- d[d$action %in% actions, ]
  }
  d$pct[is.na(d$pct)] <- 0
  # Existing protected areas inside Protect, as a share of the whole layer
  existing <- NULL
  protect <- o[.label_key(o$action) == "protect", ]
  if (nrow(protect) > 0 && any(!is.na(protect$amount_in_pa))) {
    all_rows <- overlap[as.character(overlap$region) == regions[1], ]
    totals <- tapply(all_rows$amount, as.character(all_rows$layer), sum, na.rm = TRUE)
    existing <- stats::setNames(
      .safe_pct(protect$amount_in_pa, totals[as.character(protect$layer)]),
      as.character(protect$layer)
    )
  }
  attr(d, "existing") <- existing
  d
}

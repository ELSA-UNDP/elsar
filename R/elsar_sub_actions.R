# Sub-actions: split ELSA actions into specific interventions -------------------
#
# An ELSA action (e.g. Restore) covers many interventions: riparian, wetland or
# forest restoration. Sub-action rules split each action with conditions over
# context layers. The result is a prepared action map whose scheme has one value
# per sub-action, with the parent action as its `group`, so every summary, plot
# and export in elsar works on it unchanged.

#' Define a sub-action
#'
#' One rule of a sub-action rule set: a label and the condition a cell of the
#' parent action must meet to be assigned to it. Use inside
#' [elsar_sub_actions()].
#'
#' @param label Character. Name of the sub-action, e.g. `"Riparian restoration"`.
#' @param condition An R expression over the names of the `layers` given to
#'   [elsar_assign_sub_actions()], e.g. `riparian > 0.5 & !coastal`. It is
#'   evaluated with terra arithmetic, so it can use comparison (`>`, `>=`,
#'   `==`, ...), logical (`&`, `|`, `!`) and arithmetic operators, `is.na()`,
#'   and values from the calling environment (e.g. a threshold stored in a
#'   variable). A numeric layer on its own counts as `TRUE` where it is not 0.
#'   Cells where the condition is `NA` do not match.
#' @param colour Optional colour (name or hex code). By default, shades of the
#'   parent action's colour are used.
#'
#' @return An `elsar_sub_action` object.
#' @seealso [elsar_sub_actions()]
#' @export
#'
#' @examples
#' sub_action("Riparian restoration", riparian > 0.5)
sub_action <- function(label, condition, colour = NULL) {
  assertthat::assert_that(
    assertthat::is.string(label), nzchar(label),
    msg = "'label' must be a single non-empty string."
  )
  condition <- rlang::enquo(condition)
  assertthat::assert_that(
    !rlang::quo_is_missing(condition),
    msg = paste0("Sub-action '", label, "' needs a condition.")
  )
  if (!is.null(colour)) .check_colours(colour, 1)
  structure(
    list(label = label, condition = condition, colour = colour %||% NA_character_),
    class = "elsar_sub_action"
  )
}

#' Define sub-actions of an ELSA action
#'
#' Builds a set of rules that split an ELSA action into specific interventions,
#' such as Restore into riparian, wetland and forest restoration. Pass the rules
#' to [elsar_assign_sub_actions()] to map them.
#'
#' @section How cells are assigned:
#' Rules are tried in the order given, and each cell of the parent action goes
#' to the **first** sub-action whose condition it meets. Put the most specific
#' interventions first. Cells that meet no condition go to the `other`
#' sub-action. So every cell of the parent gets exactly one sub-action, and the
#' sub-action areas always add up to the parent's area. Use
#' [elsar_sub_action_diagnostics()] to see how much area each rule matched and
#' how much of it was taken by an earlier rule.
#'
#' @param parent Character. Label of the action to split, as in the action
#'   scheme of the prepared map (e.g. `"Restore"`).
#' @param ... Sub-actions made with [sub_action()], in priority order.
#' @param other Character. Label of the sub-action that gets the parent's
#'   remaining cells. Defaults to the parent label followed by "(other)" in
#'   the current language (see [elsar_labels()]).
#' @param other_colour Optional colour of the `other` sub-action.
#' @param rules A rule set from `elsar_sub_actions()`, to add to.
#'
#' @return An `elsar_sub_actions` tibble with one row per sub-action and
#'   columns `parent`, `label`, `priority`, `condition` (text of the
#'   condition), `colour` and `is_other`. The conditions themselves are kept
#'   in the `quosure` column.
#'
#' @seealso [elsar_assign_sub_actions()], [elsar_sub_action_diagnostics()]
#' @export
#'
#' @examples
#' rules <- elsar_sub_actions(
#'   "Restore",
#'   sub_action("Wetland restoration", wetland > 0.5),
#'   sub_action("Riparian restoration", riparian > 0.5),
#'   other = "Other restoration"
#' ) |>
#'   add_sub_actions(
#'     "Manage",
#'     sub_action("Sustainable agriculture", cropland > 0.3)
#'   )
#' rules
elsar_sub_actions <- function(parent, ..., other = NULL, other_colour = NULL) {
  assertthat::assert_that(
    assertthat::is.string(parent), nzchar(parent),
    msg = "'parent' must be a single non-empty string: the label of an action."
  )
  subs <- list(...)
  assertthat::assert_that(
    length(subs) > 0,
    msg = paste0("Give at least one sub_action() for '", parent, "'.")
  )
  ok <- vapply(subs, inherits, logical(1), "elsar_sub_action")
  assertthat::assert_that(
    all(ok),
    msg = "Every argument in '...' must be made with sub_action()."
  )
  other <- other %||% paste0(parent, " (", .txt("other"), ")")
  assertthat::assert_that(
    assertthat::is.string(other), nzchar(other),
    msg = "'other' must be a single non-empty string."
  )
  if (!is.null(other_colour)) .check_colours(other_colour, 1)

  n <- length(subs)
  out <- tibble::tibble(
    parent = parent,
    label = c(vapply(subs, `[[`, character(1), "label"), other),
    priority = seq_len(n + 1),
    condition = c(
      vapply(subs, function(s) .expr_text(rlang::quo_get_expr(s$condition)), character(1)),
      NA_character_
    ),
    colour = c(vapply(subs, `[[`, character(1), "colour"), other_colour %||% NA_character_),
    is_other = c(rep(FALSE, n), TRUE),
    quosure = c(lapply(subs, `[[`, "condition"), list(NULL)),
    kind = "rule",
    params = list(NULL)
  )
  .check_sub_action_labels(out)
  class(out) <- c("elsar_sub_actions", class(out))
  out
}

#' @rdname elsar_sub_actions
#' @export
add_sub_actions <- function(rules, parent, ..., other = NULL, other_colour = NULL) {
  .check_sub_action_rules(rules)
  assertthat::assert_that(
    !parent %in% rules$parent,
    msg = paste0("The rules already split '", parent, "'. Give each parent once.")
  )
  new <- elsar_sub_actions(parent, ..., other = other, other_colour = other_colour)
  out <- rbind(tibble::as_tibble(rules), tibble::as_tibble(new))
  .check_sub_action_labels(out)
  class(out) <- c("elsar_sub_actions", class(tibble::tibble()))
  out
}

#' @export
print.elsar_sub_actions <- function(x, ...) {
  cat("<elsar sub-action rules: ", length(unique(x$parent)), " action(s), ",
      nrow(x), " sub-action(s)>\n", sep = "")
  for (p in unique(x$parent)) {
    r <- x[x$parent == p, ]
    cat(p, "\n", sep = "")
    for (i in seq_len(nrow(r))) {
      cond <- if (r$is_other[i]) "(remaining cells)" else r$condition[i]
      cat("  ", r$priority[i], ". ", r$label[i], ": ", cond, "\n", sep = "")
    }
  }
  invisible(x)
}

#' Assign ELSA actions to sub-actions
#'
#' Splits actions of a prepared ELSA action map into sub-actions, following
#' rules from [elsar_sub_actions()]. The result is itself a prepared action map:
#' each sub-action is a value of its scheme, with the parent action as its
#' `group`, so [elsar_summarise_actions()], [elsar_summarise_overlap()],
#' [elsar_plot_actions()], [elsar_plot_action_bars()] (which stacks a parent's
#' sub-actions in one bar), [elsar_export_results()] and
#' [elsar_results_report()] all work on it.
#'
#' Actions without rules (e.g. Protect, Do Nothing) are kept as they are. The
#' protected-area layer, if `x` has one, is kept too, so summaries still split
#' each sub-action by existing protected areas.
#'
#' @param x A prepared action map from [elsar_prepare_actions()].
#' @param rules Sub-action rules from [elsar_sub_actions()].
#' @param layers The context layers named in the conditions, as a `SpatRaster`
#'   (or path) with named layers, or a named list of `SpatRaster`, `sf` or
#'   `SpatVector` objects. Polygons become the share of each cell they cover
#'   (0-1). Rasters on another grid are resampled to the action grid (by
#'   average, or nearest neighbour for categorical rasters). `NULL` if the
#'   conditions use no layers.
#'
#' @param params Optional named list of single values (numbers, strings or
#'   `TRUE`/`FALSE`) that conditions can use by name, e.g.
#'   `list(riparian_min = 0.5)` for a rule `riparian > riparian_min`. This keeps
#'   thresholds out of the rules, so the same rules (or rules file) can be run
#'   with other values, as [elsar_sub_action_sensitivity()] does. Names must
#'   not clash with layer names.
#'
#' @return A prepared action map (`SpatRaster` with an `action` layer and, if
#'   `x` had one, a `pa_fraction` layer). Its categories add a `parent` column
#'   to the action scheme.
#'
#' @seealso [elsar_sub_actions()], [elsar_sub_action_diagnostics()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#'
#' # A made-up river corridor and cropland layer
#' riparian <- terra::init(x[["action"]], "x") < 30000
#' cropland <- terra::init(x[["action"]], "y") / 1e5
#' layers <- c(riparian = riparian, cropland = cropland)
#'
#' rules <- elsar_sub_actions(
#'   "Restore",
#'   sub_action("Riparian restoration", riparian)
#' ) |>
#'   add_sub_actions(
#'     "Manage",
#'     sub_action("Sustainable agriculture", cropland > 0.3)
#'   )
#'
#' sub <- elsar_assign_sub_actions(x, rules, layers)
#' elsar_summarise_actions(sub, units = "km2")
elsar_assign_sub_actions <- function(x, rules, layers = NULL, params = list()) {
  scheme <- .scheme_from_actions(x)
  .check_sub_action_rules(rules)
  .check_sub_action_parents(rules, scheme)
  layers <- .add_params(.condition_layers(layers, x), params)
  layers <- .add_protect_layers(layers, x, rules, scheme)

  action <- x[["action"]]
  levels(action) <- NULL
  new_scheme <- .sub_action_scheme(scheme, rules)

  # Every cell starts at its own action's new value (a parent's "other")
  start <- new_scheme[!new_scheme$is_sub | new_scheme$is_other, ]
  out <- terra::classify(action, cbind(start$old_value, start$value))

  # Rules from last to first, so the first matching rule wins
  sub_rules <- new_scheme[new_scheme$is_sub & !new_scheme$is_other, ]
  for (i in rev(seq_len(nrow(sub_rules)))) {
    r <- sub_rules[i, ]
    cond <- .eval_sub_action(r$quosure[[1]], r$label, layers, action)
    hit <- (action == r$old_value) & terra::ifel(is.na(cond), FALSE, cond)
    out <- terra::ifel(hit, r$value, out)
  }

  levels(out) <- as.data.frame(new_scheme[, c("value", "label", "colour", "group", "is_action", "parent")])
  terra::coltab(out) <- data.frame(value = new_scheme$value, col = new_scheme$colour)
  names(out) <- "action"

  if ("pa_fraction" %in% names(x)) out <- c(out, x[["pa_fraction"]])
  out
}

#' Check how sub-action rules split each action
#'
#' For each sub-action, reports how much of the parent action its condition
#' matches, how much it is assigned (the first matching rule wins), how much
#' of its match was taken by an earlier rule, and where its condition could not
#' be evaluated (`NA` layers). Use it to check the order of the rules and that
#' the `other` sub-action is not hiding a large share of the action.
#'
#' @inheritParams elsar_assign_sub_actions
#' @param x A prepared action map from [elsar_prepare_actions()]: the map
#'   *before* sub-actions are assigned.
#' @param units Character. Area units: `"ha"` (default), `"km2"` or `"m2"`.
#'
#' @return A tibble with one row per sub-action and columns `parent`,
#'   `sub_action`, `priority`, `condition`, `area_parent`, `area_matched`,
#'   `area_assigned`, `area_shadowed` (matched but assigned to an earlier
#'   rule), `area_na` (the condition is `NA`) and `pct_of_parent`
#'   (`area_assigned` as a percentage of `area_parent`; it adds up to 100 for
#'   each parent).
#'
#' @seealso [elsar_assign_sub_actions()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions)
#' riparian <- terra::init(x[["action"]], "x") < 30000
#' wetland <- terra::init(x[["action"]], "y") > 30000
#' rules <- elsar_sub_actions(
#'   "Restore",
#'   sub_action("Wetland restoration", wetland),
#'   sub_action("Riparian restoration", riparian)
#' )
#' elsar_sub_action_diagnostics(x, rules, c(riparian = riparian, wetland = wetland))
elsar_sub_action_diagnostics <- function(x, rules, layers = NULL,
                                         units = c("ha", "km2", "m2"),
                                         params = list()) {
  scheme <- .scheme_from_actions(x)
  units <- match.arg(units)
  unit_factor <- c(m2 = 1, ha = 1e-4, km2 = 1e-6)[[units]]
  .check_sub_action_rules(rules)
  .check_sub_action_parents(rules, scheme)
  layers <- .add_params(.condition_layers(layers, x), params)
  layers <- .add_protect_layers(layers, x, rules, scheme)

  action <- x[["action"]]
  levels(action) <- NULL
  cell_area <- .cell_area(x)
  total <- function(r) {
    v <- terra::global(r * cell_area, "sum", na.rm = TRUE)[[1]]
    if (is.na(v)) 0 else v * unit_factor
  }

  rows <- lapply(unique(rules$parent), function(p) {
    r <- rules[rules$parent == p, ]
    in_parent <- action == scheme$value[scheme$label == p]
    area_parent <- total(in_parent)
    free <- in_parent # cells not yet taken by an earlier rule
    out <- vector("list", nrow(r))
    for (i in seq_len(nrow(r))) {
      if (r$is_other[i]) {
        matched <- free
        na <- in_parent & FALSE
      } else {
        cond <- .eval_sub_action(r$quosure[[i]], r$label[i], layers, action)
        na <- in_parent & is.na(cond)
        cond <- terra::ifel(is.na(cond), FALSE, cond)
        matched <- in_parent & cond
      }
      assigned <- free & matched
      free <- free & !matched
      a_matched <- total(matched)
      a_assigned <- total(assigned)
      out[[i]] <- tibble::tibble(
        parent = p,
        sub_action = r$label[i],
        priority = r$priority[i],
        condition = r$condition[i],
        area_parent = area_parent,
        area_matched = a_matched,
        area_assigned = a_assigned,
        area_shadowed = a_matched - a_assigned,
        area_na = total(na),
        pct_of_parent = .safe_pct(a_assigned, area_parent)
      )
    }
    do.call(rbind, out)
  })
  out <- do.call(rbind, rows)
  attr(out, "units") <- units
  out
}

# Internal helpers ---------------------------------------------------------------

# Named single values that conditions can use beside the layers
.add_params <- function(layers, params) {
  if (is.null(params) || length(params) == 0) return(layers)
  assertthat::assert_that(
    is.list(params), !is.null(names(params)), all(nzchar(names(params))), !anyDuplicated(names(params)),
    msg = "'params' must be a named list, e.g. list(riparian_min = 0.5)."
  )
  ok <- vapply(params, function(v) is.atomic(v) && length(v) == 1 && !is.na(v), logical(1))
  assertthat::assert_that(
    all(ok),
    msg = paste0("Each of 'params' must be a single number, string or TRUE/FALSE: ",
                 toString(names(params)[!ok]), ".")
  )
  clash <- intersect(names(params), names(layers))
  assertthat::assert_that(
    length(clash) == 0,
    msg = paste0("'params' and 'layers' share names: ", toString(clash), ".")
  )
  c(layers, params)
}

# Full text of a condition (rlang::as_label() cuts long ones short with "...")
.expr_text <- function(expr) paste(deparse(expr, width.cutoff = 500L), collapse = " ")

.check_colours <- function(colours, n) {
  ok <- length(colours) == n && all(vapply(colours, function(x) {
    is.character(x) && !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
  }, logical(1)))
  assertthat::assert_that(ok, msg = paste0("Invalid colour: ", toString(colours), "."))
  invisible(TRUE)
}

.check_sub_action_rules <- function(rules) {
  assertthat::assert_that(
    inherits(rules, "elsar_sub_actions"),
    msg = "'rules' must be created with elsar_sub_actions()."
  )
  invisible(TRUE)
}

.check_sub_action_labels <- function(rules) {
  dups <- unique(rules$label[duplicated(rules$label)])
  assertthat::assert_that(
    length(dups) == 0,
    msg = paste0("Sub-action labels must be unique: ", toString(dups), ".")
  )
  invisible(TRUE)
}

# Parents must be actions of the scheme, and no sub-action may reuse the label
# of an action that is kept as it is
.check_sub_action_parents <- function(rules, scheme) {
  parents <- unique(rules$parent)
  unknown <- setdiff(parents, scheme$label)
  assertthat::assert_that(
    length(unknown) == 0,
    msg = paste0(
      "Sub-action parent(s) not in the action scheme: ", toString(unknown),
      ". The scheme's actions are: ", toString(scheme$label), "."
    )
  )
  not_action <- intersect(parents, scheme$label[!scheme$is_action])
  assertthat::assert_that(
    length(not_action) == 0,
    msg = paste0("No-action values cannot be split into sub-actions: ", toString(not_action), ".")
  )
  clash <- intersect(rules$label, setdiff(scheme$label, parents))
  assertthat::assert_that(
    length(clash) == 0,
    msg = paste0("Sub-action label(s) clash with an action in the scheme: ", toString(clash), ".")
  )
  invisible(TRUE)
}

# The scheme of the sub-action map: each parent replaced, in place, by its
# sub-actions; values renumbered 1..n in scheme order. `old_value` is the value
# of the action each row comes from.
.sub_action_scheme <- function(scheme, rules) {
  rows <- lapply(seq_len(nrow(scheme)), function(i) {
    s <- scheme[i, ]
    if (!s$label %in% rules$parent) {
      return(tibble::tibble(
        old_value = s$value, label = s$label, colour = s$colour, group = s$group,
        is_action = s$is_action, parent = s$label, is_sub = FALSE, is_other = FALSE,
        quosure = list(NULL)
      ))
    }
    r <- rules[rules$parent == s$label, ]
    r <- r[order(r$priority), ]
    shades <- .sub_action_colours(s$colour, nrow(r))
    tibble::tibble(
      old_value = s$value, label = r$label,
      colour = ifelse(is.na(r$colour), shades, r$colour),
      group = s$group, is_action = TRUE, parent = s$label,
      is_sub = TRUE, is_other = r$is_other, quosure = r$quosure
    )
  })
  out <- do.call(rbind, rows)
  out$value <- seq_len(nrow(out))
  out
}

# n shades of a colour, from darker to lighter; the parent's own colour for one
.sub_action_colours <- function(colour, n) {
  if (n == 1) return(colour)
  dark <- grDevices::colorRampPalette(c(colour, "black"))(10)[4]
  light <- grDevices::colorRampPalette(c(colour, "white"))(10)[7]
  grDevices::colorRampPalette(c(dark, colour, light))(n)
}

# Context layers on the action grid, as a named list of single-layer rasters
.condition_layers <- function(layers, x) {
  if (is.null(layers) || (is.list(layers) && length(layers) == 0)) return(list())
  template <- x[["action"]]
  if (is.character(layers) || inherits(layers, "SpatRaster")) {
    r <- .read_raster_arg(layers, "layers")
    layers <- as.list(r)
    names(layers) <- names(r)
  }
  assertthat::assert_that(
    is.list(layers), length(layers) > 0,
    !is.null(names(layers)), all(nzchar(names(layers))), !anyDuplicated(names(layers)),
    msg = "'layers' must be a SpatRaster or a named list with unique names."
  )
  out <- lapply(names(layers), function(nm) {
    l <- layers[[nm]]
    if (inherits(l, "SpatVector")) l <- sf::st_as_sf(l)
    if (inherits(l, c("sf", "sfc"))) {
      l <- sf::st_as_sf(l)
      if (nrow(l) == 0) return(terra::ifel(is.na(template), NA, 0))
      return(coverage_fraction_dissolved(template, l))
    }
    assertthat::assert_that(
      inherits(l, "SpatRaster"),
      msg = paste0("Layer '", nm, "' must be a SpatRaster, sf or SpatVector.")
    )
    l <- l[[1]]
    if (!terra::compareGeom(l, template, stopOnError = FALSE)) {
      message("Resampling layer '", nm, "' to the action raster grid.")
      method <- if (terra::is.factor(l)) "near" else "average"
      l <- terra::project(l, template, method = method)
    }
    l
  })
  names(out) <- names(layers)
  out
}

# Evaluate a rule's condition as a single-layer logical raster on the action grid
.eval_sub_action <- function(quo, label, layers, template) {
  res <- tryCatch(
    rlang::eval_tidy(quo, data = layers),
    error = function(e) {
      stop(
        "Could not evaluate the condition of sub-action '", label, "': ",
        conditionMessage(e), ". Layers available: ",
        if (length(layers)) toString(names(layers)) else "none", ".",
        call. = FALSE
      )
    }
  )
  if (is.logical(res) && length(res) == 1) {
    return(terra::ifel(is.na(template), NA, res))
  }
  assertthat::assert_that(
    inherits(res, "SpatRaster"), terra::nlyr(res) == 1,
    msg = paste0(
      "The condition of sub-action '", label, "' must give a single raster layer ",
      "(or TRUE/FALSE), not ", class(res)[1], "."
    )
  )
  res != 0
}

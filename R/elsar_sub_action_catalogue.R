# Default sub-action rules, and rules as tables ------------------------------------
#
# A global default rule set built only on layers elsar can make for any country,
# with the Kunming-Montreal Global Biodiversity Framework (GBF) targets each
# sub-action serves; and reading and writing rule sets as CSV, so they can live
# in a country's configuration. Conditions read from a table are parsed against
# an allow-list of operators, so a table cannot run arbitrary code.

#' The default sub-action catalogue
#'
#' Lists elsar's default sub-actions: how each splits an ELSA action, the
#' context layers it needs, and the targets of the Kunming-Montreal Global
#' Biodiversity Framework (GBF) it serves. [elsar_default_sub_actions()] turns
#' it into rules for [elsar_assign_sub_actions()].
#'
#' Every condition uses layers elsar can make for any country, named as in the
#' `layers` column. Pass them to [elsar_assign_sub_actions()] under those names:
#'
#' | Layer | Meaning | How to make it |
#' |---|---|---|
#' | `coastal` | Share of the planning unit within 10 km of the coast | `make_coastal_zone()` |
#' | `riparian` | Share within 1 km of a river | `make_riparian_zone()` |
#' | `mangroves` | Mangrove cover | [make_mangroves()] |
#' | `wetlands` | Wetlands and Ramsar sites | [make_wetlands_ramsar()] |
#' | `agriculture` | Cropland share (0-1), as used by the zones | [download_lulc_class_proportion()] |
#' | `pasture` | Pasture probability (0-1), as used by the Manage Zone | e.g. Global Pasture Watch |
#' | `forest` | Tree cover share (0-1) | [download_lulc_class_proportion()] |
#' | `forest_ecosystem` | Share in forest ecosystems, i.e. where forest is the natural cover (0-1) | e.g. ecoregion biomes or the IUCN Global Ecosystem Typology |
#'
#' Thresholds follow elsar's zone defaults where one exists (agriculture 0.1,
#' pasture 0.35). The GBF targets are indicative: Target 2 (restoration), 3
#' (protection, including OECMs), 8 (climate), 10 (sustainable agriculture and
#' forestry) and 11 (ecosystem services such as water and hazard regulation).
#'
#' Protect is split by [add_protect_sub_actions()]: its rows describe those
#' classes and need no layers, only the protected-area layer of the action map.
#'
#' @return A tibble with one row per sub-action and columns `id`, `parent`
#'   (`"protect"`, `"manage"` or `"restore"`), `priority`, `sub_action` (its
#'   name in the current language, see [elsar_labels()]), `condition`,
#'   `layers`, `gbf_targets` and `description`. The row with condition
#'   `"other"` takes a parent's remaining cells.
#' @seealso [elsar_default_sub_actions()]
#' @export
#'
#' @examples
#' catalogue <- elsar_sub_action_catalogue()
#' catalogue[, c("parent", "sub_action", "condition", "gbf_targets")]
elsar_sub_action_catalogue <- function() {
  rows <- list(
    # Manage: coast first, then the land uses in the order the Manage Zone uses
    c("coastal_management", "manage", "sa_coastal_management", "coastal > 0.5", "10, 11",
      "Management at the land-sea interface: erosion, pollution and hazard reduction"),
    c("sustainable_agriculture", "manage", "sa_sustainable_agriculture", "agriculture > 0.1", "10",
      "Agroforestry, soil conservation, reduced inputs and crop rotation on cropland"),
    c("sustainable_grazing", "manage", "sa_sustainable_grazing", "pasture > 0.35", "10",
      "Grazing management on pasture and rangeland"),
    c("sustainable_forest_management", "manage", "sa_sustainable_forest_management", "forest > 0.5", "10",
      "Reduced-impact logging and non-timber forest products in forest"),
    c("other_management", "manage", "sa_other_management", "other", "10",
      "Managed land that matches none of the above"),
    # Restore: the most specific settings first
    c("mangrove_restoration", "restore", "sa_mangrove_restoration", "mangroves > 0", "2, 8, 11",
      "Replanting and rehydrating mangroves"),
    c("wetland_restoration", "restore", "sa_wetland_restoration", "wetlands > 0", "2, 8, 11",
      "Rewetting peatlands and restoring wetlands and floodplains"),
    c("riparian_restoration", "restore", "sa_riparian_restoration", "riparian > 0.5", "2, 11",
      "Restoring vegetation and stabilising banks along rivers"),
    c("coastal_restoration", "restore", "sa_coastal_restoration", "coastal > 0.5", "2, 11",
      "Restoring dunes, coastal vegetation and other coastal ecosystems"),
    c("forest_restoration", "restore", "sa_forest_restoration", "forest_ecosystem > 0.5", "2, 8",
      "Active or assisted regeneration where forest is the natural cover"),
    c("other_restoration", "restore", "sa_other_restoration", "other", "2",
      "Restoring grasslands, shrublands and other non-forest ecosystems")
  )
  m <- do.call(rbind, rows)
  out <- tibble::tibble(
    id = m[, 1], parent = m[, 2],
    sub_action = unname(elsar_labels()[m[, 3]]),
    condition = m[, 4], gbf_targets = m[, 5], description = m[, 6]
  )
  out$layers <- vapply(out$condition, function(cond) {
    if (cond == "other") return("")
    toString(.condition_symbols(str2lang(cond)))
  }, character(1), USE.NAMES = FALSE)

  protect <- tibble::tibble(
    id = c("existing_pas", "pa_expansion", "pa_corridor", "pa_small_new", "pa_new"),
    parent = "protect",
    sub_action = unname(elsar_labels()[c("existing_pas", "pa_expansion", "pa_corridor", "pa_small_new", "pa_new")]),
    condition = c("protect:existing", "protect:expansion", "protect:corridor", "protect:small", "protect:new"),
    gbf_targets = "3",
    description = c(
      "Existing protected areas",
      "New protection touching (or near) one existing protected area",
      "New protection reaching two or more protected areas",
      "Isolated new protection below min_new_km2 (OECM candidates); only if min_new_km2 is set",
      "Isolated new protection"
    ),
    layers = ""
  )

  out <- rbind(protect, out)
  out$priority <- stats::ave(seq_len(nrow(out)), out$parent, FUN = seq_along)
  out[, c("id", "parent", "priority", "sub_action", "condition", "layers", "gbf_targets", "description")]
}

#' Build elsar's default sub-action rules
#'
#' Turns [elsar_sub_action_catalogue()] into rules for
#' [elsar_assign_sub_actions()], for the actions of the default ELSA scheme in
#' the current language.
#'
#' @param actions Which actions to split: any of `"protect"`, `"manage"` and
#'   `"restore"`.
#' @param drop Optional ids of catalogue sub-actions to leave out, e.g.
#'   `"sustainable_grazing"` where no pasture layer exists. Their cells go to
#'   the next matching rule. Remainder rows (`"other_management"`,
#'   `"other_restoration"`) cannot be dropped.
#' @param protect A list of arguments for [add_protect_sub_actions()], e.g.
#'   `list(expansion_km = 2, min_new_km2 = 5)`.
#' @param parents Optional named character vector of the actions' labels in
#'   your scheme, e.g. `c(manage = "Sustainable use")`, if they differ from
#'   elsar's defaults.
#'
#' @return An `elsar_sub_actions` rule set.
#' @seealso [elsar_sub_action_catalogue()], [elsar_sub_action_layers()]
#' @export
#'
#' @examples
#' rules <- elsar_default_sub_actions(drop = "sustainable_grazing",
#'                                    protect = list(expansion_km = 2))
#' rules
#' elsar_sub_action_layers(rules)
elsar_default_sub_actions <- function(actions = c("protect", "manage", "restore"),
                                      drop = NULL,
                                      protect = list(),
                                      parents = NULL) {
  actions <- match.arg(actions, several.ok = TRUE)
  cat <- elsar_sub_action_catalogue()
  assertthat::assert_that(
    is.null(drop) || (is.character(drop) && all(drop %in% cat$id)),
    msg = paste0("'drop' must be ids from elsar_sub_action_catalogue(): ", toString(cat$id), ".")
  )
  others <- cat$id[cat$condition == "other"]
  assertthat::assert_that(
    !any(drop %in% others),
    msg = paste0("Remainder sub-actions cannot be dropped: ", toString(intersect(drop, others)), ".")
  )
  assertthat::assert_that(
    is.list(protect),
    msg = "'protect' must be a list of arguments for add_protect_sub_actions()."
  )
  parent_labels <- c(protect = .txt("protect"), manage = .txt("manage"), restore = .txt("restore"))
  if (!is.null(parents)) {
    assertthat::assert_that(
      is.character(parents), all(names(parents) %in% names(parent_labels)),
      msg = "'parents' must be a named character vector with names among protect, manage and restore."
    )
    parent_labels[names(parents)] <- parents
  }

  rules <- NULL
  for (a in setdiff(actions, "protect")) {
    r <- cat[cat$parent == a & !cat$id %in% drop, ]
    subs <- lapply(which(r$condition != "other"), function(i) {
      .sub_action_from_text(r$sub_action[i], r$condition[i])
    })
    other <- r$sub_action[r$condition == "other"]
    new <- do.call(elsar_sub_actions, c(list(parent_labels[[a]]), subs, list(other = other)))
    rules <- if (is.null(rules)) new else .bind_rules(rules, new)
  }
  if ("protect" %in% actions) {
    rules <- do.call(add_protect_sub_actions, c(list(rules = rules, parent = parent_labels[["protect"]]), protect))
  }
  rules
}

#' Names of the layers sub-action rules need
#'
#' Lists the names the conditions of a rule set use: the context layers to
#' pass to [elsar_assign_sub_actions()] (and, for rules written in R, any
#' variables they refer to).
#'
#' @param rules Sub-action rules from [elsar_sub_actions()],
#'   [elsar_default_sub_actions()] or [elsar_read_sub_actions()].
#'
#' @return A sorted character vector. Protect splits need no layers, only the
#'   action map's protected-area layer.
#' @export
#'
#' @examples
#' elsar_sub_action_layers(elsar_default_sub_actions())
elsar_sub_action_layers <- function(rules) {
  .check_sub_action_rules(rules)
  rule_rows <- rules[!rules$is_other & rules$kind == "rule", ]
  syms <- unlist(lapply(rule_rows$quosure, function(q) .condition_symbols(rlang::quo_get_expr(q))))
  sort(unique(syms))
}

#' Read and write sub-action rules as CSV
#'
#' Keeps a rule set in a file, e.g. in a country's configuration, so it can be
#' reviewed, edited and reused. `elsar_sub_actions_from_table()` does the same
#' from a data frame.
#'
#' @section The table:
#' One row per sub-action, in priority order within each parent, with columns:
#' \describe{
#'   \item{parent}{Label of the action to split, as in the action scheme.}
#'   \item{sub_action}{Name of the sub-action.}
#'   \item{condition}{A condition over layer names, such as
#'     `riparian > 0.5 & !coastal`; `other` for the row that takes the
#'     parent's remaining cells (one per parent, last); or `protect:existing`,
#'     `protect:expansion`, `protect:corridor`, `protect:small` and
#'     `protect:new` for the classes of [add_protect_sub_actions()].}
#'   \item{colour}{Optional colour.}
#'   \item{params}{Optional, on Protect rows: arguments for
#'     [add_protect_sub_actions()] as `name=value` pairs separated by `;`, e.g.
#'     `pa_threshold=0.5; expansion_km=2; min_new_km2=5`.}
#' }
#'
#' Conditions may use only layer names, numbers, quoted strings, `TRUE`,
#' `FALSE`, comparisons (`>`, `>=`, `<`, `<=`, `==`, `!=`), `&`, `|`, `!`,
#' `is.na()`, arithmetic (`+`, `-`, `*`, `/`) and brackets. Anything else is
#' refused, so a rules file cannot run other code.
#'
#' When rules written in R refer to a variable (e.g. `forest > cutoff`), its
#' value is written into the file in its place, as long as it is a single
#' number, string or logical.
#'
#' @param path Path to a CSV file.
#' @param table A data frame with the columns described below.
#' @param rules Sub-action rules to write.
#'
#' @return `elsar_read_sub_actions()` and `elsar_sub_actions_from_table()`
#'   return an `elsar_sub_actions` rule set. `elsar_write_sub_actions()` writes
#'   the file and returns the table invisibly.
#' @seealso [elsar_sub_actions()], [elsar_default_sub_actions()]
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".csv")
#' elsar_write_sub_actions(elsar_default_sub_actions(), path)
#' read.csv(path)[, c("parent", "sub_action", "condition")]
#' rules <- elsar_read_sub_actions(path)
elsar_read_sub_actions <- function(path) {
  assertthat::assert_that(
    assertthat::is.string(path), file.exists(path),
    msg = paste0("Rules file not found: ", path)
  )
  table <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                           encoding = "UTF-8", check.names = FALSE)
  elsar_sub_actions_from_table(table)
}

#' @rdname elsar_read_sub_actions
#' @export
elsar_sub_actions_from_table <- function(table) {
  assertthat::assert_that(
    is.data.frame(table), all(c("parent", "sub_action", "condition") %in% names(table)),
    msg = "The rules table needs the columns parent, sub_action and condition."
  )
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  for (col in c("colour", "params")) if (!col %in% names(table)) table[[col]] <- NA_character_
  for (col in c("parent", "sub_action", "condition", "colour", "params")) {
    table[[col]] <- trimws(as.character(table[[col]]))
  }
  # Blank cells count as missing, as read.csv() reads them
  for (col in c("parent", "sub_action", "condition", "colour", "params")) {
    table[[col]][!is.na(table[[col]]) & table[[col]] == ""] <- NA_character_
  }
  bad <- is.na(table$parent) | is.na(table$sub_action) | is.na(table$condition)
  assertthat::assert_that(
    !any(bad),
    msg = paste0("Rows with an empty parent, sub_action or condition: ", toString(which(bad)), ".")
  )

  rules <- NULL
  for (p in unique(table$parent)) {
    r <- table[table$parent == p, ]
    is_protect <- startsWith(r$condition, "protect:")
    new <- if (any(is_protect)) {
      assertthat::assert_that(
        all(is_protect),
        msg = paste0("'", p, "' mixes protect: rows with other conditions. Split a parent one way.")
      )
      .protect_rules_from_rows(r, p)
    } else {
      .rules_from_rows(r, p)
    }
    rules <- if (is.null(rules)) new else .bind_rules(rules, new)
  }
  rules
}

#' @rdname elsar_read_sub_actions
#' @export
elsar_write_sub_actions <- function(rules, path) {
  .check_sub_action_rules(rules)
  rows <- lapply(seq_len(nrow(rules)), function(i) {
    r <- rules[i, ]
    if (r$kind == "protect") {
      p <- r$params[[1]]
      cls <- .protect_class_of(rules, i)
      params <- paste0(
        "pa_threshold=", p$pa_threshold, "; expansion_km=", p$expansion_km,
        if (!is.null(p$min_new_km2)) paste0("; min_new_km2=", p$min_new_km2) else ""
      )
      return(data.frame(parent = r$parent, sub_action = r$label, condition = paste0("protect:", cls),
                        colour = r$colour, params = params, stringsAsFactors = FALSE))
    }
    condition <- if (r$is_other) "other" else .expr_text(.inline_variables(r$quosure[[1]]))
    data.frame(parent = r$parent, sub_action = r$label, condition = condition,
               colour = r$colour, params = NA_character_, stringsAsFactors = FALSE)
  })
  table <- do.call(rbind, rows)
  utils::write.csv(table, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(table)
}

# Internal helpers ---------------------------------------------------------------

# Functions a condition read from text may call
.condition_functions <- c(">", ">=", "<", "<=", "==", "!=", "&", "|", "!", "(",
                          "is.na", "+", "-", "*", "/")

# Where conditions read from text are evaluated: only the allowed functions,
# so any other name must be a layer
.condition_env <- local({
  env <- new.env(parent = emptyenv())
  for (f in .condition_functions) assign(f, get(f, envir = baseenv()), envir = env)
  env
})

# Names (not functions) a condition uses
.condition_symbols <- function(expr) {
  if (is.symbol(expr)) return(as.character(expr))
  if (!is.call(expr)) return(character(0))
  unique(unlist(lapply(as.list(expr)[-1], .condition_symbols)))
}

# Parse a condition from text, refusing anything outside the allow-list
.parse_condition <- function(text, label) {
  expr <- tryCatch(str2lang(text), error = function(e) {
    stop("Sub-action '", label, "': cannot read the condition '", text, "': ",
         conditionMessage(e), call. = FALSE)
  })
  check <- function(e) {
    if (is.symbol(e) || is.numeric(e) || is.character(e) || is.logical(e)) return(invisible(TRUE))
    if (!is.call(e) || !is.symbol(e[[1]]) || !as.character(e[[1]]) %in% .condition_functions) {
      what <- if (is.call(e)) .expr_text(e[[1]]) else class(e)[1]
      stop("Sub-action '", label, "': '", what, "' is not allowed in a condition. Use layer names, ",
           "numbers, comparisons, &, |, !, is.na() and arithmetic.", call. = FALSE)
    }
    lapply(as.list(e)[-1], check)
    invisible(TRUE)
  }
  check(expr)
  rlang::new_quosure(expr, env = .condition_env)
}

.sub_action_from_text <- function(label, condition, colour = NULL) {
  q <- .parse_condition(condition, label)
  structure(
    list(label = label, condition = q, colour = colour %||% NA_character_),
    class = "elsar_sub_action"
  )
}

.rules_from_rows <- function(r, parent) {
  is_other <- r$condition == "other"
  assertthat::assert_that(
    sum(is_other) <= 1, !any(is_other) || utils::tail(is_other, 1),
    msg = paste0("'", parent, "' may have one 'other' row, and it must be its last.")
  )
  rule_rows <- r[!is_other, ]
  assertthat::assert_that(
    nrow(rule_rows) > 0,
    msg = paste0("'", parent, "' needs at least one condition besides 'other'.")
  )
  subs <- lapply(seq_len(nrow(rule_rows)), function(i) {
    colour <- if (is.na(rule_rows$colour[i])) NULL else rule_rows$colour[i]
    if (!is.null(colour)) .check_colours(colour, 1)
    .sub_action_from_text(rule_rows$sub_action[i], rule_rows$condition[i], colour)
  })
  args <- c(list(parent), subs)
  if (any(is_other)) {
    args$other <- r$sub_action[is_other]
    if (!is.na(r$colour[is_other])) args$other_colour <- r$colour[is_other]
  }
  do.call(elsar_sub_actions, args)
}

.protect_rules_from_rows <- function(r, parent) {
  classes <- sub("^protect:", "", r$condition)
  unknown <- setdiff(classes, names(.protect_codes))
  assertthat::assert_that(
    length(unknown) == 0,
    msg = paste0("Unknown Protect classes for '", parent, "': ", toString(unknown),
                 ". Use existing, expansion, corridor, small and new.")
  )
  assertthat::assert_that(
    all(c("existing", "expansion", "new") %in% classes),
    msg = paste0("Protect rows for '", parent, "' need at least protect:existing, protect:expansion and protect:new.")
  )
  params <- .parse_params(stats::na.omit(r$params)[1], parent)
  args <- c(list(parent = parent), params)
  args$corridors <- "corridor" %in% classes
  if ("small" %in% classes) {
    assertthat::assert_that(
      !is.null(args$min_new_km2),
      msg = paste0("protect:small for '", parent, "' needs min_new_km2 in params.")
    )
  } else {
    args$min_new_km2 <- NULL
  }
  args$labels <- stats::setNames(r$sub_action, classes)
  colours <- stats::setNames(r$colour, classes)
  if (any(!is.na(colours))) args$colours <- colours[!is.na(colours)]
  do.call(add_protect_sub_actions, args)
}

# "a=1; b=TRUE" -> list(a = 1, b = TRUE), for add_protect_sub_actions()
.parse_params <- function(text, parent) {
  if (length(text) == 0 || is.na(text) || !nzchar(text)) return(list())
  parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  allowed <- c("pa_threshold", "expansion_km", "min_new_km2")
  out <- list()
  for (p in parts) {
    kv <- trimws(strsplit(p, "=", fixed = TRUE)[[1]])
    value <- suppressWarnings(as.numeric(kv[2]))
    assertthat::assert_that(
      length(kv) == 2, kv[1] %in% allowed, !is.na(value),
      msg = paste0("Protect params for '", parent, "' must be name=number pairs with names among ",
                   toString(allowed), "; got '", p, "'.")
    )
    out[[kv[1]]] <- value
  }
  out
}

# Which Protect class a row of the rules holds (rows keep their class order)
.protect_class_of <- function(rules, i) {
  q <- rules$quosure[[i]]
  if (is.null(q)) return("new")
  code <- rlang::quo_get_expr(q)[[3]]
  names(.protect_codes)[.protect_codes == code]
}

# Replace variables a condition takes from its environment by their values
.inline_variables <- function(q) {
  env <- rlang::quo_get_env(q)
  walk <- function(e) {
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (!identical(env, .condition_env) && nzchar(nm) &&
          exists(nm, envir = env, inherits = TRUE)) {
        v <- get(nm, envir = env, inherits = TRUE)
        if (is.atomic(v) && length(v) == 1 && !is.function(v)) return(v)
      }
      return(e)
    }
    if (is.call(e)) {
      e[-1] <- lapply(as.list(e)[-1], walk)
      return(e)
    }
    e
  }
  walk(rlang::quo_get_expr(q))
}

# Combine two rule sets, keeping the class
.bind_rules <- function(a, b) {
  out <- rbind(tibble::as_tibble(a), tibble::as_tibble(b))
  .check_sub_action_labels(out)
  dup <- intersect(unique(a$parent), unique(b$parent))
  assertthat::assert_that(
    length(dup) == 0,
    msg = paste0("The rules split '", toString(dup), "' twice. Give each parent once.")
  )
  class(out) <- c("elsar_sub_actions", class(tibble::tibble()))
  out
}

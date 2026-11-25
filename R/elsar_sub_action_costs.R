# Cost envelope of sub-actions -----------------------------------------------------
#
# Multiplies sub-action areas by unit costs (low-high per hectare) to give a first
# budget envelope, nationally or per region. elsar ships no unit costs: they vary
# several-fold between countries and must come from national sources (e.g. NBSAP
# costing or BIOFIN), so elsar_cost_template() gives a table to fill in.

#' A table of unit costs to fill in
#'
#' Lists the sub-actions of a rule set (or of a summary) with empty unit-cost
#' columns, ready to be filled from national sources and passed to
#' [elsar_cost_sub_actions()]. Write it out with [utils::write.csv()] to fill
#' it in a spreadsheet.
#'
#' elsar provides no default costs. Unit costs of restoration and management
#' vary several-fold between countries and settings, so they should come from
#' national costing work (e.g. NBSAP resource mobilisation plans, BIOFIN
#' finance needs assessments) or local project budgets, with their source
#' recorded.
#'
#' @param x Sub-action rules from [elsar_sub_actions()] (or
#'   [elsar_default_sub_actions()]), or a summary from
#'   [elsar_summarise_actions()].
#' @param currency Currency code written into the table, e.g. `"USD"`.
#'
#' @return A tibble with one row per sub-action and columns `parent`,
#'   `sub_action`, `cost_low`, `cost_high` (per hectare; empty), `currency`,
#'   `per_year` (`FALSE`: a one-off cost; set `TRUE` for yearly costs) and
#'   `source` (empty).
#' @seealso [elsar_cost_sub_actions()]
#' @export
#'
#' @examples
#' costs <- elsar_cost_template(elsar_default_sub_actions(actions = "restore"))
#' costs
elsar_cost_template <- function(x, currency = "USD") {
  assertthat::assert_that(
    assertthat::is.string(currency),
    msg = "'currency' must be a single string, e.g. \"USD\"."
  )
  if (inherits(x, "elsar_sub_actions")) {
    parent <- x$parent
    sub <- x$label
  } else {
    .check_summary(x)
    s <- x[x$is_action, ]
    s <- s[!duplicated(as.character(s$action)), ]
    parent <- s$group
    sub <- as.character(s$action)
  }
  tibble::tibble(
    parent = parent, sub_action = sub,
    cost_low = NA_real_, cost_high = NA_real_,
    currency = currency, per_year = FALSE, source = NA_character_
  )
}

#' Cost envelope of sub-actions
#'
#' Multiplies the area of each sub-action by its unit costs to give a low and
#' high cost, nationally or per region: a first budget envelope for
#' implementing the ELSA map, not a costed plan.
#'
#' @section Unit costs:
#' `costs` is a data frame with one row per sub-action (see
#' [elsar_cost_template()]) and columns:
#' \describe{
#'   \item{sub_action}{Name of the sub-action, as in the summary.}
#'   \item{cost_low, cost_high}{Unit cost per hectare, low and high. Give the
#'     same value twice for a single estimate.}
#'   \item{cost_mid}{Optional central unit cost; by default the mean of low and
#'     high.}
#'   \item{currency}{Optional currency code; one per table.}
#'   \item{per_year}{Optional, `TRUE` where the cost recurs every year.}
#'   \item{`<name>_per_ha`}{Any further numeric column ending in `_per_ha`
#'     (e.g. `carbon_t_per_ha`) is multiplied by area too, and reported
#'     without the suffix (`carbon_t`).}
#' }
#' Sub-actions without a row, or with empty costs, are reported with `NA`
#' costs and listed in a message, so a partial table still gives totals for
#' what it covers.
#'
#' @param summary A summary of a sub-action map from
#'   [elsar_summarise_actions()], national or by region.
#' @param costs A data frame of unit costs (see below).
#'
#' @return A tibble with one row per region and costed sub-action, and columns
#'   `region`, `parent`, `sub_action`, `area_ha`, `cost_low`, `cost_mid`,
#'   `cost_high`, `currency` and `per_year`, plus one column per `_per_ha`
#'   column of `costs`.
#' @seealso [elsar_cost_template()], [elsar_plot_costs()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions)
#' summary <- elsar_summarise_actions(x)
#' costs <- elsar_cost_template(summary)
#' costs$cost_low <- c(50, 200, 1000, 5000)
#' costs$cost_high <- c(150, 600, 3000, 15000)
#' elsar_cost_sub_actions(summary, costs)
elsar_cost_sub_actions <- function(summary, costs) {
  .check_summary(summary)
  assertthat::assert_that(
    is.data.frame(costs), all(c("sub_action", "cost_low", "cost_high") %in% names(costs)),
    msg = "'costs' needs the columns sub_action, cost_low and cost_high (see elsar_cost_template())."
  )
  costs <- as.data.frame(costs, stringsAsFactors = FALSE)
  costs$sub_action <- as.character(costs$sub_action)
  assertthat::assert_that(
    !anyDuplicated(costs$sub_action),
    msg = paste0("Sub-actions listed twice in 'costs': ",
                 toString(unique(costs$sub_action[duplicated(costs$sub_action)])), ".")
  )
  if (!"cost_mid" %in% names(costs)) costs$cost_mid <- (costs$cost_low + costs$cost_high) / 2
  if (!"per_year" %in% names(costs)) costs$per_year <- FALSE
  costs$per_year <- as.logical(costs$per_year)
  currency <- if ("currency" %in% names(costs)) unique(stats::na.omit(costs$currency)) else character(0)
  assertthat::assert_that(
    length(currency) <= 1,
    msg = paste0("'costs' mixes currencies: ", toString(currency), ". Convert them to one first.")
  )
  currency <- if (length(currency) == 1) currency else NA_character_
  for (col in c("cost_low", "cost_mid", "cost_high")) {
    assertthat::assert_that(
      is.numeric(costs[[col]]), all(is.na(costs[[col]]) | costs[[col]] >= 0),
      msg = paste0("'", col, "' must hold numbers of 0 or more.")
    )
  }
  bad <- !is.na(costs$cost_low) & !is.na(costs$cost_high) &
    (costs$cost_low > costs$cost_high | costs$cost_mid < costs$cost_low | costs$cost_mid > costs$cost_high)
  assertthat::assert_that(
    !any(bad),
    msg = paste0("Costs must satisfy cost_low <= cost_mid <= cost_high: ", toString(costs$sub_action[bad]), ".")
  )
  extras <- grep("_per_ha$", names(costs), value = TRUE)
  for (col in extras) {
    assertthat::assert_that(is.numeric(costs[[col]]), msg = paste0("'", col, "' must be numeric."))
  }

  units <- attr(summary, "units") %||% "ha"
  to_ha <- c(ha = 1, km2 = 100, m2 = 1e-4)[[units]]
  s <- summary[summary$is_action, ]

  unknown <- setdiff(costs$sub_action, as.character(s$action))
  if (length(unknown) > 0) {
    warning("'costs' lists sub-actions not in the summary, which are ignored: ",
            toString(unknown), ".", call. = FALSE)
  }
  uncosted <- setdiff(unique(as.character(s$action)),
                      costs$sub_action[!is.na(costs$cost_low) & !is.na(costs$cost_high)])
  if (length(uncosted) > 0) {
    message("No unit costs for: ", toString(uncosted), ". Their costs are NA.")
  }

  m <- match(as.character(s$action), costs$sub_action)
  area_ha <- s$area * to_ha
  out <- tibble::tibble(
    region = s$region, parent = s$group, sub_action = as.character(s$action),
    area_ha = area_ha,
    cost_low = area_ha * costs$cost_low[m],
    cost_mid = area_ha * costs$cost_mid[m],
    cost_high = area_ha * costs$cost_high[m],
    currency = currency,
    per_year = costs$per_year[m]
  )
  for (col in extras) out[[sub("_per_ha$", "", col)]] <- area_ha * costs[[col]][m]
  out
}

#' Plot the cost envelope of sub-actions
#'
#' Draws the output of [elsar_cost_sub_actions()]: each sub-action's central
#' cost as a point, with a line from its low to its high cost, grouped by
#' parent action. Sub-actions without costs are left out.
#'
#' @param cost A tibble from [elsar_cost_sub_actions()].
#' @param region Optional region to plot; by default the national row, or the
#'   sum of all regions when the costs are by region.
#' @param title Optional plot title.
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object.
#' @seealso [elsar_cost_sub_actions()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' summary <- elsar_summarise_actions(elsar_prepare_actions(ex$actions))
#' costs <- elsar_cost_template(summary)
#' costs$cost_low <- c(50, 200, 1000, 5000)
#' costs$cost_high <- c(150, 600, 3000, 15000)
#' elsar_plot_costs(elsar_cost_sub_actions(summary, costs))
elsar_plot_costs <- function(cost, region = NULL, title = NULL, base_size = 11) {
  assertthat::assert_that(
    is.data.frame(cost), all(c("region", "parent", "sub_action", "cost_low", "cost_mid", "cost_high") %in% names(cost)),
    msg = "'cost' must be created with elsar_cost_sub_actions()."
  )
  d <- if (!is.null(region)) {
    assertthat::assert_that(
      region %in% cost$region,
      msg = paste0("'region' must be one of: ", toString(unique(cost$region)), ".")
    )
    cost[cost$region == region, ]
  } else {
    cost |>
      dplyr::group_by(.data$parent, .data$sub_action) |>
      dplyr::summarise(dplyr::across(c("cost_low", "cost_mid", "cost_high"), sum), .groups = "drop")
  }
  d <- d[!is.na(d$cost_mid), ]
  order <- unique(cost$sub_action)
  d$sub_action <- factor(d$sub_action, levels = rev(order[order %in% d$sub_action]))
  d$parent <- factor(d$parent, levels = unique(cost$parent))
  currency <- unique(stats::na.omit(cost$currency))
  per_year <- isTRUE(any(cost$per_year, na.rm = TRUE))
  x_title <- paste0(if (length(currency) == 1) paste0(currency, " ") else "",
                    if (per_year) "(yearly costs included)" else "")

  ggplot2::ggplot(d, ggplot2::aes(y = .data$sub_action)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$cost_low, xmax = .data$cost_high),
                            linewidth = 0.9, colour = "grey55") +
    ggplot2::geom_point(ggplot2::aes(x = .data$cost_mid), size = 2.4, colour = "grey15") +
    ggplot2::scale_x_continuous(labels = .label_area(), limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_y_discrete(labels = function(z) .wrap(z, 24)) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$parent), scales = "free_y", space = "free_y") +
    ggplot2::labs(x = trimws(x_title), y = NULL, title = title) +
    .results_theme(base_size)
}

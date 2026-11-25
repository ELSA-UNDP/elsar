# Cost envelope and the report's Sub-actions chapter.
# 2 x 4 grid of 1 km cells (1 cell = 100 ha): Protect, Manage, Manage, Restore / Manage, Restore, Restore, Do Nothing

cost_crs <- "ESRI:54009"

cost_actions <- function() {
  terra::rast(nrows = 2, ncols = 4, xmin = 0, xmax = 4000, ymin = 0, ymax = 2000,
              crs = cost_crs, vals = c(1, 2, 2, 3, 2, 3, 3, 5))
}

cost_sub <- function() {
  x <- elsar_prepare_actions(cost_actions())
  flag <- cost_actions()
  terra::values(flag) <- c(0, 1, 0, 1, 0, 0, 0, 0)
  rules <- elsar_sub_actions("Manage", sub_action("Farm", flag > 0)) |>
    add_sub_actions("Restore", sub_action("Wetland", flag > 0))
  list(x = x, rules = rules, sub = elsar_assign_sub_actions(x, rules, c(flag = flag)), flag = flag)
}

test_that("elsar_cost_template() lists the sub-actions with empty costs", {
  s <- cost_sub()
  t1 <- elsar_cost_template(s$rules, currency = "EUR")
  expect_equal(t1$sub_action, c("Farm", "Manage (other)", "Wetland", "Restore (other)"))
  expect_true(all(is.na(t1$cost_low)))
  expect_equal(unique(t1$currency), "EUR")
  t2 <- elsar_cost_template(elsar_summarise_actions(s$sub))
  expect_true(all(c("Farm", "Wetland", "Protect") %in% t2$sub_action))
  expect_error(elsar_cost_template(s$rules, currency = c("A", "B")), "single string")
})

test_that("elsar_cost_sub_actions() multiplies areas by unit costs, in hectares", {
  s <- cost_sub()
  costs <- data.frame(sub_action = c("Farm", "Wetland"), cost_low = c(10, 100), cost_high = c(30, 300),
                      currency = "USD", carbon_t_per_ha = c(1, 5))
  for (units in c("ha", "km2")) {
    summary <- elsar_summarise_actions(s$sub, units = units)
    out <- suppressMessages(elsar_cost_sub_actions(summary, costs))
    farm <- out[out$sub_action == "Farm", ]
    expect_equal(farm$area_ha, 100)
    expect_equal(c(farm$cost_low, farm$cost_mid, farm$cost_high), c(1000, 2000, 3000))
    expect_equal(out$carbon_t[out$sub_action == "Wetland"], 500)
    expect_equal(unique(out$currency), "USD")
  }
  expect_message(elsar_cost_sub_actions(elsar_summarise_actions(s$sub), costs), "No unit costs for")
  out <- suppressMessages(elsar_cost_sub_actions(elsar_summarise_actions(s$sub), costs))
  expect_true(is.na(out$cost_mid[out$sub_action == "Manage (other)"]))
})

test_that("costs per region add up to the national costs", {
  s <- cost_sub()
  sq <- function(x0, x1) sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 2000), c(x0, 2000), c(x0, 0))))
  regions <- sf::st_sf(name = c("West", "East"), geometry = sf::st_sfc(sq(0, 2000), sq(2000, 4000), crs = cost_crs))
  costs <- data.frame(sub_action = c("Farm", "Manage (other)"), cost_low = 1, cost_high = 3)
  nat <- suppressMessages(elsar_cost_sub_actions(elsar_summarise_actions(s$sub), costs))
  reg <- suppressMessages(elsar_cost_sub_actions(elsar_summarise_actions(s$sub, regions, "name"), costs))
  expect_equal(sum(reg$cost_mid, na.rm = TRUE), sum(nat$cost_mid, na.rm = TRUE))
  expect_s3_class(elsar_plot_costs(reg), "ggplot")
  expect_s3_class(elsar_plot_costs(reg, region = "West"), "ggplot")
  expect_error(elsar_plot_costs(reg, region = "North"), "one of")
})

test_that("elsar_cost_sub_actions() checks the unit costs", {
  summary <- elsar_summarise_actions(cost_sub()$sub)
  expect_error(elsar_cost_sub_actions(summary, data.frame(sub_action = "Farm", cost_low = 1)), "columns")
  expect_error(elsar_cost_sub_actions(summary, data.frame(sub_action = "Farm", cost_low = 5, cost_high = 1)), "cost_low <= cost_mid")
  expect_error(elsar_cost_sub_actions(summary, data.frame(sub_action = "Farm", cost_low = -1, cost_high = 1)), "0 or more")
  expect_error(elsar_cost_sub_actions(summary, data.frame(sub_action = c("Farm", "Farm"), cost_low = 1, cost_high = 2)), "twice")
  expect_error(
    elsar_cost_sub_actions(summary, data.frame(sub_action = c("Farm", "Wetland"), cost_low = 1, cost_high = 2,
                                               currency = c("USD", "EUR"))),
    "mixes currencies"
  )
  expect_warning(
    suppressMessages(elsar_cost_sub_actions(summary, data.frame(sub_action = c("Farm", "Nope"), cost_low = 1, cost_high = 2))),
    "not in the summary"
  )
})

test_that("action bars do not split existing PAs twice when Protect is split", {
  pa <- cost_actions()
  terra::values(pa) <- c(1, 0, 0, 0, 0, 0, 0, 0)
  x <- elsar_prepare_actions(cost_actions(), pas = pa)
  sub <- elsar_assign_sub_actions(x, add_protect_sub_actions())
  s <- elsar_summarise_actions(sub)
  expect_s3_class(elsar_plot_action_bars(s), "ggplot")
})

test_that("the report adds a Sub-actions chapter with diagnostics, sensitivity and costs", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  s <- cost_sub()
  rules <- elsar_sub_actions("Manage", sub_action("Farm", flag > flag_min))
  params <- list(flag_min = 0)
  sub <- elsar_assign_sub_actions(s$x, rules, c(flag = s$flag), params = params)
  d <- elsar_sub_action_diagnostics(s$x, rules, c(flag = s$flag), params = params)
  sens <- elsar_sub_action_sensitivity(s$x, rules, c(flag = s$flag), params = params,
                                       vary = list(flag_min = 2))
  costs <- data.frame(sub_action = "Farm", cost_low = 10, cost_high = 30, currency = "USD")
  out <- withr::local_tempfile(fileext = ".html")
  path <- suppressMessages(elsar_results_report(sub, diagnostics = d, sensitivity = sens, costs = costs,
                                                output_file = out, open = FALSE))
  # Pandoc 3 wraps long lines of HTML, so compare text with whitespace collapsed
  html <- gsub("\\s+", " ", paste(readLines(path, warn = FALSE), collapse = "\n"))
  expect_match(html, "Sub-actions")
  expect_match(html, "How the rules split each action")
  expect_match(html, "Sensitivity")
  expect_match(html, "Cost envelope")
  # Test the cost paragraph alone, so a failure shows that text, not the page head
  # The last mention is the section heading (an earlier one may be in the contents)
  at <- max(gregexpr("Cost envelope", html, fixed = TRUE)[[1]])
  costs_text <- substr(html, at, at + 600)
  expect_match(costs_text, "USD 1,000 to USD 3,000")

  # A map without sub-actions has no such chapter
  path <- suppressMessages(elsar_results_report(s$x, output_file = out, open = FALSE))
  expect_false(grepl("id=\"sub-actions\"", paste(readLines(path, warn = FALSE), collapse = "\n")))
})

# Sub-actions. Same 4 x 4 toy grid as test_elsar_results.R (1 cell = 100 ha):
#   1 1 2 2      1 Protect, 2 Manage, 3 Restore, 4 Urban Greening, 5 Do Nothing
#   1 3 2 5
#   5 5 4 5
#   5 NA 5 5
# Manage cells: row 1 col 3, row 1 col 4, row 2 col 3. Restore: row 2 col 2.

sa_crs <- "ESRI:54009"

sa_actions <- function() {
  terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4000, ymin = 0, ymax = 4000,
    crs = sa_crs, vals = c(1, 1, 2, 2, 1, 3, 2, 5, 5, 5, 4, 5, 5, NA, 5, 5)
  )
}

sa_pas <- function() {
  r <- sa_actions()
  terra::values(r) <- c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  r
}

# col: column number 1-4; top: 1 in row 1, else 0
sa_layers <- function() {
  r <- sa_actions()
  col <- terra::init(r, "col")
  top <- terra::init(r, "row") == 1
  c(col = col, top = top)
}

sa_area <- function(s, action) sum(s$area[s$action == action])

test_that("elsar_sub_actions() builds ordered rules with an 'other' remainder", {
  rules <- elsar_sub_actions(
    "Manage",
    sub_action("Edge", col == 4),
    sub_action("Top", top)
  )
  expect_s3_class(rules, "elsar_sub_actions")
  expect_equal(rules$label, c("Edge", "Top", "Manage (other)"))
  expect_equal(rules$priority, 1:3)
  expect_equal(rules$is_other, c(FALSE, FALSE, TRUE))
  expect_equal(rules$condition[1:2], c("col == 4", "top"))
  expect_output(print(rules), "1. Edge: col == 4")

  rules <- add_sub_actions(rules, "Restore", sub_action("Any", TRUE), other = "Rest")
  expect_equal(unique(rules$parent), c("Manage", "Restore"))
  expect_s3_class(rules, "elsar_sub_actions")
})

test_that("rule constructors validate their input", {
  expect_error(elsar_sub_actions("Manage"), "at least one")
  expect_error(elsar_sub_actions("Manage", "x > 1"), "sub_action")
  expect_error(sub_action("A"), "needs a condition")
  expect_error(sub_action("A", x > 1, colour = "notacolour"), "Invalid colour")
  expect_error(
    elsar_sub_actions("Manage", sub_action("A", top), sub_action("A", col > 1)),
    "unique"
  )
  rules <- elsar_sub_actions("Manage", sub_action("A", top))
  expect_error(add_sub_actions(rules, "Manage", sub_action("B", top)), "already split")
  expect_error(add_sub_actions(list(), "Manage", sub_action("B", top)), "elsar_sub_actions")
})

test_that("the default 'other' label follows the plot language", {
  old <- options(elsar.language = "es")
  on.exit(options(old))
  rules <- elsar_sub_actions("Manejo", sub_action("A", top))
  expect_equal(rules$label[2], "Manejo (otros)")
})

test_that("elsar_assign_sub_actions() splits a parent; the first matching rule wins", {
  x <- elsar_prepare_actions(sa_actions(), pas = sa_pas())
  # Row 1 col 4 matches both rules: Edge comes first and takes it
  rules <- elsar_sub_actions(
    "Manage",
    sub_action("Edge", col == 4),
    sub_action("Top", top)
  )
  sub <- elsar_assign_sub_actions(x, rules, sa_layers())

  expect_equal(names(sub), c("action", "pa_fraction"))
  ct <- terra::cats(sub[["action"]])[[1]]
  expect_equal(
    ct$label,
    c("Protect", "Edge", "Top", "Manage (other)", "Restore", "Urban Greening", "Do Nothing")
  )
  expect_equal(ct$group[ct$parent == "Manage"], rep("Manage", 3))
  expect_true(all(ct$is_action[2:4]))

  lab <- as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])
  expect_equal(lab[c(3, 4, 7)], c("Top", "Edge", "Manage (other)"))
  expect_equal(lab[c(1, 6, 11)], c("Protect", "Restore", "Urban Greening"))
  expect_true(is.na(lab[14]))

  # Sub-actions add up to the parent, and summaries work on the result
  s0 <- elsar_summarise_actions(x)
  s <- elsar_summarise_actions(sub)
  expect_equal(sum(s$area[s$group == "Manage"]), sa_area(s0, "Manage"))
  expect_equal(sa_area(s, "Edge"), 100)
  expect_equal(sa_area(s, "Protect"), sa_area(s0, "Protect"))
  expect_equal(sum(s$area_in_pa), sum(s0$area_in_pa))
})

test_that("cells where a condition is NA go to the next rule or 'other'", {
  x <- elsar_prepare_actions(sa_actions())
  l <- sa_layers()
  l$top <- terra::ifel(terra::init(l$top, "col") == 4, NA, l$top)
  rules <- elsar_sub_actions("Manage", sub_action("Top", top))
  sub <- elsar_assign_sub_actions(x, rules, l)
  lab <- as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])
  expect_equal(lab[c(3, 4, 7)], c("Top", "Manage (other)", "Manage (other)"))
})

test_that("conditions can be constants, use variables and polygon layers", {
  x <- elsar_prepare_actions(sa_actions())
  cutoff <- 3
  poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(3000, 3000), c(4000, 3000), c(4000, 4000), c(3000, 4000), c(3000, 3000)))),
    crs = sa_crs
  ))
  rules <- elsar_sub_actions(
    "Manage",
    sub_action("In polygon", zone > 0.5),
    sub_action("Beyond cutoff", col >= cutoff)
  ) |>
    add_sub_actions("Restore", sub_action("All restore", TRUE))
  sub <- elsar_assign_sub_actions(x, rules, list(zone = poly, col = sa_layers()$col))
  lab <- as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])
  expect_equal(lab[c(3, 4, 6, 7)], c("Beyond cutoff", "In polygon", "All restore", "Beyond cutoff"))
})

test_that("elsar_assign_sub_actions() reports bad rules clearly", {
  x <- elsar_prepare_actions(sa_actions())
  l <- sa_layers()
  expect_error(
    elsar_assign_sub_actions(x, elsar_sub_actions("Conserve", sub_action("A", top)), l),
    "not in the action scheme"
  )
  expect_error(
    elsar_assign_sub_actions(x, elsar_sub_actions("Do Nothing", sub_action("A", top)), l),
    "No-action"
  )
  expect_error(
    elsar_assign_sub_actions(x, elsar_sub_actions("Manage", sub_action("Protect", top)), l),
    "clash"
  )
  expect_error(
    elsar_assign_sub_actions(x, elsar_sub_actions("Manage", sub_action("A", nope > 1)), l),
    "sub-action 'A'.*Layers available: col, top"
  )
  expect_error(
    elsar_assign_sub_actions(x, elsar_sub_actions("Manage", sub_action("A", c(1, 2))), l),
    "single raster layer"
  )
})

test_that("custom colours are kept and default shades differ from each other", {
  x <- elsar_prepare_actions(sa_actions())
  rules <- elsar_sub_actions(
    "Manage",
    sub_action("Edge", col == 4, colour = "#123456"),
    sub_action("Top", top)
  )
  ct <- terra::cats(elsar_assign_sub_actions(x, rules, sa_layers())[["action"]])[[1]]
  manage <- ct[ct$parent == "Manage", ]
  expect_equal(manage$colour[1], "#123456")
  expect_false(anyDuplicated(manage$colour) > 0)
})

test_that("elsar_sub_action_diagnostics() shows matched, assigned and shadowed area", {
  x <- elsar_prepare_actions(sa_actions())
  rules <- elsar_sub_actions(
    "Manage",
    sub_action("Edge", col == 4),
    sub_action("Top", top)
  )
  d <- elsar_sub_action_diagnostics(x, rules, sa_layers())
  expect_equal(d$sub_action, c("Edge", "Top", "Manage (other)"))
  expect_equal(d$area_parent, rep(300, 3))
  expect_equal(d$area_matched, c(100, 200, 100))
  expect_equal(d$area_assigned, c(100, 100, 100))
  expect_equal(d$area_shadowed, c(0, 100, 0))
  expect_equal(sum(d$pct_of_parent), 100)
  expect_equal(attr(d, "units"), "ha")

  # NA cells of a condition are reported
  l <- sa_layers()
  l$top <- terra::ifel(terra::init(l$top, "col") == 4, NA, l$top)
  d <- elsar_sub_action_diagnostics(x, elsar_sub_actions("Manage", sub_action("Top", top)), l)
  expect_equal(d$area_na, c(100, 0))
})

test_that("sub-action maps plot and stack by parent action", {
  x <- elsar_prepare_actions(sa_actions())
  rules <- elsar_sub_actions("Manage", sub_action("Edge", col == 4))
  sub <- elsar_assign_sub_actions(x, rules, sa_layers())
  expect_s3_class(elsar_plot_action_bars(elsar_summarise_actions(sub)), "ggplot")
  expect_s3_class(elsar_plot_actions(sub), "ggplot")
})

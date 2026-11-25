# Protect sub-actions on a 3 x 9 grid of 1 km cells (1 cell = 1 km2):
#   E1 C  C  E2 .  X  .  N  N      E = Protect inside a PA, others Protect outside
#   M  R  U  .  .  .  .  N  N      M Manage, R Restore, U Urban Greening, . Do Nothing
#   .  .  .  .  .  .  .  .  .
# C touches both PAs (corridor); X is 2 km from E2 and 1 km2 (expansion within
# 2 km, else a small new area); N is a 4 km2 patch far from any PA (new PA).

pc_crs <- "ESRI:54009"

pc_actions <- function() {
  terra::rast(
    nrows = 3, ncols = 9, xmin = 0, xmax = 9000, ymin = 0, ymax = 3000, crs = pc_crs,
    vals = c(
      1, 1, 1, 1, 5, 1, 5, 1, 1,
      2, 3, 4, 5, 5, 5, 5, 1, 1,
      5, 5, 5, 5, 5, 5, 5, 5, 5
    )
  )
}

pc_pas <- function() {
  r <- pc_actions()
  v <- rep(0, 27)
  v[c(1, 4)] <- 1
  terra::values(r) <- v
  r
}

pc_x <- function() elsar_prepare_actions(pc_actions(), pas = pc_pas())

pc_labels <- function(sub) as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])

test_that("add_protect_sub_actions() builds the Protect rules", {
  r <- add_protect_sub_actions(min_new_km2 = 1.5)
  expect_s3_class(r, "elsar_sub_actions")
  expect_equal(r$label, c("Existing PAs", "PA expansion", "Corridor between PAs", "Small new area", "New PA"))
  expect_equal(r$is_other, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(unique(r$kind), "protect")
  expect_equal(r$colour[1], "#005000")
  expect_output(print(r), "Corridor between PAs: reaches 2\\+ PAs")

  r <- add_protect_sub_actions(corridors = FALSE)
  expect_equal(r$label, c("Existing PAs", "PA expansion", "New PA"))
})

test_that("Protect cells are classed as existing, corridor, small and new", {
  sub <- elsar_assign_sub_actions(pc_x(), add_protect_sub_actions(min_new_km2 = 1.5))
  lab <- pc_labels(sub)
  expect_equal(lab[c(1, 4)], c("Existing PAs", "Existing PAs"))
  expect_equal(lab[2:3], c("Corridor between PAs", "Corridor between PAs"))
  expect_equal(lab[6], "Small new area")
  expect_equal(lab[c(8, 9, 17, 18)], rep("New PA", 4))
  expect_equal(lab[10:12], c("Manage", "Restore", "Urban Greening"))
  expect_equal(lab[5], "Do Nothing")

  ct <- terra::cats(sub[["action"]])[[1]]
  expect_true(all(ct$group[ct$parent == "Protect"] == "Protect"))
})

test_that("expansion_km, corridors and min_new_km2 change the classes", {
  x <- pc_x()
  # X is 2 km from E2: an expansion within 2 km
  lab <- pc_labels(elsar_assign_sub_actions(x, add_protect_sub_actions(expansion_km = 2)))
  expect_equal(lab[6], "PA expansion")
  expect_equal(lab[8], "New PA")
  # Without corridors, C is an expansion; without min_new_km2, X is a new PA
  lab <- pc_labels(elsar_assign_sub_actions(x, add_protect_sub_actions(corridors = FALSE)))
  expect_equal(lab[2:3], c("PA expansion", "PA expansion"))
  expect_equal(lab[6], "New PA")
  # A threshold above every patch makes N small too
  lab <- pc_labels(elsar_assign_sub_actions(x, add_protect_sub_actions(min_new_km2 = 10)))
  expect_equal(lab[c(6, 8)], c("Small new area", "Small new area"))
})

test_that("Protect sub-actions add up to Protect and keep the PA split", {
  x <- pc_x()
  sub <- elsar_assign_sub_actions(x, add_protect_sub_actions(min_new_km2 = 1.5))
  s0 <- elsar_summarise_actions(x, units = "km2")
  s <- elsar_summarise_actions(sub, units = "km2")
  expect_equal(sum(s$area[s$group == "Protect"]), s0$area[s0$action == "Protect"])
  expect_equal(s$area[s$action == "Existing PAs"], 2)
  expect_equal(s$area_in_pa[s$action == "Existing PAs"], 2)
  expect_equal(s$area[s$action == "New PA"], 4)
  # The protection gap still counts every Protect sub-action
  g0 <- elsar_protection_gap(s0)
  g <- elsar_protection_gap(s)
  expect_equal(g$total, g0$total)
})

test_that("Protect rules combine with other rules and in diagnostics", {
  x <- pc_x()
  rules <- elsar_sub_actions("Manage", sub_action("All manage", TRUE)) |>
    add_protect_sub_actions(min_new_km2 = 1.5)
  sub <- elsar_assign_sub_actions(x, rules)
  expect_equal(pc_labels(sub)[c(2, 10)], c("Corridor between PAs", "All manage"))

  d <- elsar_sub_action_diagnostics(x, rules, units = "km2")
  d <- d[d$parent == "Protect", ]
  expect_equal(d$area_assigned, c(2, 0, 2, 1, 4))
  expect_equal(sum(d$area_shadowed), 0)
  expect_equal(sum(d$pct_of_parent), 100)
})

test_that("labels and colours can be overridden", {
  r <- add_protect_sub_actions(
    labels = c(new = "Proposed PA", existing = "Current PA"),
    colours = c(new = "#AAAAAA")
  )
  expect_equal(r$label, c("Current PA", "PA expansion", "Corridor between PAs", "Proposed PA"))
  sub <- elsar_assign_sub_actions(pc_x(), r)
  ct <- terra::cats(sub[["action"]])[[1]]
  expect_equal(ct$colour[ct$label == "Proposed PA"], "#AAAAAA")
  expect_error(add_protect_sub_actions(labels = c(newish = "x")), "names among")
  expect_error(add_protect_sub_actions(colours = c(new = "nocolour")), "Invalid colour")
})

test_that("the labels follow the plot language", {
  old <- options(elsar.language = "es")
  on.exit(options(old))
  r <- add_protect_sub_actions()
  expect_equal(r$parent[1], "Protección")
  expect_equal(r$label[4], "Nueva área protegida")
})

test_that("a map where all Protect is inside PAs has only existing PAs", {
  a <- pc_actions()
  a[a == 1] <- 5
  a[c(1, 4)] <- 1
  x <- elsar_prepare_actions(a, pas = pc_pas())
  sub <- elsar_assign_sub_actions(x, add_protect_sub_actions())
  s <- elsar_summarise_actions(sub)
  expect_equal(s$area[s$action == "Existing PAs"], 200)
  expect_equal(sum(s$area[s$group == "Protect"]), 200)
})

test_that("add_protect_sub_actions() reports bad input clearly", {
  expect_error(add_protect_sub_actions(pa_threshold = 0), "pa_threshold")
  expect_error(add_protect_sub_actions(expansion_km = -1), "expansion_km")
  expect_error(add_protect_sub_actions(min_new_km2 = 0), "min_new_km2")
  expect_error(add_protect_sub_actions(add_protect_sub_actions()), "already split")
  x <- elsar_prepare_actions(pc_actions())
  expect_error(elsar_assign_sub_actions(x, add_protect_sub_actions()), "pas")
  lonlat <- terra::project(pc_x(), "EPSG:4326", method = "near")
  expect_error(elsar_assign_sub_actions(lonlat, add_protect_sub_actions()), "projected")
})

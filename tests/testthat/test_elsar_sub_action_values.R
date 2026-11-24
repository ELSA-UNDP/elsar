# Value summaries and sensitivity, on a 2 x 4 grid of 1 km cells (1 cell = 100 ha):
#   1 2 2 3      1 Protect, 2 Manage, 3 Restore, 5 Do Nothing
#   2 3 3 5

val_crs <- "ESRI:54009"

val_actions <- function() {
  terra::rast(nrows = 2, ncols = 4, xmin = 0, xmax = 4000, ymin = 0, ymax = 2000,
              crs = val_crs, vals = c(1, 2, 2, 3, 2, 3, 3, 5))
}

val_layer <- function(v) {
  r <- val_actions()
  terra::values(r) <- v
  r
}

# suit: Manage cells 0.1, 0.6, 0.9; Restore cells 0.3, 0.7, NA
val_suit <- function() val_layer(c(0.5, 0.1, 0.6, 0.3, 0.9, 0.7, NA, 0.2))

val_regions <- function() {
  sq <- function(x0, x1) sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 2000), c(x0, 2000), c(x0, 0))))
  sf::st_sf(name = c("West", "East"), geometry = sf::st_sfc(sq(0, 2000), sq(2000, 4000), crs = val_crs))
}

pick <- function(v, action, class, col = "area") v[[col]][v$action == action & v$class == class]

test_that("elsar_summarise_values() splits each action by fixed classes, with no data reported", {
  x <- elsar_prepare_actions(val_actions())
  v <- elsar_summarise_values(x, c(suit = val_suit()), breaks = c(0, 0.5, 1))
  expect_equal(levels(v$class), c("0–0.5", "0.5–1", "No data"))
  expect_equal(pick(v, "Manage", "0–0.5"), 100)
  expect_equal(pick(v, "Manage", "0.5–1"), 200)
  expect_equal(pick(v, "Restore", "0–0.5"), 100)
  expect_equal(pick(v, "Restore", "No data"), 100)
  expect_equal(pick(v, "Manage", "0.5–1", "pct_of_action"), 200 / 3)
  # Every action's classes add up to its area
  totals <- tapply(v$area, v$action, sum)
  s <- elsar_summarise_actions(x)
  expect_equal(as.numeric(totals[c("Protect", "Manage", "Restore")]),
               s$area[match(c("Protect", "Manage", "Restore"), s$action)])
  expect_equal(v$lower[v$class == "0.5–1"][1], 0.5)
  expect_equal(attr(v, "units"), "ha")
})

test_that("values beyond the breaks fall in the outer classes", {
  x <- elsar_prepare_actions(val_actions())
  v <- elsar_summarise_values(x, c(suit = val_suit()), breaks = c(0.2, 0.5, 0.8))
  expect_equal(pick(v, "Manage", "0.2–0.5"), 100) # 0.1, below the lowest break
  expect_equal(pick(v, "Manage", "0.5–0.8"), 200) # 0.6 and 0.9, above the highest
})

test_that("quantile classes are shared by all actions", {
  x <- elsar_prepare_actions(val_actions())
  v <- elsar_summarise_values(x, c(suit = val_suit()), breaks = 2)
  # Action cells (Protect, Manage, Restore) with values: 0.5, 0.1, 0.6, 0.3, 0.9, 0.7 -> median 0.55
  expect_equal(nlevels(v$class), 3)
  expect_equal(v$upper[v$class == levels(v$class)[1]][1], 0.55)
  expect_equal(sum(v$area[v$action == "Manage" & v$class != "No data"]), 300)
  expect_message(
    elsar_summarise_values(x, c(flat = val_layer(c(0, 0, 0, 0, 0, 0, 0, 1))), breaks = 4),
    "collapse"
  )
})

test_that("categorical layers keep their categories", {
  x <- elsar_prepare_actions(val_actions())
  eco <- val_layer(c(1, 1, 2, 2, 1, 2, 2, 1))
  levels(eco) <- data.frame(id = 1:2, ecoregion = c("Forest", "Grassland"))
  v <- elsar_summarise_values(x, c(eco = eco))
  expect_equal(levels(v$class), c("Forest", "Grassland", "No data"))
  expect_equal(pick(v, "Restore", "Grassland"), 300)
  expect_equal(pick(v, "Manage", "Forest"), 200)
})

test_that("values can be summarised per region and per layer", {
  x <- elsar_prepare_actions(val_actions())
  layers <- c(suit = val_suit(), other = val_layer(c(1, 2, 3, 4, 5, 6, 7, 8)))
  v <- elsar_summarise_values(x, layers, breaks = list(suit = c(0, 0.5, 1), other = c(0, 4, 8)),
                              regions = val_regions(), region_col = "name", units = "km2")
  expect_setequal(as.character(unique(v$region)), c("West", "East"))
  expect_setequal(as.character(unique(v$layer)), c("suit", "other"))
  w <- v[v$region == "West" & v$layer == "suit" & v$action == "Manage", ]
  expect_equal(sum(w$area), 2)
  expect_error(elsar_summarise_values(x, layers, breaks = list(suit = 3)), "every layer")
  expect_error(elsar_summarise_values(x, layers, breaks = c(1, 0.5)), "increasing")
  expect_error(elsar_summarise_values(x, layers, breaks = 1), "2 or more")
})

test_that("sub-action maps can be summarised and plotted", {
  x <- elsar_prepare_actions(val_actions())
  rules <- elsar_sub_actions("Manage", sub_action("Suitable", suit > 0.5))
  sub <- elsar_assign_sub_actions(x, rules, c(suit = val_suit()))
  v <- elsar_summarise_values(sub, c(suit = val_suit()), breaks = c(0, 0.5, 1))
  expect_equal(pick(v, "Suitable", "0.5–1"), 200)
  expect_equal(pick(v, "Manage (other)", "0–0.5"), 100)
  expect_s3_class(elsar_plot_values(v), "ggplot")
  expect_error(elsar_plot_values(v, layer = "nope"), "one of")
})

test_that("params give conditions named thresholds", {
  x <- elsar_prepare_actions(val_actions())
  rules <- elsar_sub_actions("Manage", sub_action("Suitable", suit > suit_min))
  lab <- function(p) {
    sub <- elsar_assign_sub_actions(x, rules, c(suit = val_suit()), params = list(suit_min = p))
    as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])[c(2, 3, 5)]
  }
  expect_equal(lab(0.5), c("Manage (other)", "Suitable", "Suitable"))
  expect_equal(lab(0.8), c("Manage (other)", "Manage (other)", "Suitable"))
  expect_error(elsar_assign_sub_actions(x, rules, c(suit = val_suit())), "suit_min")
  expect_error(elsar_assign_sub_actions(x, rules, c(suit = val_suit()), params = list(suit = 1)), "share names")
  expect_error(elsar_assign_sub_actions(x, rules, c(suit = val_suit()), params = list(suit_min = 1:2)), "single")

  # Thresholds named in a rules file too
  table_rules <- elsar_sub_actions_from_table(
    data.frame(parent = "Manage", sub_action = "Suitable", condition = "suit > suit_min")
  )
  sub <- elsar_assign_sub_actions(x, table_rules, c(suit = val_suit()), params = list(suit_min = 0.8))
  expect_equal(as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])[5], "Suitable")
})

test_that("elsar_sub_action_sensitivity() varies thresholds, Protect settings and layers", {
  pa <- val_layer(c(1, 0, 0, 0, 0, 0, 0, 0))
  x <- elsar_prepare_actions(val_actions(), pas = pa)
  rules <- elsar_sub_actions("Manage", sub_action("Suitable", suit > suit_min)) |>
    add_protect_sub_actions()
  alt <- val_layer(rep(1, 8))
  s <- elsar_sub_action_sensitivity(
    x, rules, c(suit = val_suit()),
    params = list(suit_min = 0.5),
    vary = list(suit_min = c(0.05, 0.8), suit = list(all = alt), expansion_km = 3),
    units = "km2"
  )
  expect_equal(attr(s, "units"), "km2")
  base <- s[s$is_baseline, ]
  expect_equal(base$area[base$sub_action == "Suitable"], 2)
  expect_true(all(base$change == 0))
  got <- function(p, v, sa) s$area[s$parameter == p & s$value == v & s$sub_action == sa]
  expect_equal(got("suit_min", "0.05", "Suitable"), 3)
  expect_equal(got("suit_min", "0.8", "Suitable"), 1)
  expect_equal(got("suit", "all", "Suitable"), 3)
  expect_equal(s$pct_change[s$parameter == "suit_min" & s$value == "0.8" & s$sub_action == "Suitable"], -50)
  # Protect: the only Protect cell is an existing PA, so expansion_km changes nothing
  expect_equal(got("expansion_km", "3", "Existing PAs"), 1)
  expect_s3_class(elsar_plot_sensitivity(s), "ggplot")
})

test_that("elsar_sub_action_sensitivity() checks what it is asked to vary", {
  x <- elsar_prepare_actions(val_actions())
  rules <- elsar_sub_actions("Manage", sub_action("Suitable", suit > suit_min))
  args <- list(x = x, rules = rules, layers = c(suit = val_suit()), params = list(suit_min = 0.5))
  expect_error(do.call(elsar_sub_action_sensitivity, c(args, list(vary = list(nope = 1)))), "Cannot vary 'nope'")
  expect_error(do.call(elsar_sub_action_sensitivity, c(args, list(vary = list(expansion_km = 1)))), "no Protect split")
  expect_error(do.call(elsar_sub_action_sensitivity, c(args, list(vary = list(suit = val_suit())))), "named list")
  expect_error(do.call(elsar_sub_action_sensitivity, c(args, list(vary = list(1)))), "named list")
  pa <- val_layer(c(1, 0, 0, 0, 0, 0, 0, 0))
  xp <- elsar_prepare_actions(val_actions(), pas = pa)
  expect_error(
    elsar_sub_action_sensitivity(xp, add_protect_sub_actions(), vary = list(min_new_km2 = 1)),
    "must set it"
  )
})

test_that("sensitivity works for rules that need no layers", {
  pa <- val_layer(c(1, 0, 0, 0, 0, 0, 0, 0))
  x <- elsar_prepare_actions(val_actions(), pas = pa)
  s <- elsar_sub_action_sensitivity(x, add_protect_sub_actions(), vary = list(expansion_km = c(2, 5)))
  expect_equal(sum(s$is_baseline), 4)
  expect_s3_class(elsar_plot_sensitivity(s), "ggplot")
})

test_that("the sensitivity plot draws percent changes by default", {
  pa <- val_layer(c(1, 0, 0, 0, 0, 0, 0, 0))
  x <- elsar_prepare_actions(val_actions(), pas = pa)
  rules <- elsar_sub_actions("Manage", sub_action("Suitable", suit > suit_min))
  s <- elsar_sub_action_sensitivity(x, rules, c(suit = val_suit()), params = list(suit_min = 0.5),
                                    vary = list(suit_min = c(0.05, 0.8, 0.95)), units = "km2")
  g <- elsar_plot_sensitivity(s)
  b <- ggplot2::ggplot_build(g)
  drawn <- b$data[[4]] # points of the settings tried (after the zero line and baseline)
  alt <- s[!s$is_baseline & s$change != 0, ]
  expect_setequal(round(drawn$x, 6), round(100 * alt$change / alt$baseline_area, 6))
  expect_true(any(drawn$x == -100))
  expect_equal(g$labels$x, "Change in area (%)")
  expect_match(b$layout$panel_scales_y[[1]]$get_labels(), "km²", all = FALSE)
  expect_s3_class(elsar_plot_sensitivity(s, metric = "area"), "ggplot")
  expect_error(elsar_plot_sensitivity(s, min_change = -1), "min_change")
})

test_that("the sensitivity plot leaves out changes from no area, and tiny changes", {
  pa <- val_layer(c(1, 0, 0, 0, 0, 0, 0, 0))
  x <- elsar_prepare_actions(val_actions(), pas = pa)
  rules <- elsar_sub_actions("Manage", sub_action("Suitable", suit > suit_min))
  # At 0.95 nothing is Suitable; from there, 0.5 creates Suitable from no area
  s <- elsar_sub_action_sensitivity(x, rules, c(suit = val_suit()), params = list(suit_min = 0.95),
                                    vary = list(suit_min = 0.5), units = "km2")
  g <- elsar_plot_sensitivity(s)
  expect_match(g$labels$caption, "Suitable \\(suit_min 0.5\\)")
  expect_null(elsar_plot_sensitivity(s, metric = "area")$labels$caption)
  # Manage (other) shrinks from 3 to 1 km2 (-66.7%): drawn at min_change 50, not at 70
  n_drawn <- function(m) nrow(ggplot2::ggplot_build(elsar_plot_sensitivity(s, min_change = m))$data[[4]])
  expect_equal(n_drawn(50), 1)
  expect_equal(n_drawn(70), 0)
  # A setting whose every change is small is named in the subtitle
  s2 <- elsar_sub_action_sensitivity(x, rules, c(suit = val_suit()), params = list(suit_min = 0.5),
                                     vary = list(suit_min = 0.55), units = "km2")
  expect_match(elsar_plot_sensitivity(s2, min_change = 70)$labels$subtitle, "suit_min: change under 70%")
})

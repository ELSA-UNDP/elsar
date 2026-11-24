# Default sub-action catalogue, and rules read from and written to tables.

cat_crs <- "ESRI:54009"

# 2 x 4 grid of 1 km cells: Protect, Manage, Manage, Restore / Manage, Restore, Restore, Do Nothing
cat_actions <- function() {
  terra::rast(nrows = 2, ncols = 4, xmin = 0, xmax = 4000, ymin = 0, ymax = 2000,
              crs = cat_crs, vals = c(1, 2, 2, 3, 2, 3, 3, 5))
}

cat_layers <- function() {
  r <- cat_actions()
  set <- function(v) { terra::values(r) <- v; r }
  c(
    coastal = set(c(0, 1, 0, 0, 0, 0, 0, 0)),
    agriculture = set(c(0, 0, 0.5, 0, 0, 0, 0, 0)),
    pasture = set(c(0, 0, 0, 0, 0.9, 0, 0, 0)),
    forest = set(c(0, 0, 0, 0, 0, 0, 0, 0)),
    mangroves = set(c(0, 0, 0, 0, 0, 0, 0, 0)),
    wetlands = set(c(0, 0, 0, 1, 0, 0, 0, 0)),
    riparian = set(c(0, 0, 0, 0, 0, 1, 0, 0)),
    forest_ecosystem = set(c(0, 0, 0, 0, 0, 0, 0, 0))
  )
}

cat_labels <- function(sub) as.character(terra::values(sub[["action"]], dataframe = TRUE)[[1]])

test_that("the catalogue lists every default sub-action once, with one remainder per action", {
  catalogue <- elsar_sub_action_catalogue()
  expect_s3_class(catalogue, "tbl_df")
  expect_false(anyDuplicated(catalogue$id) > 0)
  expect_setequal(unique(catalogue$parent), c("protect", "manage", "restore"))
  for (p in c("manage", "restore")) {
    r <- catalogue[catalogue$parent == p, ]
    expect_equal(sum(r$condition == "other"), 1)
    expect_equal(utils::tail(r$condition, 1), "other")
    expect_equal(r$priority, seq_len(nrow(r)))
  }
  expect_equal(catalogue$layers[catalogue$id == "riparian_restoration"], "riparian")
  expect_true(all(catalogue$gbf_targets[catalogue$parent == "protect"] == "3"))
  expect_true(all(nzchar(catalogue$sub_action)))
})

test_that("catalogue names follow the plot language", {
  old <- options(elsar.language = "es")
  on.exit(options(old))
  catalogue <- elsar_sub_action_catalogue()
  expect_equal(catalogue$sub_action[catalogue$id == "sustainable_agriculture"], "Agricultura sostenible")
  rules <- elsar_default_sub_actions(actions = "manage")
  expect_equal(unique(rules$parent), "Manejo")
})

test_that("elsar_default_sub_actions() builds rules that split an action map", {
  rules <- elsar_default_sub_actions()
  expect_s3_class(rules, "elsar_sub_actions")
  expect_setequal(unique(rules$parent), c("Protect", "Manage", "Restore"))
  expect_setequal(
    elsar_sub_action_layers(rules),
    c("agriculture", "coastal", "forest", "forest_ecosystem", "mangroves", "pasture", "riparian", "wetlands")
  )

  x <- elsar_prepare_actions(cat_actions(), pas = terra::ifel(cat_actions() == 1, 1, 0))
  lab <- cat_labels(elsar_assign_sub_actions(x, rules, cat_layers()))
  expect_equal(lab, c(
    "Existing PAs", "Coastal management", "Sustainable agriculture", "Wetland restoration",
    "Sustainable grazing", "Riparian restoration", "Other ecosystem restoration", "Do Nothing"
  ))
})

test_that("default rules can drop sub-actions, skip actions and rename parents", {
  rules <- elsar_default_sub_actions(actions = c("manage", "restore"), drop = "sustainable_grazing")
  expect_false("Sustainable grazing" %in% rules$label)
  expect_false("Protect" %in% rules$parent)
  expect_false("pasture" %in% elsar_sub_action_layers(rules))

  rules <- elsar_default_sub_actions(actions = "manage", parents = c(manage = "Sustainable use"))
  expect_equal(unique(rules$parent), "Sustainable use")

  rules <- elsar_default_sub_actions(actions = "protect", protect = list(min_new_km2 = 5))
  expect_true("Small new area" %in% rules$label)

  expect_error(elsar_default_sub_actions(drop = "nope"), "ids from")
  expect_error(elsar_default_sub_actions(drop = "other_management"), "cannot be dropped")
  expect_error(elsar_default_sub_actions(parents = c(conserve = "x")), "names among")
})

test_that("rules written to CSV read back and assign the same cells", {
  rules <- elsar_default_sub_actions(protect = list(expansion_km = 2, min_new_km2 = 5))
  path <- withr::local_tempfile(fileext = ".csv")
  table <- elsar_write_sub_actions(rules, path)
  expect_equal(nrow(table), nrow(rules))
  expect_true(all(c("parent", "sub_action", "condition", "colour", "params") %in% names(table)))

  back <- elsar_read_sub_actions(path)
  expect_equal(back$label, rules$label)
  expect_equal(back$condition, rules$condition)
  expect_equal(back$parent, rules$parent)
  expect_equal(back$params[[which(back$kind == "protect")[1]]]$expansion_km, 2)

  x <- elsar_prepare_actions(cat_actions(), pas = terra::ifel(cat_actions() == 1, 1, 0))
  expect_equal(
    cat_labels(elsar_assign_sub_actions(x, back, cat_layers())),
    cat_labels(elsar_assign_sub_actions(x, rules, cat_layers()))
  )
})

test_that("rules written in R keep their full condition and their variables' values", {
  cutoff <- 0.25
  rules <- elsar_sub_actions(
    "Manage",
    sub_action("Long", coastal > 0 | agriculture > 0 | pasture > 0 & !is.na(forest) & forest < cutoff)
  )
  expect_equal(rules$condition[1], "coastal > 0 | agriculture > 0 | pasture > 0 & !is.na(forest) & forest < cutoff")
  path <- withr::local_tempfile(fileext = ".csv")
  table <- elsar_write_sub_actions(rules, path)
  expect_equal(table$condition[1], "coastal > 0 | agriculture > 0 | pasture > 0 & !is.na(forest) & forest < 0.25")
  expect_equal(table$condition[2], "other")
})

test_that("conditions read from a table may only use allowed operators", {
  tbl <- function(cond) data.frame(parent = "Manage", sub_action = "A", condition = cond)
  ok <- elsar_sub_actions_from_table(tbl("(agriculture + pasture) / 2 >= 0.5 & !is.na(forest) | forest == 'x'"))
  expect_s3_class(ok, "elsar_sub_actions")

  for (bad in c("system('echo hi')", "file.remove('x') > 0", "base::sum(forest) > 1",
                "layers$forest > 1", "forest[1] > 0", "(forest <- 1) > 0", "sum(forest) > 1",
                "function(x) x")) {
    expect_error(elsar_sub_actions_from_table(tbl(bad)), "not allowed|cannot read", info = bad)
  }
  expect_error(elsar_sub_actions_from_table(tbl("forest >")), "cannot read")
})

test_that("a name that is not a layer fails clearly, and is never looked up elsewhere", {
  x <- elsar_prepare_actions(cat_actions())
  # 'pi' exists in base R; a condition from a table must not find it
  rules <- elsar_sub_actions_from_table(
    data.frame(parent = "Manage", sub_action = "A", condition = "pi > 3")
  )
  expect_error(elsar_assign_sub_actions(x, rules, cat_layers()), "object 'pi' not found")
})

test_that("tables are checked for their structure", {
  t <- function(...) data.frame(..., stringsAsFactors = FALSE)
  expect_error(elsar_sub_actions_from_table(t(parent = "Manage", condition = "x > 1")), "columns")
  expect_error(
    elsar_sub_actions_from_table(t(parent = "Manage", sub_action = c("A", "B"), condition = c("other", "x > 1"))),
    "last"
  )
  expect_error(
    elsar_sub_actions_from_table(t(parent = "Manage", sub_action = "A", condition = "other")),
    "at least one"
  )
  expect_error(
    elsar_sub_actions_from_table(t(parent = "Manage", sub_action = c("A", ""), condition = c("x > 1", "x > 2"))),
    "empty"
  )
  expect_error(
    elsar_sub_actions_from_table(t(parent = "Protect", sub_action = c("A", "B"),
                                   condition = c("protect:existing", "x > 1"))),
    "mixes"
  )
  expect_error(
    elsar_sub_actions_from_table(t(parent = "Protect", sub_action = c("A", "B"),
                                   condition = c("protect:existing", "protect:other"))),
    "Unknown Protect classes"
  )
  expect_error(
    elsar_sub_actions_from_table(t(parent = "Protect", sub_action = c("A", "B"),
                                   condition = c("protect:existing", "protect:new"))),
    "need at least"
  )
  protect <- t(parent = "Protect", sub_action = c("E", "X", "S", "N"),
               condition = c("protect:existing", "protect:expansion", "protect:small", "protect:new"))
  expect_error(elsar_sub_actions_from_table(protect), "min_new_km2")
  protect$params <- "min_new_km2=five"
  expect_error(elsar_sub_actions_from_table(protect), "name=number")
  protect$params <- "min_new_km2=5; expansion_km=1"
  r <- elsar_sub_actions_from_table(protect)
  expect_equal(r$label, c("E", "X", "S", "N"))
  expect_equal(r$params[[1]]$expansion_km, 1)
})

gg <- function(...) elsar_plot_representation(..., interactive = FALSE)

wide_rep <- function() {
  data.frame(
    feature = c("Forest", "Wetlands", "Carbon"),
    theme = c("Biodiversity", "Biodiversity", "Climate"),
    overall = c(60, 50, 40),
    protect = c(40, 20, 30),
    `reduce-pressures` = c(10, 10, 5),
    restore = c(10, 20, 5),
    check.names = FALSE
  )
}

test_that("wide ELSA output plots with scheme labels and colours", {
  p <- gg(wide_rep())
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_setequal(levels(d$action), c("Protect", "Manage", "Restore"))
  expect_equal(levels(d$action), c("Protect", "Manage", "Restore"))
  expect_equal(nrow(d), 9)
  # overall column ignored, not drawn as an action
  expect_false("overall" %in% d$action)
  b <- ggplot2::ggplot_build(p)
  expect_true(all(c("#6CCF00", "#E79000", "#2A5AFF") %in% b$data[[1]]$fill))
})

test_that("features sort weakest first within each theme", {
  d <- gg(wide_rep())$data
  # horizontal: first feature on top = last level
  expect_equal(rev(levels(d$feature)), c("Wetlands", "Forest", "Carbon"))
  d <- gg(wide_rep(), sort = "strongest", orientation = "vertical")$data
  expect_equal(levels(d$feature), c("Forest", "Wetlands", "Carbon"))
  d <- gg(wide_rep(), sort = "input", orientation = "vertical")$data
  expect_equal(levels(d$feature), c("Forest", "Wetlands", "Carbon"))
  expect_equal(levels(d$theme), c("Biodiversity", "Climate"))
})

test_that("prioritizr summaries are read as proportions, overall rows dropped", {
  pz <- data.frame(
    summary = rep(c("overall", "protect", "restore"), each = 2),
    feature = rep(c("a", "b"), 3),
    relative_held = c(0.5, 0.6, 0.3, 0.4, 0.2, 0.2)
  )
  d <- gg(pz)$data
  expect_equal(nrow(d), 4)
  expect_equal(sort(d$pct), c(20, 20, 30, 40))
  expect_true(all(is.na(d$theme)))
})

test_that("long input, chosen actions, unknown actions and colour overrides", {
  long <- data.frame(
    feature = rep(c("a", "b"), 3),
    action = rep(c("Protect", "Agroforestry", "Restore"), each = 2),
    pct = c(10, 20, 5, 5, NA, 3)
  )
  p <- gg(long, actions = c("Agroforestry", "Protect"), colours = c(Agroforestry = "#A6761D"))
  d <- p$data
  expect_equal(levels(d$action), c("Agroforestry", "Protect"))
  b <- ggplot2::ggplot_build(p)
  expect_true("#A6761D" %in% b$data[[1]]$fill)

  d <- gg(long)$data
  expect_equal(sum(d$pct), 43) # NA drawn as 0
  expect_equal(levels(d$action), c("Protect", "Restore", "Agroforestry"))
})

test_that("themes argument and allocated line", {
  r <- wide_rep()[, -2]
  p <- gg(r, themes = c(Forest = "Bio", Wetlands = "Bio"), allocated = 30)
  expect_equal(levels(p$data$theme), c("Bio", "—"))
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomVline" %in% layers)
  pv <- gg(r, allocated = 30, orientation = "vertical")
  expect_true("GeomHline" %in% vapply(pv$layers, function(l) class(l$geom)[1], character(1)))
})

test_that("labels follow the elsar.language option", {
  withr::local_options(elsar.language = "es")
  p <- gg(wide_rep())
  expect_equal(levels(p$data$action), c("Protección", "Manejo", "Restauración"))
  expect_equal(p$labels$x, "Representación")
})

test_that("bad input is rejected", {
  expect_error(gg(data.frame(x = 1)), "feature")
  expect_error(gg(data.frame(feature = "a", note = "x")), "numeric action")
  expect_error(gg(wide_rep(), actions = "nope"), "not in")
  expect_error(gg(wide_rep(), allocated = "30"), "allocated")
  expect_error(gg(wide_rep(), themes = "x"), "themes")
  dup <- data.frame(feature = "a", protect = 1, Protect = 2)
  expect_error(gg(dup), "same scheme action")
})

test_that("exports with paired measure columns and translated headers are read", {
  ex <- data.frame(
    Datos = c("Bosque", "Carbono"), Tema = c("Biodiversidad", "Clima"),
    `Agregar (spatial)` = c(67, 50), `Agregar (effective)` = c(63, 45),
    `Protección (spatial)` = c(45.8, 30), `Protección (effective)` = c(45.8, 30),
    `Manejo (spatial)` = c(6.3, 10), `Manejo (effective)` = c(9.5, 5),
    check.names = FALSE
  )
  d <- gg(ex)$data
  expect_equal(levels(d$action), c("Protect", "Manage"))
  expect_equal(d$pct[d$feature == "Bosque" & d$action == "Manage"], 9.5)
  expect_equal(levels(d$theme), c("Biodiversidad", "Clima"))
  d <- gg(ex, measure = "spatial")$data
  expect_equal(d$pct[d$feature == "Bosque" & d$action == "Manage"], 6.3)

  # national app: suffixes in the deployment language
  names(ex) <- sub("\\(effective\\)", "(efectiva)", sub("\\(spatial\\)", "(espacial)", names(ex)))
  d <- gg(ex, measure = "spatial")$data
  expect_equal(d$pct[d$feature == "Bosque" & d$action == "Manage"], 6.3)
})

test_that("existing protected areas split out of Protect", {
  r <- wide_rep()
  pa <- c(Forest = 25, Wetlands = 5, Carbon = 10)
  d <- gg(r, existing = pa)$data
  expect_equal(levels(d$action), c("Existing PAs", "New protection (ELSA)", "Manage", "Restore"))
  forest <- d[d$feature == "Forest", ]
  expect_equal(forest$pct[forest$action == "Existing PAs"], 25)
  expect_equal(forest$pct[forest$action == "New protection (ELSA)"], 15)
  # totals unchanged
  expect_equal(sum(d$pct), sum(r$protect + r$`reduce-pressures` + r$restore))

  # a data frame, or FALSE for no split
  d2 <- gg(r, existing = data.frame(feature = names(pa), pct = pa))$data
  expect_equal(d2$pct, d$pct)
  expect_false("Existing PAs" %in% gg(r, existing = FALSE)$data$action)

  # coverage above Protect is capped, with a warning
  expect_warning(d3 <- gg(r, existing = c(Forest = 50, Wetlands = 0, Carbon = 0))$data, "capped")
  expect_equal(d3$pct[d3$feature == "Forest" & d3$action == "Existing PAs"], 40)
  expect_warning(gg(r, existing = c(Nope = 5)), "No feature names match")
})

test_that("the national app's lock-in column is read as existing protection", {
  r <- data.frame(Datos = c("Bosque", "Carbono"), Tema = c("Bio", "Clima"), Agregar = c(60, 40),
                  p = c(40, 30), lock = c("(12.5)", "(3)"), Manejo = c(10, 5))
  names(r)[4:5] <- c("Protecci\u00f3n", "Protecci\u00f3n actual")
  d <- gg(r)$data
  expect_equal(d$pct[d$feature == "Bosque" & d$action == "Existing PAs"], 12.5)
  expect_equal(d$pct[d$feature == "Bosque" & d$action == "New protection (ELSA)"], 27.5)
  expect_false(any(grepl("actual", levels(d$action))))
  # in long form too
  long <- data.frame(feature = c("a", "a"), action = c("Protect", "Current Protection"), pct = c(30, 10))
  expect_equal(sort(gg(long)$data$pct), c(10, 20))
})

test_that("existing coverage can come from elsar_summarise_overlap()", {
  ov <- data.frame(
    region = "National", layer = rep(c("Forest", "Wetlands", "Carbon"), each = 2),
    action = "x", amount = c(60, 40, 10, 10, 5, 5), amount_in_pa = c(20, 0, 2, 0, 0, 0)
  )
  d <- gg(wide_rep(), existing = ov)$data
  expect_equal(d$pct[d$feature == "Forest" & d$action == "Existing PAs"], 20)
  expect_equal(d$pct[d$feature == "Wetlands" & d$action == "Existing PAs"], 10)
  ov$amount_in_pa <- NA
  expect_error(gg(wide_rep(), existing = ov), "no protected-area split")
})

test_that("the default is an interactive plotly chart, a boxed panel per theme", {
  skip_if_not_installed("plotly")
  w <- elsar_plot_representation(wide_rep(), allocated = 30, existing = c(Forest = 10, Wetlands = 0, Carbon = 0))
  expect_s3_class(w, "plotly")
  b <- plotly::plotly_build(w)$x
  bars <- Filter(function(t) t$type == "bar", b$data)
  first <- Filter(function(t) t$yaxis == "y", bars)
  expect_equal(vapply(first, `[[`, "", "name"),
               c("Existing PAs", "New protection (ELSA)", "Manage", "Restore"))
  expect_equal(vapply(first, function(t) t$marker$color, ""), c("#005000", "#6CCF00", "#E79000", "#2A5AFF"))
  expect_false(b$layout$showlegend)
  # two themes: two panels, each trace drawn once per panel
  expect_length(bars, 8)
  expect_setequal(vapply(bars, `[[`, "", "yaxis"), c("y", "y2"))
  expect_equal(b$layout$barmode, "stack")
  # Biodiversity on top, weakest feature first
  expect_equal(b$layout$yaxis$categoryarray, c("Forest", "Wetlands"))
  expect_gt(b$layout$yaxis$domain[1], b$layout$yaxis2$domain[2])
  texts <- vapply(b$layout$annotations, `[[`, "", "text")
  expect_equal(texts[1:2], c("<b>Biodiversity</b>", "<b>Climate</b>"))
  # legend entries, spread by the widget's script
  legend <- Filter(function(a) grepl("^elsar-legend-", a$name %||% ""), b$layout$annotations)
  expect_length(legend, 5)
  expect_match(legend[[1]]$text, "#005000.*Existing PAs")
  expect_match(legend[[5]]$text, "Area allocated \\(30.0%\\)")
  expect_true(all(diff(vapply(legend, `[[`, 0, "x")) > 0))
  expect_length(w$jsHooks$render, 1)
  # a box and an allocated line per panel, and a legend entry for the line
  types <- vapply(b$layout$shapes, `[[`, "", "type")
  expect_equal(sum(types == "rect"), 2)
  expect_equal(sum(types == "line"), 2)
  forest <- Filter(function(t) t$name == "Manage" && t$yaxis == "y", bars)[[1]]
  expect_match(forest$hovertext[forest$y == "Forest"], "Forest.*Manage: 10.0%.*Total: 60.0%")
  expect_equal(b$config$toImageButtonOptions$format, "png")
  expect_match(b$layout$font$family, "Roboto")
})

test_that("without themes the plotly chart has a single panel", {
  skip_if_not_installed("plotly")
  b <- plotly::plotly_build(elsar_plot_representation(wide_rep()[, -2], orientation = "vertical"))$x
  expect_equal(b$layout$xaxis$categoryarray, c("Carbon", "Wetlands", "Forest"))
  expect_equal(b$data[[1]]$orientation, "v")
  expect_false(any(grepl("<b>", vapply(b$layout$annotations, `[[`, "", "text"))))
})

# A tiny ELSA run on disk: two features, a lock-in layer and a config
fake_run <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:3857")
  terra::writeRaster(terra::setValues(r, c(1, 1, 1, 1)), file.path(dir, "forest.tif"))
  terra::writeRaster(terra::setValues(r, c(3, 1, 0, 0)), file.path(dir, "carbon.tif"))
  dir.create(file.path(dir, "pa"))
  terra::writeRaster(terra::setValues(r, c(1, 0, NA, 0.5)), file.path(dir, "pa", "wdpa.tif"))
  cfg <- list(
    actions = list(
      list(slug = "protect", name = "Protect", budget = 30, lockin = list(fileName = "wdpa.tif")),
      list(slug = "restore", name = "Restore", budget = 5)
    ),
    layers = list(
      list(name = "Forest", fileName = "forest.tif", actionImpacts = list(protect = 1, restore = 1)),
      list(name = "Carbon", fileName = "carbon.tif", actionImpacts = list(protect = 0.5, restore = 1))
    )
  )
  jsonlite::write_json(cfg, file.path(dir, "config.json"), auto_unbox = TRUE)
  file.path(dir, "config.json")
}

test_that("elsar_existing_protection() reads the run's layers and lock-in", {
  path <- fake_run()
  sp <- elsar_existing_protection(path, measure = "spatial")
  # Forest: (1 + 0 + 0 + 0.5) / 4; Carbon: (3 * 1) / 4
  expect_equal(sp, c(Forest = 37.5, Carbon = 75))
  # effective: times Protect's impact
  expect_equal(elsar_existing_protection(path), c(Forest = 37.5, Carbon = 37.5))
  expect_error(elsar_existing_protection(path, action = "manage"), "No action 'manage'")
  cfg <- jsonlite::read_json(path)
  cfg$actions[[1]]$lockin <- NULL
  expect_error(elsar_existing_protection(cfg, dir = dirname(path)), "no lock-in layer")
  cfg <- jsonlite::read_json(path)
  cfg$layers[[1]]$fileName <- "nope.tif"
  expect_error(elsar_existing_protection(cfg, dir = dirname(path)), "not found.*nope.tif.*layers_dir")
})

test_that("config fills in existing protection and the allocated share", {
  path <- fake_run()
  r <- data.frame(feature = c("Forest", "Carbon"), protect = c(50, 40), restore = c(10, 5))
  p <- gg(r, config = path)
  d <- p$data
  expect_equal(d$pct[d$feature == "Forest" & d$action == "Existing PAs"], 37.5)
  expect_equal(d$pct[d$feature == "Carbon" & d$action == "Existing PAs"], 37.5)
  lines <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_equal(lines[[1]]$data$xintercept, 35)
  # explicit arguments win
  d <- gg(r, config = path, existing = FALSE)$data
  expect_false("Existing PAs" %in% d$action)
})

test_that("platform deployment configs: default budgets and lock-in options", {
  path <- fake_run()
  cfg <- jsonlite::read_json(path)
  dep <- cfg
  dep$actions[[1]]$lockin <- NULL
  dep$actions[[1]]$budget <- NULL
  dep$actions[[1]]$defaultBudget <- 30
  dep$actions[[1]]$lockinOptions <- list(
    list(slug = "no-lockin"),
    list(slug = "protect-lockin-zone-1", fileName = "wdpa.tif")
  )
  dep$actions[[1]]$defaultLockin <- "protect-lockin-zone-1"
  dir <- dirname(path)
  expect_equal(elsar_existing_protection(dep, dir = dir, measure = "spatial"),
               c(Forest = 37.5, Carbon = 75))
  expect_error(elsar_existing_protection(dep, dir = dir, lockin = "nope"), "not one of the options")
  expect_error(elsar_existing_protection(dep, dir = dir, lockin = "no-lockin"), "no lock-in layer")
  expect_equal(.config_allocated(dep), 35)
})

test_that("an elsar_summarise_overlap() table plots with existing PAs split out", {
  ex <- elsar_example_results()
  x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
  crs <- terra::crs(x)
  layers <- list(
    Wetland = sf::st_sf(geometry = sf::st_buffer(sf::st_sfc(sf::st_point(c(65000, 15000)), crs = crs), 8000)),
    KBA = sf::st_sf(geometry = sf::st_buffer(sf::st_sfc(sf::st_point(c(38000, 40000)), crs = crs), 9000))
  )
  ov <- elsar_summarise_overlap(x, layers)
  d <- gg(ov)$data
  expect_true("Existing PAs" %in% levels(d$action))
  expect_false("Do Nothing" %in% levels(d$action))
  # bars equal the share of each layer under any action
  acted <- tapply(ov$pct_of_layer[ov$is_action], as.character(ov$layer[ov$is_action]), sum)
  drawn <- tapply(d$pct, as.character(d$feature), sum)
  expect_equal(drawn[names(acted)], acted, tolerance = 1e-8)
  # existing = layer amount in PAs within Protect
  pr <- ov[ov$action == "Protect", ]
  tot <- tapply(ov$amount, as.character(ov$layer), sum)
  kba <- d$pct[d$feature == "KBA" & d$action == "Existing PAs"]
  expect_equal(kba, 100 * pr$amount_in_pa[pr$layer == "KBA"] / tot[["KBA"]])
})

test_that("budgets go after the action names in the legend", {
  d <- gg(wide_rep(), budgets = c(protect = 30, manage = 5, restore = 18.56))$data
  expect_equal(levels(d$action), c("Protect (30.0%)", "Manage (5.0%)", "Restore (18.6%)"))
  # with existing PAs split out, Protect's budget labels the rest of Protect
  d <- gg(wide_rep(), budgets = c(protect = 30, restore = 10),
          existing = c(Forest = 10, Wetlands = 0, Carbon = 0))$data
  expect_equal(levels(d$action), c("Existing PAs", "Protect (30.0%)", "Manage", "Restore (10.0%)"))
  # from a config, in the plot's language
  withr::local_options(elsar.language = "es")
  path <- fake_run()
  r <- data.frame(feature = c("Forest", "Carbon"), protect = c(50, 40), restore = c(10, 5))
  d <- gg(r, config = path)$data
  expect_equal(levels(d$action)[2:3], c("Protección (30,0%)", "Restauración (5,0%)"))
  skip_if_not_installed("plotly")
  b <- plotly::plotly_build(elsar_plot_representation(r, config = path))$x
  expect_true("Protección (30,0%)" %in% vapply(b$data, function(t) t$name %||% "", ""))
  expect_true(any(grepl("Protección \\(30,0%\\)", vapply(b$layout$annotations, `[[`, "", "text"))))
})

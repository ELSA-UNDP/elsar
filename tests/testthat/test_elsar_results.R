# Post-analysis of ELSA results: action scheme, preparation, summaries and the
# protection gap. A 4 x 4 grid of 1 km cells in an equal-area CRS makes every
# expected area exact (1 cell = 100 ha).

ea_crs <- "ESRI:54009"

# Columns 1-2 form region "West", columns 3-4 region "East".
#   1 1 2 2
#   1 3 2 5
#   5 5 4 5
#   5 NA 5 5
toy_actions <- function() {
  terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4000, ymin = 0, ymax = 4000,
    crs = ea_crs, vals = c(1, 1, 2, 2, 1, 3, 2, 5, 5, 5, 4, 5, 5, NA, 5, 5)
  )
}

# Protected area covering the top-left 2 x 2 block (cells valued 1, 1, 1, 3)
toy_pas <- function() {
  r <- toy_actions()
  terra::values(r) <- c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  r
}

toy_regions <- function() {
  sq <- function(x0, x1) {
    sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 4000), c(x0, 4000), c(x0, 0))))
  }
  sf::st_sf(name = c("West", "East"), geometry = sf::st_sfc(sq(0, 2000), sq(2000, 4000), crs = ea_crs))
}

area_of <- function(s, region, action, col = "area") {
  s[[col]][s$region == region & s$action == action]
}

# Scheme ------------------------------------------------------------------------

test_that("elsar_action_scheme() returns the default ELSA scheme", {
  s <- elsar_action_scheme()
  expect_s3_class(s, "tbl_df")
  expect_equal(s$value, 1:5)
  expect_equal(s$is_action, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(s$group, s$label)
})

test_that("elsar_action_scheme() validates its inputs", {
  expect_error(elsar_action_scheme(values = 1:3), "same length")
  expect_error(elsar_action_scheme(values = c(1, 1, 2, 3, 4)), "duplicates")
  expect_error(elsar_action_scheme(no_action = "Nothing"), "no_action")
  expect_error(
    elsar_action_scheme(colours = c("red", "blue", "notacolour", "green", "grey")),
    "notacolour"
  )
})

# Preparation ---------------------------------------------------------------------

test_that("elsar_prepare_actions() attaches the scheme and PA layer", {
  x <- elsar_prepare_actions(toy_actions(), pas = toy_pas())
  expect_equal(names(x), c("action", "pa_fraction"))
  cats <- terra::cats(x[["action"]])[[1]]
  expect_true(all(c("label", "colour", "group", "is_action") %in% names(cats)))
  # pa_fraction is masked to the action raster
  expect_true(is.na(terra::values(x[["pa_fraction"]])[14]))
})

test_that("elsar_prepare_actions() sets unknown values to NA with a warning", {
  r <- toy_actions()
  r[1] <- 9
  expect_warning(x <- elsar_prepare_actions(r), "9")
  expect_true(is.na(terra::values(x)[1]))
})

test_that("PA polygons give the same cover as an equivalent raster", {
  pa_poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 2000), c(2000, 2000), c(2000, 4000), c(0, 4000), c(0, 2000)))),
    crs = ea_crs
  ))
  from_poly <- elsar_prepare_actions(toy_actions(), pas = pa_poly)
  from_rast <- elsar_prepare_actions(toy_actions(), pas = toy_pas())
  expect_equal(
    terra::values(from_poly[["pa_fraction"]]),
    terra::values(from_rast[["pa_fraction"]])
  )
})

test_that("an empty PA layer gives zero protection", {
  empty <- sf::st_sf(geometry = sf::st_sfc(crs = ea_crs))
  x <- elsar_prepare_actions(toy_actions(), pas = empty)
  s <- suppressMessages(elsar_summarise_actions(x))
  expect_equal(sum(s$area_in_pa), 0)
})

test_that("PA rasters outside 0-1 are rejected", {
  pa <- toy_pas() * 100
  expect_error(elsar_prepare_actions(toy_actions(), pas = pa), "between 0 and 1")
})

test_that("pa_threshold binarises protected-area cover", {
  pa <- toy_pas() * 0.4
  x_frac <- elsar_prepare_actions(toy_actions(), pas = pa)
  x_bin <- elsar_prepare_actions(toy_actions(), pas = pa, pa_threshold = 0.5)
  expect_equal(sum(elsar_summarise_actions(x_frac)$area_in_pa), 4 * 100 * 0.4, tolerance = 1e-3)
  expect_equal(sum(elsar_summarise_actions(x_bin)$area_in_pa), 0)
})

# Summaries ---------------------------------------------------------------------

test_that("national summary gives exact areas and percentages", {
  x <- elsar_prepare_actions(toy_actions(), pas = toy_pas())
  s <- elsar_summarise_actions(x)
  # 15 valid cells; Protect = 3, Restore = 3, Manage = 1, Urban = 1, Do Nothing = 7
  expect_equal(as.character(s$action), elsar_action_scheme()$label)
  expect_equal(s$area, c(300, 300, 100, 100, 700), tolerance = 1e-3)
  expect_equal(s$pct_of_region, 100 * c(3, 3, 1, 1, 7) / 15, tolerance = 1e-3)
  expect_equal(s$area_in_pa, c(300, 0, 100, 0, 0), tolerance = 1e-3)
  expect_equal(s$area_in_pa + s$area_outside_pa, s$area)
})

test_that("units are applied", {
  x <- elsar_prepare_actions(toy_actions())
  expect_equal(sum(elsar_summarise_actions(x, units = "km2")$area), 15, tolerance = 1e-3)
  expect_equal(sum(elsar_summarise_actions(x, units = "m2")$area), 15e6, tolerance = 1e-3)
})

test_that("regional summary splits areas between regions", {
  x <- elsar_prepare_actions(toy_actions(), pas = toy_pas())
  s <- elsar_summarise_actions(x, toy_regions(), region_col = "name")
  expect_equal(levels(s$region), c("West", "East"))
  expect_equal(area_of(s, "West", "Protect"), 300, tolerance = 1e-3)
  expect_equal(area_of(s, "East", "Restore"), 300, tolerance = 1e-3)
  expect_equal(area_of(s, "West", "Urban Greening"), 0)
  # East: Restore 3 of 8 valid cells
  expect_equal(area_of(s, "East", "Restore", "pct_of_region"), 100 * 3 / 8, tolerance = 1e-3)
  # pct_of_action sums to 100 for every action present
  totals <- tapply(s$pct_of_action, s$action, sum)
  expect_equal(as.vector(totals), rep(100, 5), tolerance = 1e-6)
  # Regions tile the grid, so they add up to the national total
  expect_equal(sum(s$area), sum(elsar_summarise_actions(x)$area), tolerance = 1e-6)
})

test_that("rows sharing a region name are combined, with a message", {
  regions <- toy_regions()
  regions$name <- "Whole"
  x <- elsar_prepare_actions(toy_actions())
  expect_message(s <- elsar_summarise_actions(x, regions, region_col = "name"), "combined")
  expect_equal(nrow(s), 5)
  expect_equal(sum(s$area), 1500, tolerance = 1e-3)
})

test_that("a region outside the raster gets zero area, not an error", {
  far <- sf::st_sf(name = "Far", geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(9e5, 9e5), c(9.1e5, 9e5), c(9.1e5, 9.1e5), c(9e5, 9.1e5), c(9e5, 9e5)))),
    crs = ea_crs
  ))
  regions <- rbind(toy_regions(), far)
  x <- elsar_prepare_actions(toy_actions())
  s <- elsar_summarise_actions(x, regions, region_col = "name")
  expect_true(all(s$area[s$region == "Far"] == 0))
  expect_true(all(s$pct_of_region[s$region == "Far"] == 0))
})

test_that("regions in another CRS are transformed", {
  x <- elsar_prepare_actions(toy_actions())
  s_ref <- elsar_summarise_actions(x, toy_regions(), region_col = "name")
  s_ll <- elsar_summarise_actions(x, sf::st_transform(toy_regions(), 4326), region_col = "name")
  expect_equal(s_ll$area, s_ref$area, tolerance = 1e-3)
})

test_that("areas are true ground areas for lon/lat rasters", {
  # 1 x 1 degree cells at the equator are ~1.23e6 ha; a planar res^2 would give 1e-4 ha
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = -1, ymax = 1,
                   crs = "EPSG:4326", vals = c(1, 2, 3, 5))
  s <- elsar_summarise_actions(elsar_prepare_actions(r))
  expect_equal(sum(s$area), 4 * 1.2308e6, tolerance = 1e-3)
})

test_that("no PA layer gives NA for the PA split", {
  s <- elsar_summarise_actions(elsar_prepare_actions(toy_actions()))
  expect_true(all(is.na(s$area_in_pa)))
  expect_false(attr(s, "has_pa"))
})

test_that("a custom scheme carries through to the summary", {
  scheme <- elsar_action_scheme(
    labels = c("Conserve", "Rehabilitate", "Sustain", "Green", "None"),
    no_action = "None"
  )
  s <- elsar_summarise_actions(elsar_prepare_actions(toy_actions(), scheme))
  expect_equal(levels(s$action), scheme$label)
  expect_equal(s$is_action, scheme$is_action)
})

test_that("unprepared rasters are rejected", {
  expect_error(elsar_summarise_actions(toy_actions()), "elsar_prepare_actions")
})

# Protection gap ----------------------------------------------------------------

test_that("protection gap separates current and new protection", {
  x <- elsar_prepare_actions(toy_actions(), pas = toy_pas())
  g <- elsar_protection_gap(elsar_summarise_actions(x), target = 0.5)
  # Current: 4 PA cells (3 Protect + 1 Manage, i.e. not locked in) = 400 ha.
  # New: Protect outside PAs = 0 ha.
  expect_equal(g$current, 400, tolerance = 1e-3)
  expect_equal(g$new, 0, tolerance = 1e-3)
  expect_equal(g$pct_total, 100 * 4 / 15, tolerance = 1e-3)
  expect_equal(g$gap, 0.5 * 1500 - 400, tolerance = 1e-3)
  expect_false(g$target_met)
})

test_that("protection gap counts Protect outside PAs as new", {
  pa <- toy_pas()
  pa[2] <- 0 # one Protect cell is no longer in a PA
  x <- elsar_prepare_actions(toy_actions(), pas = pa)
  s <- elsar_summarise_actions(x, toy_regions(), region_col = "name")
  g <- elsar_protection_gap(s, target = 0.3)
  west <- g[g$region == "West", ]
  expect_equal(west$current, 300, tolerance = 1e-3)
  expect_equal(west$new, 100, tolerance = 1e-3)
  expect_true(west$target_met)
  # National row totals the regions
  expect_equal(as.character(g$region), c("West", "East", "National"))
  expect_equal(g$total[3], sum(g$total[1:2]))
})

test_that("protection gap needs PA information and known actions", {
  s <- elsar_summarise_actions(elsar_prepare_actions(toy_actions()))
  expect_error(elsar_protection_gap(s), "protected-area")
  s_pa <- elsar_summarise_actions(elsar_prepare_actions(toy_actions(), pas = toy_pas()))
  expect_error(elsar_protection_gap(s_pa, protect_actions = "Conserve"), "Conserve")
  g <- elsar_protection_gap(s_pa, target = NULL)
  expect_false("gap" %in% names(g))
})

test_that("example results load and summarise", {
  ex <- elsar_example_results()
  expect_s4_class(ex$actions, "SpatRaster")
  expect_s3_class(ex$regions, "sf")
  x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
  s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
  expect_equal(nlevels(s$region), 3)
})

# Plots and export ----------------------------------------------------------------

example_inputs <- function() {
  ex <- elsar_example_results()
  x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
  s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
  list(ex = ex, x = x, s = s, g = elsar_protection_gap(s))
}

builds <- function(p) {
  expect_no_error(suppressMessages(ggplot2::ggplot_build(p)))
}

test_that("action map builds, with and without PAs and regions", {
  d <- example_inputs()
  builds(elsar_plot_actions(d$x, regions = d$ex$regions))
  builds(elsar_plot_actions(elsar_prepare_actions(d$ex$actions), show_no_action = FALSE))
  builds(elsar_plot_actions(d$x, legend = FALSE))
})

test_that("bar charts use the scheme colours and the PA segment", {
  d <- example_inputs()
  p <- elsar_plot_action_bars(d$s)
  built <- ggplot2::ggplot_build(p)
  fills <- unique(built$data[[1]]$fill)
  expect_true(all(c("#005000", "#06C723", "#8803FC") %in% fills))
  # Segments of each region add up to the action share of the region
  # (vertical bars: Westmoor is the first category on the x axis)
  bars <- built$data[[1]]
  expect_equal(
    sum(bars$ymax[bars$x == 1] - bars$ymin[bars$x == 1]),
    sum(d$s$pct_of_region[d$s$region == "Westmoor" & d$s$is_action]),
    tolerance = 1e-6
  )
  # Horizontal bars: Westmoor is drawn at the top (last y position)
  h <- ggplot2::ggplot_build(elsar_plot_action_bars(d$s, orientation = "horizontal"))$data[[1]]
  expect_equal(
    sum(h$xmax[h$y == 3] - h$xmin[h$y == 3]),
    sum(d$s$pct_of_region[d$s$region == "Westmoor" & d$s$is_action]),
    tolerance = 1e-6
  )
  builds(elsar_plot_action_bars(d$s, metric = "area", by = "action"))
  builds(elsar_plot_action_bars(d$s, split_pa = FALSE))
})

test_that("bar charts warn when asked to split PAs without PA data", {
  s <- elsar_summarise_actions(elsar_prepare_actions(toy_actions()))
  expect_warning(p <- elsar_plot_action_bars(s, split_pa = TRUE), "split_pa")
  builds(p)
})

test_that("protection gap plot sorts regions and keeps national last", {
  d <- example_inputs()
  p <- elsar_plot_protection_gap(d$g)
  lv <- levels(p$data$region)
  regions <- d$g[d$g$region != "National", ]
  expect_equal(lv, c(as.character(regions$region[order(-regions$pct_total)]), "National"))
  expect_equal(levels(elsar_plot_protection_gap(d$g, sort = "none")$data$region),
               as.character(d$g$region))
  # Horizontal bars run top to bottom, so the levels are reversed
  expect_equal(levels(elsar_plot_protection_gap(d$g, orientation = "horizontal")$data$region), rev(lv))
})

test_that("protection gap plot builds, with and without a target", {
  d <- example_inputs()
  builds(elsar_plot_protection_gap(d$g))
  builds(elsar_plot_protection_gap(d$g, orientation = "horizontal"))
  builds(elsar_plot_protection_gap(d$g, metric = "area"))
  s_no_target <- elsar_protection_gap(d$s, target = NULL)
  builds(elsar_plot_protection_gap(s_no_target))
  expect_error(elsar_plot_protection_gap(d$s), "elsar_protection_gap")
})

test_that("region panels are made for every region", {
  d <- example_inputs()
  plots <- elsar_plot_regions(d$x, d$s, d$ex$regions, "region_name", metrics = "percent")
  expect_named(plots, levels(d$s$region))
  expect_s3_class(plots[[1]], "patchwork")
  expect_error(
    elsar_plot_region(d$x, d$s, d$ex$regions, "region_name", "Atlantis"),
    "Atlantis"
  )
})

test_that("elsar_export_results() writes summaries, plots and region panels", {
  d <- example_inputs()
  dir <- withr::local_tempdir()
  out <- suppressMessages(elsar_export_results(
    d$ex$actions, dir, regions = d$ex$regions, region_col = "region_name",
    pas = d$ex$pas, dpi = 30
  ))
  expect_true(all(file.exists(out$files)))
  expect_setequal(
    basename(out$files),
    c("elsa_actions_national.csv", "elsa_actions_by_region.csv", "elsa_protection_gap.csv",
      "elsa_map.png", "elsa_bars.png", "elsa_protection_gap.png",
      "elsa_Westmoor.png", "elsa_Central_Highlands.png", "elsa_Eastern_Delta.png")
  )
  csv <- utils::read.csv(file.path(dir, "elsa_protection_gap.csv"))
  expect_equal(csv$total, out$gap$total, tolerance = 1e-6)
})

test_that("elsar_export_results() works without regions or PAs", {
  dir <- withr::local_tempdir()
  out <- suppressMessages(elsar_export_results(toy_actions(), dir, dpi = 30))
  expect_null(out$gap)
  expect_setequal(basename(out$files),
                  c("elsa_actions_national.csv", "elsa_map.png", "elsa_bars.png"))
})

test_that("plot file names are safe and unique", {
  expect_equal(.safe_name(c("São Tomé", "A/B", "")), c("Sao_Tome", "A_B", "region"))
})

# Overlap -------------------------------------------------------------------------

test_that("overlap gives exact amounts for polygons and rasters", {
  x <- elsar_prepare_actions(toy_actions(), pas = toy_pas())
  # Polygon over the left half: columns 1-2 (actions 1,1 / 1,3 / 5,5 / 5,NA)
  left <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(2000, 0), c(2000, 4000), c(0, 4000), c(0, 0)))),
    crs = ea_crs
  ))
  ov <- elsar_summarise_overlap(x, list(Left = left, PA = toy_pas()))
  left_ov <- ov[ov$layer == "Left", ]
  expect_equal(left_ov$amount, c(300, 0, 100, 0, 300), tolerance = 1e-3)
  # The NA cell is outside the planning area, so 7 cells make 100%
  expect_equal(left_ov$pct_of_layer, 100 * c(3, 0, 1, 0, 3) / 7, tolerance = 1e-3)
  expect_equal(left_ov$amount_in_pa + left_ov$amount_outside_pa, left_ov$amount)
  # The PA layer overlaps itself completely: amount in PA = amount
  pa_ov <- ov[ov$layer == "PA", ]
  expect_equal(pa_ov$amount_in_pa, pa_ov$amount)
  expect_equal(sum(pa_ov$amount), 400, tolerance = 1e-3)
})

test_that("overlap works by region, with sums and multi-layer rasters", {
  x <- elsar_prepare_actions(toy_actions())
  counts <- toy_actions() * 0 + 2
  names(counts) <- "people"
  stack <- c(counts, toy_pas())
  names(stack) <- c("people", "pa")
  ov <- elsar_summarise_overlap(x, stack, toy_regions(), "name", measure = "sum")
  expect_equal(levels(ov$layer), c("people", "pa"))
  expect_equal(sum(ov$amount[ov$layer == "people"]), 2 * 15)
  expect_equal(sum(ov$amount[ov$layer == "people" & ov$region == "East"]), 2 * 8)
  expect_true(all(is.na(ov$amount_in_pa)))
  expect_true(is.na(attr(ov, "units")))
})

test_that("overlap validates layers", {
  x <- elsar_prepare_actions(toy_actions())
  expect_error(elsar_summarise_overlap(x, list(toy_pas())), "named")
  expect_error(elsar_summarise_overlap(x, list(a = 1)), "SpatRaster, sf or SpatVector")
  neg <- toy_pas() - 1
  expect_warning(elsar_summarise_overlap(x, list(neg = neg)), "negative")
})

test_that("overlap plot builds and bars reach 100%", {
  x <- elsar_prepare_actions(toy_actions())
  ov <- elsar_summarise_overlap(x, list(PA = toy_pas()), toy_regions(), "name")
  p <- elsar_plot_overlap(ov)
  built <- ggplot2::ggplot_build(p)
  ends <- tapply(built$data[[1]]$ymax, list(built$data[[1]]$PANEL, built$data[[1]]$x), max)
  # West holds the whole PA; East has none (bar of 0)
  expect_equal(max(ends, na.rm = TRUE), 100, tolerance = 1e-6)
  builds(elsar_plot_overlap(ov, show_no_action = FALSE))
  builds(elsar_plot_overlap(ov, orientation = "horizontal"))
})

# Report --------------------------------------------------------------------------

test_that("the results report renders", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  d <- example_inputs()
  out <- withr::local_tempfile(fileext = ".html")
  path <- suppressMessages(elsar_results_report(
    d$x, d$ex$regions, "region_name",
    layers = list(PA = d$ex$pas), output_file = out, open = FALSE
  ))
  expect_true(file.exists(path))
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Regional profiles")
  expect_match(html, "Overlap with other layers")
})

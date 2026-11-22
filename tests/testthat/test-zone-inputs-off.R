# Switching a zone's default inputs off (issue #96): agriculture_threshold =
# FALSE / built_areas_threshold = FALSE leave that input out of the Protect,
# Restore and Manage zones, and no layer (or LULC) is needed for it.

ea <- "ESRI:54009"
grid <- function(vals, n = 4) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * 1000, ymin = 0, ymax = n * 1000,
              crs = ea, vals = vals)
}
pus <- function(n = 4) grid(1, n)
v <- function(r) unname(terra::values(r)[, 1])
pa <- sf::st_sf(geometry = sf::st_sfc(
  sf::st_polygon(list(rbind(c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))), crs = ea))
ag <- c(1, 0, 0, rep(0, 13))   # cell 1 farmland
bu <- c(0, 1, 0, rep(0, 13))   # cell 2 built-up

test_that("a FALSE threshold is the only non-number accepted", {
  expect_true(.zone_input_off(FALSE, "agriculture_threshold"))
  expect_false(.zone_input_off(0.1, "agriculture_threshold"))
  expect_error(.zone_input_off(NA, "agriculture_threshold"), "or FALSE")
  expect_error(.zone_input_off("off", "built_areas_threshold"), "built_areas_threshold")
})

test_that("Protect: an input switched off excludes nothing and needs no layer", {
  protect <- function(...) suppressWarnings(suppressMessages(make_protect_zone(
    iso3 = "TST", pus = pus(), current_protected_areas = pa, human_pressure = grid(1),
    hii_threshold = 5, filter_patch_size = FALSE, ...)))
  expect_equal(v(protect(agricultural_areas = grid(ag), built_areas = grid(bu)))[1:3], c(0, 0, 1))
  expect_equal(v(protect(agricultural_areas = grid(ag), built_areas = grid(bu),
                         agriculture_threshold = FALSE))[1:3], c(1, 0, 1))
  # no agriculture layer, no LULC, a local product without class values: fine when off
  expect_equal(v(protect(built_areas = grid(bu), agriculture_threshold = FALSE,
                         lulc_product = "local"))[1:3], c(1, 0, 1))
  expect_equal(v(protect(agriculture_threshold = FALSE, built_areas_threshold = FALSE))[1:3], c(1, 1, 1))
  # without it, the missing layer is still an error
  expect_error(protect(built_areas = grid(bu)), "lulc_raster")
})

test_that("Protect: no HII and both inputs off warns that every unit is eligible", {
  expect_warning(z <- suppressMessages(make_protect_zone(
    iso3 = "TST", pus = pus(), current_protected_areas = pa, agriculture_threshold = FALSE,
    built_areas_threshold = FALSE, filter_patch_size = FALSE)), "every planning unit is eligible")
  expect_true(all(v(z) == 1))
})

test_that("Restore: an input switched off excludes nothing and needs no layer", {
  restore <- function(...) suppressMessages(make_restore_zone(
    iso3 = "TST", pus = pus(), degradation = grid(1), human_pressure = grid(2),
    forest_mask = grid(1), filter_patch_size = FALSE, ...))[["restore_zone_v1"]]
  expect_equal(v(restore(agricultural_areas = grid(ag), built_areas = grid(bu)))[1:3], c(0, 0, 1))
  expect_equal(v(restore(built_areas = grid(bu), agriculture_threshold = FALSE))[1:3], c(1, 0, 1))
  expect_equal(v(restore(agricultural_areas = grid(ag), built_areas_threshold = FALSE))[1:3], c(0, 1, 1))
  expect_equal(v(restore(agriculture_threshold = FALSE, built_areas_threshold = FALSE))[1:3], c(1, 1, 1))
  expect_error(restore(), "must be provided")
})

test_that("Manage: agriculture off no longer qualifies a unit; built-up off excludes nothing", {
  manage <- function(pasture = c(0, 0, 1, rep(0, 13)), ...) suppressMessages(make_manage_zone(
    iso3 = "TST", pus = pus(), human_pressure = grid(1:16), managed_forests = grid(0),
    pasturelands = grid(pasture), filter_patch_size = FALSE, ...))
  base <- manage(agricultural_areas = grid(ag), built_areas = grid(bu))
  expect_equal(v(base[["manage_zone_v2"]])[1:3], c(1, 0, 1))
  off <- manage(built_areas = grid(bu), agriculture_threshold = FALSE)
  expect_equal(v(off[["manage_zone_v2"]])[1:3], c(0, 0, 1))      # pasture only
  # v1 is as if there were no farmland at all
  no_ag <- manage(agricultural_areas = grid(0), built_areas = grid(bu))
  expect_equal(v(off[["manage_zone_v1"]]), v(no_ag[["manage_zone_v1"]]))
  # built-up off: the built-up pasture cell stays in
  bp <- manage(pasture = c(0, 1, 0, rep(0, 13)), agricultural_areas = grid(ag),
               built_areas_threshold = FALSE)
  expect_equal(v(bp[["manage_zone_v2"]])[1:2], c(1, 1))
})

test_that("inputs switched off are not written as intermediate layers", {
  out <- withr::local_tempdir()
  suppressMessages(make_restore_zone(
    iso3 = "TST", pus = pus(), degradation = grid(1), human_pressure = grid(2), forest_mask = grid(1),
    built_areas = grid(bu), agriculture_threshold = FALSE, filter_patch_size = FALSE, output_path = out))
  expect_false(file.exists(file.path(out, "agriculture_areas_TST.tif")))
  expect_true(file.exists(file.path(out, "built_areas_TST.tif")))
})

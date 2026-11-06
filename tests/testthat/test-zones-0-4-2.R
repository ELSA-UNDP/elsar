# Regression tests for the 0.4.2 zone fixes: the Restore band rule (decided
# Nov 2026), the 0/1 degradation handoff (P3), the sieve (audit §2 #7), Manage
# without pasture (F17) and SpatVector protected areas.

ea <- "ESRI:54009"
grid <- function(vals, n = 4) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * 1000, ymin = 0, ymax = n * 1000,
              crs = ea, vals = vals)
}
pus <- function(n = 4) grid(1, n)

restore <- function(hii, degradation, agriculture = 0, built = 0, ...) {
  suppressMessages(make_restore_zone(
    iso3 = "TST", pus = pus(), degradation = grid(degradation), human_pressure = grid(hii),
    forest_mask = grid(1), agricultural_areas = grid(agriculture), built_areas = grid(built),
    filter_patch_size = FALSE, ...
  ))[["restore_zone_v1"]]
}

test_that("the Restore Zone is a band of restorable degraded land", {
  # cells: 1 HII 25 no degradation, 2 HII 10, 3 degraded with HII 2, 4 untouched,
  #        5 HII 10 but farmland, 6 degraded but HII 30, 7-16 untouched
  hii <- c(25, 10, 2, 2, 10, 30, rep(2, 10))
  deg <- c(0, 0, 1, 0, 0, 1, rep(0, 10))   # 0/1 degraded-areas layer
  ag  <- c(0, 0, 0, 0, 1, 0, rep(0, 10))
  v <- terra::values(restore(hii, deg, agriculture = ag))[, 1]
  expect_equal(v[1:6], c(0, 1, 1, 0, 0, 0))
})

test_that("an SDG layer (-1 = degraded) and a 0/1 layer both work (P3)", {
  sdg <- c(-1, 0, 1, rep(0, 13))           # -1 degraded, +1 improving
  v <- terra::values(restore(rep(2, 16), sdg))[, 1]
  expect_equal(v[1:3], c(1, 0, 0))
  binary <- c(1, 0, 0, rep(0, 13))
  v2 <- terra::values(restore(rep(2, 16), binary))[, 1]
  expect_equal(v2[1:3], c(1, 0, 0))
})

test_that("hii_max = Inf removes the upper cap; old argument still works", {
  v <- terra::values(restore(rep(30, 16), 0, hii_max = Inf))[, 1]
  expect_true(all(v == 1))
  lifecycle::expect_deprecated(z <- restore(rep(10, 16), 0, human_pressure_threshold = 20))
  expect_true(all(terra::values(z) == 0)) # 10 < the old threshold of 20
  expect_error(restore(rep(10, 16), 0, hii_min = 30), "hii_max")
})

test_that("the patch filter removes small zones but never fills excluded holes (audit §2 #7)", {
  z <- grid(1, 8)
  z[c(19, 20, 27, 28)] <- 0          # a 2 x 2 excluded hole (e.g. a town)
  z[1] <- 1
  z[c(2, 9, 10)] <- 0                # an isolated single eligible cell in the corner
  # min_patch_size 10 > the 4-cell hole: terra::sieve() alone fills the hole
  out <- .sieve_zone(z, min_patch_size = 10)
  v <- terra::values(out)[, 1]
  expect_true(all(v[c(19, 20, 27, 28)] == 0))
  expect_equal(v[1], 0)
  expect_equal(sum(v == 1), sum(terra::values(z) == 1) - 1)
})

test_that("the Manage Zone works without a pasture layer (F17)", {
  z <- suppressMessages(make_manage_zone(
    iso3 = "TST", pus = pus(), human_pressure = grid(1:16),
    agricultural_areas = grid(c(1, rep(0, 15))), built_areas = grid(0),
    managed_forests = grid(0), filter_patch_size = FALSE
  ))
  expect_s4_class(z, "SpatRaster")
  expect_equal(unname(terra::values(z[[2]])[1, 1]), 1) # farmland is manageable
  # Built-up farmland is excluded from both zones
  zb <- suppressMessages(make_manage_zone(
    iso3 = "TST", pus = pus(), human_pressure = grid(1:16),
    agricultural_areas = grid(c(1, rep(0, 15))), built_areas = grid(c(1, rep(0, 15))),
    managed_forests = grid(0), filter_patch_size = FALSE
  ))
  expect_equal(unname(terra::values(zb[[1]])[1, 1]), 0)
  expect_equal(unname(terra::values(zb[[2]])[1, 1]), 0)
})

test_that("protected areas can be a multi-row SpatVector in another CRS", {
  pas <- terra::vect(sf::st_transform(sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))),
    sf::st_polygon(list(rbind(c(2000, 2000), c(3000, 2000), c(3000, 3000), c(2000, 3000), c(2000, 2000)))),
    crs = ea)), 4326))
  z <- suppressWarnings(suppressMessages(make_protect_zone(
    iso3 = "TST", pus = pus(), current_protected_areas = pas, human_pressure = grid(1:16),
    agricultural_areas = grid(0), built_areas = grid(0), filter_patch_size = FALSE
  )))
  expect_s4_class(z, "SpatRaster")
})

test_that("auto encoding is read from the region and fails on unclear values", {
  # An SDG layer cropped to a region with degradation reads as SDG
  v <- terra::values(restore(rep(2, 16), c(-1, 1, 0, rep(0, 13))))[, 1]
  expect_equal(v[1:3], c(1, 0, 0))
  # A 0/1 layer with a negative NoData sentinel is an error, not silently SDG
  expect_error(restore(rep(2, 16), c(-9999, 1, rep(0, 14))), "degradation_encoding")
  # An explicit encoding is used as given
  v2 <- terra::values(restore(rep(2, 16), c(1, 0, rep(0, 14)), degradation_encoding = "sdg"))[, 1]
  expect_equal(v2[1], 0)
})

test_that("exact HII and threshold boundaries of the Restore Zone", {
  hii <- c(3.999, 4, 20.999, 21, rep(2, 12))
  v <- terra::values(restore(hii, 0))[, 1]
  expect_equal(v[1:4], c(0, 1, 1, 0))
  built <- c(0.1, 0.1000001, rep(0, 14))
  v2 <- terra::values(restore(rep(10, 16), 0, built = built))[, 1]
  expect_equal(v2[1:2], c(1, 0))
})

test_that("protected areas without a CRS are assumed to be in the PU CRS", {
  pas <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0))))))
  expect_warning(z <- suppressMessages(make_protect_zone(
    iso3 = "TST", pus = pus(), current_protected_areas = pas, human_pressure = grid(1:16),
    agricultural_areas = grid(0), built_areas = grid(0), filter_patch_size = FALSE
  )), "no CRS")
  expect_s4_class(z, "SpatRaster")
})

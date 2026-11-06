# Regression tests for the 0.4.2 vector-input fixes (audit §2 #5, #6, #12, #22;
# function review F1, F2, F6). Equal-area grid with exact 1 km cells.

ea <- "ESRI:54009"
pus_grid <- function(n = 6) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * 1000, ymin = 0, ymax = n * 1000,
              crs = ea, vals = 1)
}
square <- function(x0, y0, size = 1000) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size),
                            c(x0, y0 + size), c(x0, y0))))
}

# convert_points_polygon ---------------------------------------------------------

test_that("polygons are kept whatever their area attribute (audit §2 #12)", {
  x <- sf::st_sf(REP_AREA = c(0, NA, 2),
                 geometry = sf::st_sfc(square(0, 0), square(2000, 0),
                                       sf::st_point(c(4500, 4500)), crs = ea))
  out <- suppressWarnings(convert_points_polygon(x, area_crs = ea))
  expect_equal(nrow(out), 3)
  expect_equal(sum(sf::st_is(out, c("POLYGON", "MULTIPOLYGON"))), 3)
})

test_that("a multipoint's area is shared between its points", {
  mp <- sf::st_sf(REP_AREA = 2, geometry = sf::st_sfc(
    sf::st_multipoint(rbind(c(1000, 1000), c(5000, 5000))), crs = ea))
  out <- convert_points_polygon(mp, area_crs = ea, nQuadSegs = 200)
  expect_equal(sum(as.numeric(sf::st_area(out))) / 1e6, 2, tolerance = 0.01)
})

# KBAs ---------------------------------------------------------------------------

kbas <- function(...) {
  # At a cell centre, so the buffer size decides how many cells it reaches
  sf::st_sf(..., geometry = sf::st_sfc(sf::st_point(c(3500, 3500)), crs = ea))
}

test_that("KBA point buffers use the reported area in km2 (audit §2 #5)", {
  kba <- kbas(iso3 = "TST", azestatus = NA, kbaclass = "Global", repareakm2 = 4)
  r <- suppressMessages(make_kbas(kba, pus_grid(), "TST"))
  # A 4 km2 circle (radius 1.13 km) reaches the 3 x 3 cells around its centre.
  # With the old hectare factor it was 0.04 km2 and stayed inside one cell.
  expect_equal(sum(terra::values(r) > 0, na.rm = TRUE), 9)
})

test_that("KBAs without an iso3 column are rejected instead of all kept", {
  kba <- kbas(ISO3 = "XXX", azestatus = NA, kbaclass = "Global", repareakm2 = 4)
  expect_error(make_kbas(kba, pus_grid(), "TST"), "iso3")
})

test_that("KBAs from other countries are ignored", {
  kba <- kbas(iso3 = "XXX", azestatus = NA, kbaclass = "Global", repareakm2 = 4)
  r <- suppressMessages(make_kbas(kba, pus_grid(), "TST"))
  expect_true(all(terra::values(r) == 0))
})

# Ramsar and wetlands ---------------------------------------------------------------

test_that("Ramsar polygons are rasterised, with or without points (audit §2 #6)", {
  poly <- sf::st_sf(iso3 = "TST", area_ha = NA,
                    geometry = sf::st_sfc(square(0, 0, 2000), crs = ea))
  r <- suppressMessages(make_wetlands_ramsar(ramsar_in = poly, pus = pus_grid(), iso3 = "TST"))
  expect_equal(sum(terra::values(r) == 1, na.rm = TRUE), 4)

  both <- rbind(poly, sf::st_sf(iso3 = "TST", area_ha = 300,
                                geometry = sf::st_sfc(sf::st_point(c(5000, 5000)), crs = ea)))
  r2 <- suppressWarnings(suppressMessages(
    make_wetlands_ramsar(ramsar_in = both, pus = pus_grid(), iso3 = "TST")
  ))
  expect_gt(sum(terra::values(r2) > 0, na.rm = TRUE), 4)
  r3 <- suppressMessages(make_wetlands_ramsar(ramsar_in = both, pus = pus_grid(), iso3 = "TST",
                                              buffer_points = FALSE))
  expect_equal(sum(terra::values(r3) > 0, na.rm = TRUE), 4)
})

test_that("a 0/1 wetland mask counts as wetland (F6)", {
  wet <- pus_grid() * 0
  wet[1:6] <- 1
  r <- suppressMessages(make_wetlands_ramsar(wetlands_in = wet, pus = pus_grid(), iso3 = "TST"))
  expect_equal(sum(terra::values(r) > 0, na.rm = TRUE), 6)
})

# Protected areas --------------------------------------------------------------------

test_that("buffer_points = FALSE keeps every protected-area polygon (F1)", {
  pas <- sf::st_sf(
    STATUS = "Designated", SITE_TYPE = "PA", DESIG_ENG = "Park", REP_AREA = 1,
    geometry = sf::st_sfc(square(0, 0), square(4000, 4000), sf::st_point(c(3000, 1000)), crs = ea)
  )
  r <- suppressWarnings(suppressMessages(make_protected_areas(
    pus = pus_grid(), iso3 = "TST", from_wdpca = FALSE, sf_in = pas,
    buffer_points = FALSE, binary = TRUE, threshold = 0.5
  )))
  expect_equal(sum(terra::values(r) == 1, na.rm = TRUE), 2)
})

# exact_rasterise ---------------------------------------------------------------------

test_that("exact_rasterise mean does not average over absent features (F2)", {
  feats <- sf::st_sf(score = c(1, 1, 1),
                     geometry = sf::st_sfc(square(0, 0), square(2000, 0), square(4000, 0), crs = ea))
  r <- suppressMessages(exact_rasterise(feats, "score", "TST", pus_grid(), rescaled = FALSE))
  v <- terra::values(r)[, 1]
  expect_equal(sum(v == 1), 3) # each covered cell gets its feature's value, not 1/3
  expect_equal(sum(v == 0), 33)
})

test_that("exact_rasterise combines overlaps by fun", {
  feats <- sf::st_sf(score = c(1, 3),
                     geometry = sf::st_sfc(square(0, 5000), square(0, 5000), crs = ea))
  get <- function(f) {
    r <- suppressMessages(exact_rasterise(feats, "score", "TST", pus_grid(), rescaled = FALSE, fun = f))
    unname(terra::values(r)[1, 1])
  }
  expect_equal(get(mean), 2)
  expect_equal(get(sum), 4)
  expect_equal(get("max"), 3)
  expect_equal(get(terra::mean), 2) # terra's generic is recognised too
  expect_error(get(median), "mean, sum, max or min")
})

test_that("max is not reduced at the edges of features", {
  half <- sf::st_sf(score = 5, geometry = sf::st_sfc(square(0, 5000, 500), crs = ea))
  r <- suppressMessages(exact_rasterise(half, "score", "TST", pus_grid(), rescaled = FALSE, fun = "max"))
  expect_equal(unname(terra::values(r)[1, 1]), 5)
})

test_that("exact_rasterise with no features returns zeros", {
  none <- sf::st_sf(score = numeric(0), geometry = sf::st_sfc(crs = ea))
  r <- suppressMessages(exact_rasterise(none, "score", "TST", pus_grid(), rescaled = FALSE))
  expect_true(all(terra::values(r) == 0))
})

# Riparian and coastal zones on a 10 x 10 grid of 1 km cells.

zc_crs <- "ESRI:54009"

zc_pus <- function() {
  r <- terra::rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000,
    crs = zc_crs, vals = 1
  )
  r[100] <- NA # bottom-right cell is outside the planning units
  r
}

zc_lines <- function(coords, crs = zc_crs, ...) {
  sf::st_sf(..., geometry = sf::st_sfc(lapply(coords, sf::st_linestring), crs = crs))
}

# A horizontal river through the centres of row 3 (y = 7500), past both edges
zc_river <- function() rbind(c(-1000, 7500), c(11000, 7500))

zc_matrix <- function(r) matrix(terra::values(r), 10, byrow = TRUE)

test_that("make_riparian_zone() returns the share of each cell in the buffer", {
  r <- suppressMessages(make_riparian_zone(zc_lines(list(zc_river())), zc_pus(), buffer_m = 500))
  m <- zc_matrix(r)
  expect_equal(names(r), "riparian_zone")
  expect_equal(m[3, 1:9], rep(1, 9))
  expect_equal(sum(m[-3, ], na.rm = TRUE), 0)
  expect_true(is.na(m[10, 10]))
  # A wider buffer covers half of each neighbouring row
  m <- zc_matrix(suppressMessages(make_riparian_zone(zc_lines(list(zc_river())), zc_pus(), buffer_m = 1000)))
  expect_equal(m[2, 1:9], rep(0.5, 9))
  expect_equal(m[4, 1:9], rep(0.5, 9))
})

test_that("overlapping river segments are counted once", {
  # The same reach three times, and a tributary meeting it: cover never exceeds 1
  rivers <- zc_lines(list(zc_river(), zc_river(), zc_river(), rbind(c(5500, 7500), c(5500, -1000))))
  m <- zc_matrix(suppressMessages(make_riparian_zone(rivers, zc_pus(), buffer_m = 500)))
  expect_lte(max(m, na.rm = TRUE), 1)
  expect_equal(m[3, 1:9], rep(1, 9))
  expect_equal(m[4:9, 6], rep(1, 6))
})

test_that("rivers can be filtered, and widths read from a column", {
  rivers <- zc_lines(
    list(zc_river(), rbind(c(-1000, 2500), c(11000, 2500))),
    order = c(5, 2), width = c(500, 1500)
  )
  m <- zc_matrix(suppressMessages(make_riparian_zone(rivers, zc_pus(), buffer_m = 500,
                                                      filter_column = "order", min_value = 4)))
  expect_equal(m[3, 1], 1)
  expect_equal(m[8, 1], 0)

  m <- zc_matrix(suppressMessages(make_riparian_zone(rivers, zc_pus(), buffer_m = "width")))
  expect_equal(m[2, 1], 0) # 500 m buffer on the upper river
  expect_equal(m[c(7, 8, 9), 1], c(1, 1, 1)) # 1500 m on the lower river
})

test_that("rivers in another CRS, far away or absent are handled", {
  ll <- sf::st_transform(zc_lines(list(zc_river())), "EPSG:4326")
  m <- zc_matrix(suppressMessages(make_riparian_zone(ll, zc_pus(), buffer_m = 500)))
  expect_equal(m[3, 5], 1, tolerance = 1e-3)

  far <- zc_lines(list(rbind(c(1e6, 1e6), c(1.1e6, 1e6))))
  m <- zc_matrix(suppressMessages(make_riparian_zone(far, zc_pus())))
  expect_equal(sum(m, na.rm = TRUE), 0)
  expect_true(is.na(m[10, 10]))

  m <- zc_matrix(suppressMessages(make_riparian_zone(far[0, ], zc_pus())))
  expect_equal(sum(m, na.rm = TRUE), 0)
})

test_that("make_riparian_zone() checks its input", {
  rivers <- zc_lines(list(zc_river()))
  expect_error(make_riparian_zone(sf::st_geometry(rivers), zc_pus()), "sf object")
  expect_error(make_riparian_zone(rivers, zc_pus(), buffer_m = 0), "buffer_m")
  expect_error(make_riparian_zone(rivers, zc_pus(), buffer_m = "nope"), "buffer_m")
  expect_error(make_riparian_zone(rivers, zc_pus(), filter_column = "x"), "both")
  expect_error(
    make_riparian_zone(rivers, terra::project(zc_pus(), "EPSG:4326")),
    "projected"
  )
})

test_that("make_coastal_zone() measures a zone or a distance from lines", {
  coast <- zc_lines(list(rbind(c(0, -1000), c(0, 11000))))
  m <- zc_matrix(suppressMessages(make_coastal_zone(coast, zc_pus(), distance_km = 2.5)))
  expect_equal(m[1, ], c(1, 1, 0.5, rep(0, 7)))

  d <- suppressMessages(make_coastal_zone(coast, zc_pus(), distance_km = 2, type = "distance"))
  expect_equal(names(d), "coast_distance")
  m <- zc_matrix(d)
  # Exact within distance_km (+ half a cell diagonal), close beyond it
  expect_equal(m[1, 1:3], c(0.5, 1.5, 2.5))
  expect_true(all(abs(m[1, 4:10] - seq(3.5, 9.5, 1)) <= sqrt(2) / 2))
  expect_true(is.na(m[10, 10]))

  # With a wide enough zone every distance is exact
  d <- suppressMessages(make_coastal_zone(coast, zc_pus(), distance_km = 20, type = "distance"))
  expect_equal(zc_matrix(d)[1, ], seq(0.5, 9.5, 1))

  # Coastline just outside the planning units still counts
  off <- zc_lines(list(rbind(c(-1500, -1000), c(-1500, 11000))))
  m <- zc_matrix(suppressMessages(make_coastal_zone(off, zc_pus(), distance_km = 3, type = "distance")))
  expect_equal(m[1, 1:2], c(2, 3))
  expect_true(all(abs(m[1, ] - seq(2, 11, 1)) <= sqrt(2) / 2))

  # A diagonal coastline, where cell-based distances are furthest off
  diag_coast <- zc_lines(list(rbind(c(-1000, -1000), c(11000, 11000))))
  m <- zc_matrix(suppressMessages(make_coastal_zone(diag_coast, zc_pus(), distance_km = 20, type = "distance")))
  xy <- terra::xyFromCell(zc_pus(), 1:100)
  truth <- matrix(abs(xy[, 1] - xy[, 2]) / sqrt(2) / 1000, 10, byrow = TRUE)
  expect_equal(m[-100], truth[-100], tolerance = 1e-9)
})

test_that("make_coastal_zone() uses the outline of land or ocean polygons", {
  sea <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-5000, -5000), c(0, -5000), c(0, 15000), c(-5000, 15000), c(-5000, -5000)))),
    crs = zc_crs
  ))
  m <- zc_matrix(suppressMessages(make_coastal_zone(sea, zc_pus(), distance_km = 1)))
  expect_equal(m[5, 1:3], c(1, 0, 0))
})

test_that("make_coastal_zone() handles no coastline and bad input", {
  far <- zc_lines(list(rbind(c(1e6, 1e6), c(1.1e6, 1e6))))
  m <- zc_matrix(suppressMessages(make_coastal_zone(far, zc_pus())))
  expect_equal(sum(m, na.rm = TRUE), 0)
  expect_warning(
    suppressMessages(make_coastal_zone(far, zc_pus(), type = "distance")),
    "No coastline"
  )
  pts <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = zc_crs))
  expect_error(make_coastal_zone(pts, zc_pus()), "lines or polygons")
  expect_error(make_coastal_zone(far, zc_pus(), distance_km = 0), "distance_km")
})

test_that(".split_lines() keeps every vertex and splits long lines", {
  l <- sf::st_sfc(sf::st_linestring(cbind(1:100, 0)), crs = zc_crs)
  p <- .split_lines(l, max_vertices = 10)
  expect_equal(length(p), 11)
  expect_equal(as.numeric(sum(sf::st_length(p))), 99)
  poly <- sf::st_boundary(sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))), crs = zc_crs))
  expect_equal(length(.split_lines(poly)), 1)
})

test_that("zones are saved with the iso3 code in the file name", {
  dir <- withr::local_tempdir()
  r <- suppressMessages(make_riparian_zone(zc_lines(list(zc_river())), zc_pus(),
                                           iso3 = "PER", output_path = dir))
  f <- file.path(dir, "riparian_zone_PER.tif")
  expect_true(file.exists(f))
  expect_equal(terra::values(terra::rast(f)), terra::values(r), tolerance = 1e-6)
  suppressMessages(make_coastal_zone(zc_lines(list(rbind(c(0, 0), c(0, 10000)))), zc_pus(),
                                     output_path = dir, type = "distance"))
  expect_true(file.exists(file.path(dir, "coast_distance.tif")))
})

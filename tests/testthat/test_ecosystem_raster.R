# Ecosystem rasters must give the same answers as the equivalent polygons.

ea <- "ESRI:54009"
eco_pus <- function() {
  terra::rast(nrows = 6, ncols = 10, xmin = 0, xmax = 10000, ymin = 0, ymax = 6000,
              crs = ea, vals = 1)
}
rect <- function(x0, x1, y0 = 0, y1 = 6000) sf::st_polygon(list(rbind(
  c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
# Two ecosystems split at x = 4000, on a 250 m grid (finer than the 1 km PUs)
eco_two_rast <- function(res = 250) {
  r <- terra::rast(xmin = 0, xmax = 10000, ymin = 0, ymax = 6000, resolution = res, crs = ea)
  terra::values(r) <- ifelse(terra::xFromCell(r, seq_len(terra::ncell(r))) < 4000, 1L, 2L)
  r
}
eco_two_sf <- function() {
  sf::st_sf(eco_id = c(1, 2), geometry = sf::st_sfc(rect(0, 4000), rect(4000, 10000), crs = ea))
}
# A protected area that cuts through PUs and ecosystem cells
pa_sf <- function() sf::st_sf(geometry = sf::st_sfc(rect(0, 1100), rect(500, 2300, 0, 3000),
                                                     rect(6130, 7000), crs = ea))

test_that("underrepresented ecosystems: raster input matches polygons", {
  pus <- eco_pus()
  from_sf <- suppressMessages(make_underrepresented_ecosystems(
    eco_two_sf(), "eco_id", protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  from_r <- suppressMessages(make_underrepresented_ecosystems(
    eco_two_rast(), protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  expect_equal(terra::values(from_r), terra::values(from_sf), tolerance = 1e-6)
  expect_equal(names(from_r), "underrepresented_ecosystems")
})

test_that("ecosystem raster coverage of protected areas is exact within cells", {
  pus <- eco_pus()
  # Ecosystem 1 (x < 4000, area 24 km2) has 1100 x 6000 + 1200 x 3000 protected:
  # 10.2 km2 = 42.5%, target met. Ecosystem 2 (36 km2): 870 x 6000 = 5.22 km2,
  # 14.5%, gap 15.5 points = 0.5167 of the target
  r <- suppressMessages(make_underrepresented_ecosystems(
    eco_two_rast(res = 1000), protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  x <- terra::xFromCell(pus, seq_len(terra::ncell(pus)))
  v <- terra::values(r)[, 1]
  expect_equal(unique(v[x < 4000]), 0)
  expect_equal(unique(v[x > 4000]), (30 - 14.5) / 30, tolerance = 1e-6)
})

test_that("threatened ecosystems: raster input matches polygons", {
  pus <- eco_pus()
  degraded <- terra::rast(pus)
  terra::values(degraded) <- as.numeric(terra::xFromCell(pus, seq_len(terra::ncell(pus))) < 3000)
  run <- function(eco, g) suppressWarnings(suppressMessages(make_threatened_ecosystems_protection(
    eco, g, pus = pus, boundary_layer = eco_two_sf(), integrity_raster = degraded,
    integrity_type = "degraded", iso3 = "TST")))
  from_sf <- run(eco_two_sf(), "eco_id")
  from_r <- run(eco_two_rast(), NULL)
  expect_equal(terra::values(from_r), terra::values(from_sf), tolerance = 1e-6)
  x <- terra::xFromCell(pus, seq_len(terra::ncell(pus)))
  expect_gt(min(terra::values(from_r)[x < 4000, 1]), max(terra::values(from_r)[x > 4000, 1]))
})

test_that("a categorical raster is grouped by the chosen levels column", {
  pus <- eco_pus()
  r <- terra::rast(xmin = 0, xmax = 10000, ymin = 0, ymax = 6000, resolution = 500, crs = ea)
  x <- terra::xFromCell(r, seq_len(terra::ncell(r)))
  terra::values(r) <- ifelse(x < 2000, 10L, ifelse(x < 4000, 20L, 30L))
  levels(r) <- data.frame(value = c(10, 20, 30), code = c("a", "b", "c"),
                          biome = c("forest", "forest", "grass"))
  # By biome, 10 and 20 form one ecosystem: the same as the two-ecosystem case
  by_biome <- suppressMessages(make_underrepresented_ecosystems(
    r, "biome", protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  ref <- suppressMessages(make_underrepresented_ecosystems(
    eco_two_sf(), "eco_id", protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  expect_equal(terra::values(by_biome), terra::values(ref), tolerance = 1e-6)
  # By code, "a" (x < 2000) is well protected and "b" not: they now differ
  by_code <- suppressMessages(make_underrepresented_ecosystems(
    r, "code", protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  px <- terra::xFromCell(pus, seq_len(terra::ncell(pus)))
  v <- terra::values(by_code)[, 1]
  expect_lt(max(v[px < 2000]), min(v[px > 2000 & px < 4000]))
  expect_error(make_underrepresented_ecosystems(
    r, "nope", protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"), "levels table")
})

test_that("an ecosystem raster in another CRS is projected to the PUs", {
  pus <- eco_pus()
  r <- terra::project(eco_two_rast(res = 100), "EPSG:4326", method = "near")
  out <- suppressMessages(make_underrepresented_ecosystems(
    r, protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  ref <- suppressMessages(make_underrepresented_ecosystems(
    eco_two_sf(), "eco_id", protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  # Two reprojections move the class edge by up to a cell, so the PUs along
  # it can pick up the other ecosystem; elsewhere the values agree closely
  x <- terra::xFromCell(pus, seq_len(terra::ncell(pus)))
  away <- abs(x - 4000) > 1000
  expect_equal(terra::values(out)[away, 1], terra::values(ref)[away, 1], tolerance = 0.01)
})

test_that("PUs without ecosystem cells take the nearest PU's value", {
  pus <- eco_pus()
  r <- eco_two_rast(res = 1000)
  r[terra::cellFromXY(r, cbind(c(500, 9500), c(5500, 500)))] <- NA
  out <- suppressMessages(make_underrepresented_ecosystems(
    r, protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"))
  expect_false(anyNA(terra::values(out)[, 1]))
  at <- function(x, y) terra::extract(out, cbind(x, y))[1, 1]
  expect_equal(at(500, 5500), at(1500, 5500))
  expect_equal(at(9500, 500), at(8500, 500))
})

test_that("an ecosystem raster with no protected areas has the full gap", {
  pus <- eco_pus()
  pa0 <- pa_sf()[0, ]
  expect_warning(
    out <- suppressMessages(make_underrepresented_ecosystems(
      eco_two_rast(), protected_areas_sf = pa0, pus = pus, iso3 = "TST")),
    class = "elsar_uninformative_layer"
  )
  expect_equal(unique(terra::values(out)[, 1]), 1)
})

test_that("non-integer ecosystem rasters are refused", {
  pus <- eco_pus()
  r <- terra::rast(pus, vals = 0.5)
  expect_error(make_underrepresented_ecosystems(
    r, protected_areas_sf = pa_sf(), pus = pus, iso3 = "TST"), "integer class codes")
})

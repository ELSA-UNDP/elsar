# Countries with no usable protected-area geometry (e.g. Somalia: WDPA has
# only points with REP_AREA = 0). Protected-area functions must return a clear,
# well-formed result instead of failing.

empty_pa_pus <- function() {
  r <- terra::rast(nrows = 10, ncols = 20, xmin = 0, xmax = 20, ymin = 0, ymax = 10,
                   crs = "EPSG:3857", vals = 1)
  r[1, 1] <- NA # one cell outside the planning units
  r
}

square <- function(x0, x1) {
  sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 10), c(x0, 10), c(x0, 0))))
}

# Run expr, muffling every warning but returning their messages alongside.
with_warnings <- function(expr) {
  msgs <- character()
  value <- withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = msgs)
}

expect_all_zero_pa_layer <- function(r, pus) {
  expect_s4_class(r, "SpatRaster")
  v <- terra::values(r)[, 1]
  pv <- terra::values(pus)[, 1]
  expect_true(all(v[!is.na(pv)] == 0))
  expect_true(all(is.na(v[is.na(pv)])))
}

test_that("make_protected_areas() returns an all-0 layer for points with no area", {
  pus <- empty_pa_pus()
  pts <- sf::st_sf(
    REP_AREA = c(0, 0),
    geometry = sf::st_sfc(sf::st_point(c(5, 5)), sf::st_point(c(15, 5)), crs = "EPSG:3857")
  )
  res <- with_warnings(make_protected_areas(pus, "SOM", from_wdpca = FALSE,
                                            sf_in = pts, n_quad_segs = 5,
                                            return_sf = TRUE))
  expect_true(any(grepl("No usable protected-area geometry for SOM: 2 record", res$warnings)))
  expect_all_zero_pa_layer(res$value$raster, pus)
  expect_s3_class(res$value$sf, "sf")
  expect_equal(nrow(res$value$sf), 0)
})

test_that("make_protected_areas() handles a 0-row input and PAs outside the PUs", {
  pus <- empty_pa_pus()
  outside <- sf::st_sf(geometry = sf::st_sfc(square(100, 110), crs = "EPSG:3857"))

  for (pa in list(outside[0, ], outside)) {
    res <- with_warnings(make_protected_areas(pus, "SOM", from_wdpca = FALSE,
                                              sf_in = pa, return_sf = TRUE))
    expect_true(any(grepl("No usable protected-area geometry", res$warnings)))
    expect_all_zero_pa_layer(res$value$raster, pus)
    expect_equal(nrow(res$value$sf), 0)
  }
})

test_that("make_protected_areas() keeps returning a raster for sf_in by default", {
  pus <- empty_pa_pus()
  pa <- sf::st_sf(geometry = sf::st_sfc(square(0, 5), crs = "EPSG:3857"))
  r <- suppressMessages(make_protected_areas(pus, "TST", from_wdpca = FALSE, sf_in = pa))
  expect_s4_class(r, "SpatRaster")

  res <- suppressMessages(make_protected_areas(pus, "TST", from_wdpca = FALSE,
                                               sf_in = pa, return_sf = TRUE))
  expect_named(res, c("raster", "sf"))
  expect_gt(nrow(res$sf), 0)
})

test_that("make_protect_zone() with no PAs needs a fixed hii_threshold", {
  pus <- empty_pa_pus()
  no_pas <- sf::st_sf(geometry = sf::st_sfc(crs = "EPSG:3857"))
  hii <- terra::rast(pus, vals = rep(c(2, 6), each = 100))
  zero <- terra::rast(pus, vals = 0)

  expect_error(
    suppressMessages(make_protect_zone("SOM", pus, current_protected_areas = no_pas,
                                       agricultural_areas = zero, built_areas = zero,
                                       human_pressure = hii, filter_patch_size = FALSE)),
    "Supply a fixed 'hii_threshold'"
  )

  pz <- suppressMessages(make_protect_zone("SOM", pus, current_protected_areas = no_pas,
                                           agricultural_areas = zero, built_areas = zero,
                                           human_pressure = hii, hii_threshold = 4,
                                           filter_patch_size = FALSE))
  expect_s4_class(pz, "SpatRaster")
  expect_equal(sum(terra::values(pz) == 1, na.rm = TRUE), 99) # low-HII half minus the NA cell
})

eco_three <- function() {
  sf::st_sf(eco_id = c(1, 2, 3),
            geometry = sf::st_sfc(square(0, 8), square(8, 14), square(14, 20),
                                  crs = "EPSG:3857"))
}

test_that("make_underrepresented_ecosystems() gives unprotected ecosystems the full gap", {
  # Regression: an ecosystem no PA overlaps used to get an NA gap, which ended
  # up as 0 (lowest priority) instead of the largest gap.
  pus <- empty_pa_pus()
  pa <- sf::st_sf(geometry = sf::st_sfc(square(0, 1), crs = "EPSG:3857")) # 12.5% of eco 1
  r <- suppressWarnings(suppressMessages(
    make_underrepresented_ecosystems(eco_three(), "eco_id", protected_areas_sf = pa,
                                     pus = pus, iso3 = "TST")
  ))
  row <- terra::values(r)[21:40, 1] # second row: no NA cell
  expect_lt(max(row[1:8]), min(row[9:20]))
  expect_true(all(row[9:20] == 1))
})

test_that("make_underrepresented_ecosystems() works with no protected areas", {
  pus <- empty_pa_pus()
  no_pas <- sf::st_sf(geometry = sf::st_sfc(crs = "EPSG:3857"))
  res <- with_warnings(make_underrepresented_ecosystems(
    eco_three(), "eco_id", protected_areas_sf = no_pas, pus = pus, iso3 = "SOM"
  ))
  expect_true(any(grepl("No protected areas: every ecosystem is 0% protected", res$warnings)))
  v <- terra::values(res$value)[, 1]
  expect_true(all(v[!is.na(terra::values(pus)[, 1])] == 1))
})

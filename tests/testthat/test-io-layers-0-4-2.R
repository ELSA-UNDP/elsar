# Regression tests for the 0.4.2 I/O and layer fixes (function review F4, F5,
# F11, F12, F13, F18; P9).

ea <- "ESRI:54009"
grid <- function(vals, n = 4) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * 1000, ymin = 0, ymax = n * 1000,
              crs = ea, vals = vals)
}

test_that("save_raster writes each data type with its own NoData (F11)", {
  dir <- withr::local_tempdir()
  r16 <- grid(c(255, 1000, rep(1, 14)))
  f <- suppressMessages(save_raster(r16, file.path(dir, "u16.tif"), datatype = "INT2U"))
  expect_equal(f, file.path(dir, "u16.tif"))
  back <- terra::values(terra::rast(f))[, 1]
  expect_equal(back[1:2], c(255, 1000)) # 255 used to become NoData
  expect_no_error(suppressMessages(
    save_raster(grid(0.5), file.path(dir, "f64.tif"), datatype = "FLT8S")
  ))
  expect_error(save_raster(grid(1), file.path(dir, "x.tif"), datatype = "NOPE"), "datatype")
})

test_that("get_binary_layers tests each layer exactly (F12)", {
  s <- c(grid(NA), grid(c(4e-5, rep(0.5, 15))), grid(rep(c(0, 1), 8)), grid(c(0.99996, rep(1, 15))))
  names(s) <- c("all_na", "float", "binary", "near_one")
  out <- suppressMessages(get_binary_layers(s))
  expect_equal(names(out[[1]]), "binary")
  expect_equal(names(out[[2]]), c("all_na", "float", "near_one"))
})

test_that("get_coverage counts only cells inside the planning units (F18)", {
  pus <- grid(c(NA, rep(1, 15)))
  zone <- grid(1)
  expect_equal(get_coverage(zone, pus), 100)
  expect_equal(get_coverage(zone, grid(c(0, rep(1, 15)))), 100) # 0-coded outside
})

test_that("crop_global_raster gives zeros only when there is no overlap (F13)", {
  far <- terra::rast(nrows = 2, ncols = 2, xmin = 1e7, xmax = 1.0002e7, ymin = 0, ymax = 2000,
                     crs = ea, vals = 1)
  out <- suppressMessages(crop_global_raster(far, grid(1)))
  expect_true(all(terra::values(out) == 0))
  inside <- suppressMessages(crop_global_raster(grid(5, 8), grid(1)))
  expect_true(all(terra::values(inside) == 5))
  # Other failures are raised instead of becoming an all-0 layer
  local_mocked_bindings(.crop_to = function(...) stop("out of memory"))
  expect_error(suppressMessages(crop_global_raster(grid(5, 8), grid(1))), "out of memory")
})

test_that("filter_sf warns when an ISO3 filter matches nothing (F13)", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "x.gpkg")
  sf::st_write(sf::st_sf(code = "TST", geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = ea)),
               f, layer = "x", quiet = TRUE)
  expect_warning(out <- suppressMessages(filter_sf(f, iso3 = "TST", iso3_column = "code_wrong")),
                 "No features")
})

test_that("forest integrity weights FLII and FSII equally (F4)", {
  pus <- grid(1)
  flii <- grid(c(10000, rep(0, 15)))      # FLII is 0-10000
  fsii <- grid(c(0, 1, rep(0, 14)))       # FSII is 0-1
  fi <- suppressMessages(make_forest_integrity(flii, fsii, pus, "TST"))
  v <- terra::values(fi)[, 1]
  expect_equal(v[1], v[2])                # each index top in one cell: same score
  expect_warning(z <- suppressMessages(make_forest_integrity(grid(0), grid(0), pus, "TST")), "0 values")
  expect_true(all(terra::values(z) == 0))
})

test_that("flood abatement ranks flood-prone land lacking vegetation highest (F5)", {
  pus <- grid(1)
  ndvi <- grid(c(0.1, 0.5, 0.9, -0.1, rep(0.6, 12)))
  risk <- grid(c(1, 1, 1, 1, rep(0.1, 12)))
  out <- suppressMessages(make_flood_abatement_opportunities(risk, ndvi, pus, "TST"))
  v <- terra::values(out)[, 1]
  expect_equal(which.max(v), 1)           # flood-prone, nearly bare
  expect_gt(v[2], v[3])                   # less vegetation scores more
  expect_equal(v[4], 0)                   # water
  expect_gt(v[2], 0.3)                    # bounded: no single cell takes the whole range
})

test_that("make_postgres_connection is available again (P9)", {
  db <- make_postgres_connection(dbname = "elsa", user = "u", password = "p")
  expect_equal(db$port, 5432L)
  expect_equal(db$host, "localhost")
  expect_error(make_postgres_connection(user = "u", password = "p"), "dbname")
})

test_that("loading several layers warns once, only if nothing matched", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "multi.gpkg")
  pt <- function(code) sf::st_sf(code = code, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = ea))
  sf::st_write(pt("TST"), f, layer = "a", quiet = TRUE)
  sf::st_write(pt("OTH"), f, layer = "b", quiet = TRUE, append = TRUE)
  expect_no_warning(x <- suppressMessages(
    elsar_load_data(file_name = "multi.gpkg", file_path = dir, iso3 = "TST", iso3_column = "code")
  ))
  expect_equal(nrow(x), 1)
})


test_that("the representation gap is a share of the target, not rescaled", {
  ea <- "ESRI:54009"
  pus <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4000, ymin = 0,
                     ymax = 4000, crs = ea, vals = 1)
  sq <- function(x0, x1, y1 = 4000) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, y1), c(x0, y1), c(x0, 0))))
  # One ecosystem, 25% protected: a 5-point gap is 5/30 everywhere, not 1
  eco <- sf::st_sf(eco_id = 1, geometry = sf::st_sfc(sq(0, 4000), crs = ea))
  pa <- sf::st_sf(geometry = sf::st_sfc(sq(0, 1000), crs = ea))
  r <- suppressMessages(make_underrepresented_ecosystems(
    eco, "eco_id", protected_areas_sf = pa, pus = pus, iso3 = "TST"))
  expect_equal(unique(terra::values(r)[, 1]), 5 / 30, tolerance = 1e-6)
  # Two ecosystems: 50% protected (gap 0) and unprotected (gap 30)
  eco2 <- sf::st_sf(eco_id = c(1, 2),
                    geometry = sf::st_sfc(sq(0, 2000), sq(2000, 4000), crs = ea))
  r2 <- suppressMessages(make_underrepresented_ecosystems(
    eco2, "eco_id", protected_areas_sf = pa, pus = pus, iso3 = "TST"))
  expect_equal(terra::values(r2)[1:4, 1], c(0, 0, 1, 1))
})

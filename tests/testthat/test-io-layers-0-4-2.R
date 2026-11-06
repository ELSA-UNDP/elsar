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
                 "iso3_column|Failed|No features")
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

test_that("flood abatement is bounded and ranks vegetated flood-prone land highest (F5)", {
  pus <- grid(1)
  ndvi <- grid(c(0.9, 0.95, 0.5, -0.1, rep(0.2, 12)))
  risk <- grid(c(1, 1, 1, 1, rep(0.1, 12)))
  out <- suppressMessages(make_flood_abatement_opportunities(risk, ndvi, pus, "TST"))
  v <- terra::values(out)[, 1]
  expect_equal(which.max(v), 2)           # greenest flood-prone cell
  expect_gt(v[1], v[3])                   # more vegetation scores more
  expect_equal(v[4], 0)                   # water
  expect_gt(v[1], 0.5)                    # no single cell takes the whole range
})

test_that("make_postgres_connection is available again (P9)", {
  db <- make_postgres_connection(dbname = "elsa", user = "u", password = "p")
  expect_equal(db$port, 5432L)
  expect_equal(db$host, "localhost")
  expect_error(make_postgres_connection(user = "u", password = "p"), "dbname")
})

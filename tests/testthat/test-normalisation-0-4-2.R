# Regression tests for the 0.4.2 normalisation fixes (audit §2 #1-#4, F3).
# Grids are in an equal-area CRS with exact cell sizes, so aggregates are exact.

ea <- "ESRI:54009"

pus_1km <- function(n = 4) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * 1000, ymin = 0, ymax = n * 1000,
              crs = ea, vals = 1)
}

# A 100 m binary layer: exactly k of the 100 fine cells in each 1 km cell are 1
fine_binary <- function(k_per_cell) {
  fine <- terra::rast(nrows = 40, ncols = 40, xmin = 0, xmax = 4000, ymin = 0, ymax = 4000,
                      crs = ea, vals = 0)
  v <- matrix(0, 40, 40)
  for (bi in 0:3) for (bj in 0:3) {
    k <- k_per_cell[bi * 4 + bj + 1]
    if (k > 0) {
      idx <- seq_len(k) - 1
      v[bi * 10 + idx %/% 10 + 1, bj * 10 + idx %% 10 + 1] <- 1
    }
  }
  terra::values(fine) <- as.vector(t(v))
  fine
}

test_that("aggregation to planning units is area-weighted (audit §2 #1)", {
  k <- c(0, 5, 10, 20, 50, 100, 3, 7, 0, 1, 2, 4, 80, 60, 40, 30)
  fine <- fine_binary(k)
  out <- suppressMessages(make_normalised_raster(fine, pus_1km(), "TST", rescaled = FALSE,
                                                 name_out = "share"))
  expect_equal(as.vector(terra::values(out)), k / 100, tolerance = 1e-6)

  out_max <- suppressMessages(make_normalised_raster(fine, pus_1km(), "TST", rescaled = FALSE,
                                                     method_override = "max", name_out = "m"))
  expect_equal(as.vector(terra::values(out_max)), as.numeric(k > 0))
})

test_that("aggregation works across CRSs", {
  k <- rep(c(25, 75), 8)
  fine <- fine_binary(k)
  # Same data in a different CRS: the share per planning unit stays close
  fine_ll <- terra::project(fine, "EPSG:4326", method = "near", res = 0.0002)
  out <- suppressMessages(make_normalised_raster(fine_ll, pus_1km(), "TST", rescaled = FALSE,
                                                 name_out = "share"))
  v <- terra::values(out)[, 1]
  # The overall share matches the input (which reprojection itself shifts a little)
  expect_equal(mean(v), terra::global(fine_ll, "mean", na.rm = TRUE)[[1]], tolerance = 0.02)
  # and the 25% / 75% contrast between neighbouring cells survives. Before
  # 0.4.2 a bilinear warp blurred it to about 0.3.
  expect_true(all(abs(diff(v)) > 0.45))
})

test_that("a constant layer is not turned into NaN (audit §2 #2)", {
  r <- pus_1km() * 7
  expect_message(out <- rescale_raster(r), "constant")
  expect_true(all(terra::values(out) == 1))
  zero <- pus_1km() * 0
  expect_message(out0 <- rescale_raster(zero), "constant")
  expect_true(all(terra::values(out0) == 0))
})

test_that("missing data gets the lowest score, also when inverted (audit §2 #4)", {
  r <- pus_1km()
  terra::values(r) <- c(NA, 1:15)
  out <- suppressMessages(make_normalised_raster(r, pus_1km(), "TST", invert = TRUE, name_out = "x"))
  v <- terra::values(out)[, 1]
  expect_equal(v[1], 0)          # missing cell: lowest
  expect_equal(v[2], 1)          # smallest value, inverted: highest
  expect_equal(v[16], 0)         # largest value, inverted: lowest
})

test_that("layers with only non-positive values are not treated as empty (F3)", {
  r <- pus_1km()
  terra::values(r) <- c(0, -(1:15)) # maximum exactly 0: the old check returned all 0
  out <- suppressMessages(make_normalised_raster(r, pus_1km(), "TST", name_out = "neg"))
  v <- terra::values(out)[, 1]
  expect_equal(v[1], 1)
  expect_equal(v[16], 0)
  expect_equal(names(out), "neg")
})

test_that("an all-NA input returns zeros inside the planning units, named and saved", {
  r <- pus_1km() * NA
  dir <- withr::local_tempdir()
  out <- suppressWarnings(suppressMessages(make_normalised_raster(
    r, pus_1km(), "TST", name_out = "empty", output_path = dir
  )))
  expect_true(all(terra::values(out) == 0))
  expect_equal(names(out), "empty")
  expect_true(file.exists(file.path(dir, "empty_TST.tif")))
})

test_that("zone thresholds use true proportions, not rescaled ones (audit §2 #3)", {
  # 2-4% agriculture everywhere: none of it should pass a 10% threshold
  lulc <- fine_binary(rep(c(2, 3, 4, 3), 4)) # value 1 = agriculture
  lulc <- terra::ifel(lulc == 1, 5, 1)       # ESRI agriculture class 5
  pus <- pus_1km()
  hii <- pus * 0
  pas <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))), crs = ea))
  zone <- suppressWarnings(suppressMessages(make_protect_zone(
    iso3 = "TST", pus = pus, current_protected_areas = pas, human_pressure = hii,
    lulc_raster = lulc, hii_threshold = 1, agriculture_threshold = 0.1,
    filter_patch_size = FALSE
  )))
  expect_true(all(terra::values(zone) == 1))
})

test_that("urban greening ranks low vegetation highest and water lowest", {
  pus <- pus_1km(2)
  names(pus) <- "pu" # any layer name works
  ndvi <- pus
  terra::values(ndvi) <- c(-0.2, 0.1, 0.5, 0.9) # water, bare, medium, green
  built <- pus * 1
  # No heat-exposure data for this country, so only NDVI ranks the cells
  sdei <- sf::st_sf(CTR_MN_ISO = "XXX", ID_HDC_G0 = 1, avg_intens = 1,
                    geometry = sf::st_sfc(sf::st_point(c(500, 500)), crs = ea))
  out <- suppressWarnings(suppressMessages(make_urban_greening_opportunities(
    ndvi_raster = ndvi, built_areas_raster = built, sdei_statistics = sdei,
    pus = pus, iso3 = "TST"
  )))
  v <- terra::values(out[[1]])[, 1]
  expect_true(v[2] > v[3] && v[3] > v[4])
  expect_equal(v[1], 0)
})

# Cases from the independent review of PR #105 ----------------------------------

pus_offset <- function() {
  terra::rast(nrows = 20, ncols = 20, xmin = 1e6, xmax = 1.02e6, ymin = 5e6, ymax = 5.02e6,
              crs = ea, vals = 1)
}

# Exact area-weighted mean of a lon/lat raster over each planning unit
truth_mean <- function(x, pus) {
  polys <- sf::st_as_sf(terra::as.polygons(pus, dissolve = FALSE, aggregate = FALSE))
  polys <- sf::st_transform(sf::st_segmentize(polys, 50), terra::crs(x))
  exactextractr::exact_extract(x, polys, "mean", progress = FALSE)
}

test_that("inputs coarser than the PUs in another CRS are area-weighted", {
  pus <- pus_offset()
  e <- terra::ext(terra::project(terra::as.polygons(terra::ext(pus), crs = ea), "EPSG:4326"))
  coarse <- terra::rast(terra::ext(e[1] - 0.2, e[2] + 0.2, e[3] - 0.2, e[4] + 0.2),
                        res = 0.05, crs = "EPSG:4326")
  set.seed(42)
  terra::values(coarse) <- stats::runif(terra::ncell(coarse))
  out <- .align_to_pus(coarse, pus)
  # Cell order of as.polygons() follows the raster, so values line up
  expect_lt(mean(abs(terra::values(out)[, 1] - truth_mean(coarse, pus))), 0.01)
})

test_that("max keeps small features and sum keeps totals across CRSs", {
  pus <- pus_offset()
  e <- terra::ext(terra::project(terra::as.polygons(terra::ext(pus), crs = ea), "EPSG:4326"))
  fine <- terra::rast(terra::ext(e[1] - 0.01, e[2] + 0.01, e[3] - 0.01, e[4] + 0.01),
                      res = 0.001, crs = "EPSG:4326", vals = 0)
  set.seed(7)
  hits <- sample(terra::ncell(fine), 150)
  fine[hits] <- 1
  mx <- .align_to_pus(fine, pus, method = "max")
  found <- terra::extract(mx, terra::project(terra::vect(terra::xyFromCell(fine, hits),
                                                         crs = "EPSG:4326"), ea))[, 2]
  expect_gte(mean(found == 1, na.rm = TRUE), 0.97)

  counts <- terra::rast(fine)
  terra::values(counts) <- 2
  inside <- terra::crop(counts, terra::project(terra::as.polygons(terra::ext(pus), crs = ea),
                                               "EPSG:4326"), mask = TRUE)
  total_in <- terra::global(inside * 0 + 2, "sum", na.rm = TRUE)[[1]]
  s <- .align_to_pus(counts, pus, method = "sum")
  expect_equal(terra::global(s, "sum", na.rm = TRUE)[[1]], total_in, tolerance = 0.02)
})

test_that("planning units across the antimeridian stay fast and correct", {
  moll <- "+proj=moll +lon_0=178 +datum=WGS84 +units=m"
  pus <- terra::rast(nrows = 10, ncols = 40, xmin = -2e5, xmax = 2e5, ymin = -1.9e6,
                     ymax = -1.8e6, crs = moll, vals = 1)
  lon <- terra::rast(terra::ext(-180, 180, -20, -10), res = 0.1, crs = "EPSG:4326")
  terra::values(lon) <- terra::xFromCell(lon, seq_len(terra::ncell(lon)))
  elapsed <- system.time(out <- .align_to_pus(lon, pus))[["elapsed"]]
  expect_lt(elapsed, 10)
  expect_false(anyNA(terra::values(out)))
})

test_that("partial and no overlap give 0 inside the PUs", {
  pus <- pus_1km()
  half <- terra::rast(nrows = 4, ncols = 2, xmin = 2000, xmax = 4000, ymin = 0, ymax = 4000,
                      crs = ea, vals = 1)
  out <- suppressMessages(make_normalised_raster(half, pus, "TST", rescaled = FALSE, name_out = "h"))
  v <- matrix(terra::values(out)[, 1], 4, byrow = TRUE)
  expect_true(all(v[, 1:2] == 0) && all(v[, 3:4] == 1))
  far <- terra::rast(nrows = 2, ncols = 2, xmin = 1e7, xmax = 1.0002e7, ymin = 0, ymax = 2000,
                     crs = ea, vals = 1)
  out2 <- suppressMessages(make_normalised_raster(far, pus, "TST", name_out = "f"))
  expect_true(all(terra::values(out2) == 0))
})

test_that("fill_na = NULL leaves gaps NA but still masks to the PUs", {
  pus <- pus_1km()
  pus[16] <- NA
  r <- pus_1km()
  terra::values(r) <- c(NA, 1:15)
  out <- suppressMessages(make_normalised_raster(r, pus, "TST", fill_na = NULL, name_out = "x"))
  v <- terra::values(out)[, 1]
  expect_true(is.na(v[1]))
  expect_true(is.na(v[16]))
})

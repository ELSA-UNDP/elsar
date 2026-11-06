# Regression tests for the 0.4.2 region fixes: antimeridian projection (audit
# §2 #8), mainland (F9, P2), planning-unit grids (F10) and ISO3 case (P8).

ll <- function(...) sf::st_sfc(..., crs = 4326)
box <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
}

test_that("the projection is centred on regions crossing the antimeridian", {
  fiji <- sf::st_sf(iso3cd = "FJI", geometry = ll(sf::st_multipolygon(list(
    box(177, 179.99, -19, -16), box(-180, -178, -19, -16)
  ))))
  wkt <- suppressMessages(make_custom_mollweide_projection(fiji, iso3 = "FJI"))
  lon0 <- as.numeric(sub(".*central_meridian\",([-0-9.e]+).*", "\\1", gsub("\\s", "", wkt)))
  expect_gt(abs(lon0), 170)
  # Projected width stays country-sized (was ~35,000 km)
  w <- diff(sf::st_bbox(sf::st_transform(fiji, sf::st_crs(wkt)))[c(1, 3)])
  expect_lt(as.numeric(w), 1e6)
  # Ordinary regions are unchanged
  npl <- sf::st_sf(iso3cd = "NPL", geometry = ll(box(80, 88, 26, 30)))
  wkt2 <- suppressMessages(make_custom_mollweide_projection(npl, iso3 = "NPL"))
  expect_match(gsub("\\s", "", wkt2), "central_meridian\",84[],]")
})

provinces <- function() {
  # A mainland of two provinces (the larger one is half the mainland) and an island
  sf::st_sf(iso3cd = "TST", name = c("west", "east", "island"),
            geometry = ll(box(0, 2, 0, 2), box(2, 3.5, 0, 2), box(5, 5.5, 0, 0.5)))
}

test_that("limit_to_mainland keeps the whole mainland, not one province (F9)", {
  b <- suppressMessages(make_boundary(provinces(), iso3 = "TST", iso3_column = "iso3cd",
                                      limit_to_mainland = TRUE, custom_projection = FALSE))
  expect_equal(nrow(b), 1)
  bb <- sf::st_bbox(b)
  expect_equal(unname(c(bb$xmin, bb$xmax)), c(0, 3.5))
})

test_that("make_planning_units handles several features and limit_to_mainland (F10, P2)", {
  b <- suppressMessages(make_boundary(provinces(), iso3 = "TST", iso3_column = "iso3cd",
                                      custom_projection = TRUE))
  expect_gt(nrow(b), 1)
  pus <- suppressMessages(make_planning_units(b, pu_threshold = 5000, iso3 = "TST"))
  expect_s4_class(pus, "SpatRaster")
  main <- suppressMessages(make_planning_units(b, pu_threshold = 5000, iso3 = "TST",
                                               limit_to_mainland = TRUE))
  island <- sf::st_transform(sf::st_sf(geometry = ll(box(5.1, 5.4, 0.1, 0.4))), terra::crs(pus))
  cover <- exactextractr::exact_extract(main, island, "sum", progress = FALSE)
  expect_equal(cover, 0)
})

test_that("the planning grid covers the whole boundary (F10)", {
  b <- sf::st_sf(geometry = sf::st_sfc(box(0, 10400, 0, 10400), crs = "ESRI:54009"))
  pus <- suppressMessages(make_planning_units(b, pu_size = 1000, iso3 = "TST"))
  e <- terra::ext(pus)
  expect_gte(e$xmax, 10400)
  expect_gte(e$ymax, 10400)
  expect_equal(terra::global(pus, "sum", na.rm = TRUE)[[1]], 11 * 11)
})

test_that("a too-fine first size estimate grows instead of failing", {
  # Many tiny islands: with touches = TRUE the first estimate overshoots
  set.seed(3)
  pts <- sf::st_sfc(lapply(seq_len(300), function(i) {
    x <- stats::runif(1, 0, 2e6); y <- stats::runif(1, 0, 2e6)
    box(x, x + 300, y, y + 300)
  }), crs = "ESRI:54009")
  b <- sf::st_sf(geometry = pts)
  pus <- suppressMessages(make_planning_units(b, pu_threshold = 100, iso3 = "TST"))
  expect_lte(terra::global(pus, "sum", na.rm = TRUE)[[1]], 105)
})

test_that("lower-case ISO3 codes match upper-case data (P8)", {
  expect_equal(iso3_base("npl"), "NPL")
  expect_equal(iso3_base("usa_ca"), "USA")
  b <- suppressMessages(make_boundary(provinces(), iso3 = "tst", iso3_column = "iso3cd",
                                      custom_projection = FALSE))
  expect_equal(nrow(b), 3)
})

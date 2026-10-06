# Sub-national ISO3 codes (e.g. "USA_CA") filter global datasets by their
# national part, and national PA layers are cropped to the planning units.

make_test_pus <- function() {
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                   crs = "EPSG:3857", vals = 1)
  names(r) <- "Planning Units"
  r
}

square <- function(x0, y0, size = 2) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size),
                            c(x0, y0 + size), c(x0, y0))))
}

test_that("iso3_base returns the national part of a code", {
  expect_equal(iso3_base("USA_CA"), "USA")
  expect_equal(iso3_base("ECU-GEF8"), "ECU")
  expect_equal(iso3_base("KEN"), "KEN")
})

test_that("crop_to_extent keeps only features overlapping the raster extent", {
  pus <- make_test_pus()
  x <- sf::st_sf(id = 1:3,
                 geometry = sf::st_sfc(square(1, 1), square(9, 9), square(50, 50),
                                       crs = "EPSG:3857"))
  out <- crop_to_extent(x, pus)
  expect_equal(out$id, c(1L, 2L))
  expect_equal(nrow(crop_to_extent(x[0, ], pus)), 0)
})

test_that("make_kbas matches a sub-national code by its national part", {
  pus <- make_test_pus()
  kba <- sf::st_sf(
    iso3 = c("USA", "MEX"),
    azestatus = NA_character_,
    kbaclass = "Global",
    geometry = sf::st_sfc(square(1, 1), square(5, 5), crs = "EPSG:3857")
  )
  r <- make_kbas(kba_in = kba, pus = pus, iso3 = "USA_CA")
  # Only the USA square (4 cells) is retained; the MEX square is filtered out.
  expect_equal(sum(terra::values(r) > 0, na.rm = TRUE), 4)
})

test_that("make_protected_areas drops PAs outside the planning units before dissolving", {
  pus <- make_test_pus()
  pas <- sf::st_sf(
    STATUS = "Designated",
    SITE_TYPE = "PA",
    geometry = sf::st_sfc(square(1, 1), square(500, 500), crs = "EPSG:3857")
  )
  expect_message(
    r <- make_protected_areas(pus = pus, iso3 = "USA_CA", from_wdpca = FALSE, sf_in = pas),
    "Dropping 1 of 2 features"
  )
  expect_equal(sum(terra::values(r) > 0, na.rm = TRUE), 4)
})

test_that("wad_dat", {
  wad_dat <- get_wad_data()
  expect_equal(class(wad_dat)[1], "SpatRaster")
  expect_equal(terra::global(wad_dat, "min", na.rm = TRUE)[[1]], 0)
})

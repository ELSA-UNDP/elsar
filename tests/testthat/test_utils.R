test_that("utils (rescale)", {
  wad_subset <- elsar::get_wad_data()
  wad_rescaled <- rescale_raster(wad_subset)

  expect_equal(class(wad_rescaled)[1], "SpatRaster")
  expect_equal(terra::global(wad_rescaled, "min", na.rm = TRUE)[[1]], 0)
  expect_equal(terra::global(wad_rescaled, "max", na.rm = TRUE)[[1]], 1)
})

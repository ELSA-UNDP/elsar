test_that("WAD data load", {
  wad_subset <- elsar::get_wad_data()
  expect_true(inherits(wad_subset, "SpatRaster"))
  expect_lte(terra::global(wad_subset, "max", na.rm = TRUE)[[1]], 13)
})

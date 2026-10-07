test_that("utils (rescale)", {
  wad_subset <- elsar::get_wad_data()
  wad_rescaled <- rescale_raster(wad_subset)

  expect_equal(class(wad_rescaled)[1], "SpatRaster")
  expect_equal(terra::global(wad_rescaled, "min", na.rm = TRUE)[[1]], 0)
  expect_equal(terra::global(wad_rescaled, "max", na.rm = TRUE)[[1]], 1)
})

test_that("median_from_rast errors explain how to create the side-car", {
  tif <- tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 4, ncols = 4, vals = seq(0, 1, length.out = 16))
  terra::writeRaster(r, tif, overwrite = TRUE)
  unlink(paste0(tif, ".aux.xml"))
  r <- terra::rast(tif)

  # No side-car at all
  expect_error(median_from_rast(r), "Side-car XML not found")
  expect_error(median_from_rast(r), "gdalinfo -hist", fixed = TRUE)

  # Side-car without a histogram (e.g. only statistics were computed)
  writeLines("<PAMDataset></PAMDataset>", paste0(tif, ".aux.xml"))
  expect_error(median_from_rast(r), "No <HistItem>", fixed = TRUE)
  expect_error(median_from_rast(r), "gdalinfo -hist", fixed = TRUE)

  # Following the instruction fixes it
  unlink(paste0(tif, ".aux.xml"))
  sf::gdal_utils("info", tif, options = "-hist", quiet = TRUE)
  expect_true(is.numeric(median_from_rast(r)))
})

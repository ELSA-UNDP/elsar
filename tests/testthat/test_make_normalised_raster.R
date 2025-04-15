test_that("make_normalised_raster (not inverted)", {
  boundary_proj <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd",
    custom_projection = TRUE
  )

  pus <- make_planning_units(
    boundary_proj = boundary_proj,
    pu_size = NULL,
    pu_threshold = 8.5e5,
    limit_to_mainland = FALSE
  )

  wad_subset <- elsar::get_wad_data()

  wadOut <- make_normalised_raster(raster_in = wad_subset,
                                   pus = pus,
                                   iso3 = "NPL")

  expect_equal(class(wadOut)[1], "SpatRaster")
  expect_equal(terra::global(wadOut, "min", na.rm = TRUE)[[1]], 0)
  expect_equal(terra::global(wadOut, "max", na.rm = TRUE)[[1]], 1)
})

test_that("make_normalised_raster (inverted)", {
  boundary_proj <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd",
    custom_projection = TRUE
  )

  pus <- make_planning_units(
    boundary_proj = boundary_proj,
    pu_size = NULL,
    pu_threshold = 8.5e5,
    limit_to_mainland = FALSE
  )

  wad_subset <- elsar::get_wad_data()

  wadOut <- make_normalised_raster(
    raster_in = wad_subset,
    pus = pus,
    invert = TRUE,
    iso3 = "NPL"
  )

  expect_equal(class(wadOut)[1], "SpatRaster")
  expect_equal(terra::global(wadOut, "min", na.rm = TRUE)[[1]], 0)
  expect_equal(terra::global(wadOut, "max", na.rm = TRUE)[[1]], 1)
})

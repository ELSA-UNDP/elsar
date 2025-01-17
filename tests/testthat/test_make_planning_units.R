test_that("planning_units", {
  boundary_proj <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd",
    custom_projection = TRUE
  )

  pus <- make_planning_units(boundary_proj = boundary_proj,
                             pu_size = NULL,
                             pu_threshold = 8.5e5,
                             limit_to_mainland = FALSE)
  expect_equal(class(pus)[1], "SpatRaster")
  expect_equal(terra::global(pus, "min", na.rm = TRUE)[[1]], 0)
  expect_equal(terra::global(pus, "max", na.rm = TRUE)[[1]], 1)
  expect_lt(terra::global(pus, "sum", na.rm = TRUE)[[1]], 850000)
})

test_that("custom_projection", {
  boundary <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd"
  )

  wkt <- make_custom_projection(boundary = boundary, iso3 = "NPL")
  expect_equal(class(wkt)[2], "character")
})

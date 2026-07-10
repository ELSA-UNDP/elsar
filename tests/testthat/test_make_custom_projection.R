test_that("make_custom_mollweide_projection", {
  boundary <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd"
  )

  wkt <- make_custom_mollweide_projection(boundary = boundary, iso3 = "NPL")
  expect_equal(class(wkt)[2], "character")
})

test_that("make_custom_projection() is deprecated but still works", {
  boundary <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd"
  )

  expect_warning(
    wkt <- make_custom_projection(boundary = boundary, iso3 = "NPL"),
    "deprecated"
  )
  expect_type(wkt, "character")
})

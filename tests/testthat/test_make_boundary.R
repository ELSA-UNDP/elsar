test_that("make_boundary (custom_projection == FALSE)", {
  boundary <- make_boundary(
    boundary_in = boundary_dat,
    iso3 = "NPL",
    iso3_column = "iso3cd"
  )
  expect_true(inherits(boundary, "sf"))
})

# Need to figure out what we want to test here more clearly?
# test_that("make_boundary (custom_projection == TRUE)", {
#   boundary <- make_boundary(
#     boundary_in = boundary_dat,
#     iso3 = "NPL",
#     iso3_column = "iso3cd"
#   )
#
#   wkt <- make_custom_projection(boundary = boundary, iso3 = "NPL")
#   boundary_proj <- sf::st_transform(boundary, crs = sf::st_crs(wkt))
#
#   boundary_proj2 <- make_boundary(
#     boundary_in = boundary_dat,
#     iso3 = "NPL",
#     iso3_column = "iso3cd",
#     custom_projection = TRUE
#   )
#
#   expect_true(inherits(boundary_proj, "sf"))
#   expect_true(inherits(boundary_proj2, "sf"))
#   expect_equal(sf::st_crs(boundary_proj), sf::st_crs(boundary_proj2))
# })

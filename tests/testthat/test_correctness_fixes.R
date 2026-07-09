# Regression tests for the correctness fixes in the docs/access branch.

# ---- make_boundary ---------------------------------------------------------

test_that("make_boundary() errors on an iso3 that matches no feature", {
  # Previously returned a 0-row sf silently (or a cryptic GDAL WKT error).
  expect_error(
    make_boundary(boundary_in = boundary_dat, iso3 = "NEP",
                  iso3_column = "iso3cd", custom_projection = FALSE),
    "No features found for iso3 = 'NEP'"
  )
  # The message lists the codes that ARE available.
  expect_error(
    make_boundary(boundary_in = boundary_dat, iso3 = "NEP",
                  iso3_column = "iso3cd", custom_projection = FALSE),
    "NPL"
  )
})

test_that("make_boundary() errors when iso3_column is not a column", {
  expect_error(
    make_boundary(boundary_in = boundary_dat, iso3 = "NPL",
                  iso3_column = "not_a_column", custom_projection = FALSE),
    "is not a column"
  )
})

test_that("make_boundary(limit_to_mainland = TRUE) works and stays in-country", {
  # Previously always errored (sf::st_area on an rlang data pronoun).
  b <- make_boundary(boundary_in = boundary_dat, iso3 = "NPL",
                     iso3_column = "iso3cd", limit_to_mainland = TRUE,
                     custom_projection = FALSE)
  expect_s3_class(b, "sf")
  expect_equal(nrow(b), 1L)
  # Filtered to Nepal before choosing the largest polygon, not across all rows.
  expect_true(all(b$iso3cd == "NPL"))
  # Normalised to WGS84 (a geographic CRS).
  expect_true(sf::st_is_longlat(b))
})

# ---- make_planning_units ---------------------------------------------------

test_that("make_planning_units() rejects a geographic (lat/long) boundary", {
  # Previously produced a silent 1-cell raster.
  nb <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
  expect_true(sf::st_is_longlat(nb))
  expect_error(
    make_planning_units(boundary_proj = nb, iso3 = "NPL"),
    "geographic"
  )
})

test_that("make_planning_units() still works on a projected boundary", {
  bp <- make_boundary(boundary_in = boundary_dat, iso3 = "NPL",
                      iso3_column = "iso3cd", custom_projection = TRUE)
  pus <- make_planning_units(boundary_proj = bp, iso3 = "NPL", pu_size = 5000)
  expect_s4_class(pus, "SpatRaster")
  expect_gt(terra::ncell(pus), 1)
})

# ---- elsar_load_data (PostgreSQL path) -------------------------------------

test_that("elsar_load_data() PostgreSQL path guards on RPostgres", {
  # When RPostgres is absent, the user gets an actionable install message
  # rather than the old `!!!`-splice "invalid argument type" error.
  skip_if(requireNamespace("RPostgres", quietly = TRUE),
          "RPostgres installed; guard path not exercised")
  expect_error(
    elsar_load_data(file_name = "postgres", file_lyr = "admin",
                    db_info = list(host = "h", dbname = "d", port = 5432L,
                                   user = "u", password = "p")),
    "RPostgres"
  )
})

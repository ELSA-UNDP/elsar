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

test_that("make_boundary() errors clearly when the boundary has no CRS", {
  nb <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
  sf::st_crs(nb) <- NA
  expect_error(
    make_boundary(nb, iso3 = "NPL", iso3_column = "iso3cd",
                  filter_by_iso3 = FALSE, custom_projection = FALSE),
    "no coordinate reference system"
  )
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

# ---- elsar_load_data aggregation (filter_sf failures) ----------------------

# Helper: write a small valid shapefile from boundary_dat.
.write_shp <- function(dir, iso, name) {
  suppressWarnings(sf::st_write(
    boundary_dat[boundary_dat$iso3cd == iso, ],
    file.path(dir, name), quiet = TRUE, delete_dsn = TRUE
  ))
}

test_that("elsar_load_data() returns an sf when all reads succeed", {
  dir <- file.path(tempdir(), "eld_ok"); dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE))
  .write_shp(dir, "NPL", "a.shp"); .write_shp(dir, "IND", "b.shp")

  res <- suppressMessages(elsar_load_data(file_name = NULL, file_path = dir))
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2L)
})

test_that("combine_layers() errors clearly when layers have different CRS", {
  a <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
  b <- sf::st_transform(boundary_dat[boundary_dat$iso3cd == "IND", ], 3857)
  expect_error(
    suppressMessages(combine_layers(list(a, b), c("a.shp", "b.shp"), noun = "file")),
    "do not share a coordinate reference system"
  )
})

test_that("elsar_load_data() warns but continues on partial read failure", {
  dir <- file.path(tempdir(), "eld_partial"); dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE))
  .write_shp(dir, "NPL", "a.shp")
  writeLines("junk", file.path(dir, "bad.shp"))

  expect_warning(
    res <- suppressMessages(elsar_load_data(file_name = NULL, file_path = dir)),
    "failed to read"
  )
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1L)
})

test_that("elsar_load_data() errors when every read fails (was a 0x0 tibble)", {
  dir <- file.path(tempdir(), "eld_allfail"); dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines("junk", file.path(dir, "a.shp"))
  writeLines("junk", file.path(dir, "b.shp"))

  expect_error(
    suppressMessages(elsar_load_data(file_name = NULL, file_path = dir)),
    "Failed to read all"
  )
})

test_that("elsar_load_data() errors on a single unreadable file (was silent NULL)", {
  dir <- file.path(tempdir(), "eld_single"); dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines("junk", file.path(dir, "a.shp"))

  expect_error(
    suppressMessages(elsar_load_data(file_name = "a.shp", file_path = dir)),
    "Failed to read"
  )
})

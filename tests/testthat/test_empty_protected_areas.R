# Regression tests for make_underrepresented_ecosystems().

empty_pa_pus <- function() {
  r <- terra::rast(nrows = 10, ncols = 20, xmin = 0, xmax = 20, ymin = 0, ymax = 10,
                   crs = "EPSG:3857", vals = 1)
  r[1, 1] <- NA # one cell outside the planning units
  r
}

square <- function(x0, x1) {
  sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 10), c(x0, 10), c(x0, 0))))
}

# Run expr, muffling every warning but returning their messages alongside.
with_warnings <- function(expr) {
  msgs <- character()
  value <- withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = msgs)
}

expect_all_zero_pa_layer <- function(r, pus) {
  expect_s4_class(r, "SpatRaster")
  v <- terra::values(r)[, 1]
  pv <- terra::values(pus)[, 1]
  expect_true(all(v[!is.na(pv)] == 0))
  expect_true(all(is.na(v[is.na(pv)])))
}

eco_three <- function() {
  sf::st_sf(eco_id = c(1, 2, 3),
            geometry = sf::st_sfc(square(0, 8), square(8, 14), square(14, 20),
                                  crs = "EPSG:3857"))
}

test_that("make_underrepresented_ecosystems() gives unprotected ecosystems the full gap", {
  # Regression: an ecosystem no PA overlaps used to get an NA gap, which ended
  # up as 0 (lowest priority) instead of the largest gap.
  pus <- empty_pa_pus()
  pa <- sf::st_sf(geometry = sf::st_sfc(square(0, 1), crs = "EPSG:3857")) # 12.5% of eco 1
  r <- suppressWarnings(suppressMessages(
    make_underrepresented_ecosystems(eco_three(), "eco_id", protected_areas_sf = pa,
                                     pus = pus, iso3 = "TST")
  ))
  row <- terra::values(r)[21:40, 1] # second row: no NA cell
  expect_lt(max(row[1:8]), min(row[9:20]))
  expect_true(all(row[9:20] == 1))
})

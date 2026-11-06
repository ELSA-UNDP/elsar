# Regression tests for 0.4.2 calibration fixes: PA lock-ins are enforced, as
# in the original ELSA pre-calibration (F14), and 0/1 zone masks keep their
# eligibility (F15).

calibration_fixture <- function() {
  r0 <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                    crs = "EPSG:3857")
  xx <- terra::init(r0, "x")
  # One feature, only on the right half; the lock-in is on the left half
  features <- c((xx > 5) * 1)
  names(features) <- "feat"
  pu <- terra::ifel(!is.na(features), 1, NA)
  list(
    features = features, pu = pu,
    impacts = data.frame(feature = "feat", protect = 1, restore = 0.5),
    lockin = (xx < 2) * 1 # 20 cells
  )
}

solve_once <- function(fx, zones_pu, lockins, budgets) {
  prepared <- .apply_lockins(zones_pu, lockins, fx$pu)
  prob <- .elsar_calibration_problem(
    features = fx$features, impacts = fx$impacts, zones_pu = prepared$zones_pu,
    planning_units = fx$pu, budgets = budgets, lockins = lockins,
    locked = prepared$locked, zone_names = names(zones_pu),
    all_zone_names = c(names(zones_pu), "do_nothing"),
    gap = 0, solver = "highs", threads = 1, time_limit = 60
  )
  w <- matrix(c(1, 1, 0), nrow = 1)
  # (prioritizr warns about the all-zero do_nothing zone and presolve checks)
  sol <- suppressWarnings(prioritizr::solve.ConservationProblem(
    prioritizr::add_feature_weights(prob, w), force = TRUE
  ))
  list(sol = sol, prepared = prepared)
}

test_that("locked-in cells are allocated to their zone", {
  skip_if_not_installed("prioritizr")
  skip_if_not_installed("highs")
  fx <- calibration_fixture()
  zones <- list(protect = fx$pu, restore = fx$pu)
  out <- solve_once(fx, zones, list(protect = fx$lockin), c(protect = 10, restore = 10))
  locked_cells <- which(terra::values(fx$lockin)[, 1] == 1)
  protect <- terra::values(out$sol[[1]])[, 1]
  expect_true(all(protect[locked_cells] == 1)) # before 0.4.2: none were
})

test_that("0/1 zone masks become 1/NA, and lock-ins leave other zones", {
  fx <- calibration_fixture()
  restore_01 <- terra::ifel(terra::init(fx$pu, "x") > 8, 1, 0) # 0 = not eligible
  prepared <- .apply_lockins(list(protect = fx$pu, restore = restore_01),
                             list(protect = fx$lockin), fx$pu)
  r <- terra::values(prepared$zones_pu$restore)[, 1]
  expect_true(all(is.na(r[terra::values(restore_01)[, 1] == 0])))
  locked <- terra::values(fx$lockin)[, 1] == 1
  expect_true(all(is.na(r[locked])))
  expect_equal(sum(!is.na(terra::values(prepared$locked))), 20)
  expect_error(.apply_lockins(list(protect = fx$pu), list(other = fx$lockin), fx$pu), "lockins")
})

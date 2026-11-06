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
    planning_units = fx$pu, budgets = budgets, lockins = prepared$lockins,
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

# Cases from the independent review of PR #109 -----------------------------------

test_that("fractional lock-ins stay feasible with a budget below the locked count", {
  skip_if_not_installed("prioritizr")
  skip_if_not_installed("highs")
  fx <- calibration_fixture()
  frac <- fx$lockin * 0.3 # 20 cells at 0.3 cover: raw sum 6% of the grid, 20 cells locked
  zones <- list(protect = fx$pu, restore = fx$pu)
  out <- solve_once(fx, zones, list(protect = frac), c(protect = 5, restore = 10))
  locked_cells <- which(terra::values(fx$lockin)[, 1] == 1)
  expect_true(all(terra::values(out$sol[[1]])[locked_cells, 1] == 1))
})

test_that("overlapping lock-ins follow the ELSA tool's order, whatever the list order", {
  fx <- calibration_fixture()
  zones <- list(restore = fx$pu, manage = fx$pu, protect = fx$pu)
  both <- list(restore = fx$lockin, manage = fx$lockin)
  p <- .apply_lockins(zones, both, fx$pu)
  locked <- terra::values(fx$lockin)[, 1] == 1
  expect_true(all(terra::values(p$zones_pu$manage)[locked, 1] == 1)) # manage before restore
  expect_true(all(is.na(terra::values(p$zones_pu$restore)[locked, 1])))
  p2 <- .apply_lockins(zones, both, fx$pu, priority = "restore")
  expect_true(all(terra::values(p2$zones_pu$restore)[locked, 1] == 1))
  expect_equal(.lockin_priority(c("urban-adapt", "restore", "reduce-pressures", "protect")),
               c("protect", "reduce-pressures", "restore", "urban-adapt"))
})

test_that("cover exactly at the threshold is not locked in", {
  fx <- calibration_fixture()
  p <- .apply_lockins(list(protect = fx$pu), list(protect = fx$lockin * 0.1), fx$pu)
  expect_true(all(is.na(terra::values(p$locked)[, 1]))) # nothing locked
})

test_that("elsar_calibrate_weights runs with lock-ins end to end", {
  skip_if_not_installed("prioritizr")
  skip_if_not_installed("highs")
  fx <- calibration_fixture()
  # Two features (the representation summary needs more than one)
  yy <- terra::init(fx$features, "y")
  fx$features <- c(fx$features, (yy > 5) * 1)
  names(fx$features) <- c("feat", "feat2")
  fx$impacts <- data.frame(feature = c("feat", "feat2"), protect = 1, restore = 0.5)
  cal <- suppressWarnings(elsar_calibrate_weights(
    features = fx$features, impacts = fx$impacts,
    zones_pu = list(protect = fx$pu, restore = fx$pu), planning_units = fx$pu,
    budgets = c(protect = 10, restore = 10), lockins = list(protect = fx$lockin),
    solver = "highs", gap = 0, max_iter = 3, verbose = FALSE
  ))
  expect_s3_class(cal, "elsar_calibration")
  expect_error(elsar_calibrate_weights(
    features = fx$features, impacts = fx$impacts, zones_pu = list(protect = fx$pu, restore = fx$pu),
    planning_units = fx$pu, budgets = c(protect = 10, restore = 10),
    lockins = list(protect = fx$lockin), lockin_threshold = "a", solver = "highs"
  ), "lockin_threshold")
})

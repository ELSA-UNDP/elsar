test_that("elsar_calibrate_weights balances representation on a synthetic problem", {
  skip_if_not_installed("prioritizr")
  skip_if_not_installed("highs")

  set.seed(1)
  r0 <- terra::rast(nrows = 20, ncols = 20, xmin = 0, xmax = 20,
                    ymin = 0, ymax = 20, crs = "EPSG:3857")
  xx <- terra::init(r0, "x")
  yy <- terra::init(r0, "y")
  features <- c(
    terra::init(r0, runif),          # broad
    (xx < 7) * 1,                    # left strip
    ((xx - 10)^2 + (yy - 10)^2 < 30) * 1,  # central disc
    (yy > 16) * terra::init(r0, runif)     # sparse top band
  )
  names(features) <- c("feat_a", "feat_b", "feat_c", "feat_d")
  pu_all <- terra::ifel(!is.na(features[[1]]), 1, NA)

  impacts <- data.frame(
    feature = names(features),
    protect = c(1, 1, 1, 1),
    restore = c(0.6, 0.3, 0.8, 0.5)
  )
  zones_pu <- list(protect = pu_all, restore = pu_all)
  budgets  <- c(protect = 20, restore = 15)

  cal <- elsar_calibrate_weights(
    features = features, impacts = impacts, zones_pu = zones_pu,
    planning_units = pu_all, budgets = budgets,
    solver = "highs", gap = 0.001, freeze_patience = 2, max_iter = 40,
    verbose = FALSE
  )

  # structure
  expect_s3_class(cal, "elsar_calibration")
  expect_named(cal, c("weights", "weight_matrix", "representation", "a_max",
                      "trajectory", "converged", "iterations", "meta"))
  expect_equal(nrow(cal$weights), 4L)
  expect_setequal(cal$weights$feature, names(features))
  expect_true(all(is.finite(cal$weights$weight)))
  expect_true(all(cal$weights$weight >= 1))          # increase-only from baseline 1
  expect_true(all(cal$a_max >= 0))

  # do_nothing weight column is zero
  dn <- which(cal$meta$zones == "do_nothing")
  expect_true(all(cal$weight_matrix[, dn] == 0))

  # balance improves: best spread <= baseline spread
  baseline <- cal$trajectory$spread_new[cal$trajectory$iter == 0]
  best <- min(cal$trajectory$spread_new, na.rm = TRUE)
  expect_lt(best, baseline)                       # calibration improves balance
  expect_gte(cal$iterations, 1L)                  # ran at least one iteration
})

test_that("elsar_calibrate_weights handles a zero-total feature without aborting", {
  skip_if_not_installed("prioritizr")
  skip_if_not_installed("highs")

  set.seed(2)
  r0 <- terra::rast(nrows = 16, ncols = 16, xmin = 0, xmax = 16,
                    ymin = 0, ymax = 16, crs = "EPSG:3857")
  xx <- terra::init(r0, "x")
  features <- c(
    terra::init(r0, runif),
    (xx < 6) * 1,
    xx * 0                             # zero-total feature (undefined shortfall)
  )
  names(features) <- c("feat_a", "feat_b", "feat_zero")
  pu_all <- terra::ifel(!is.na(features[[1]]), 1, NA)

  impacts <- data.frame(
    feature = names(features),
    protect = c(1, 1, 1),
    restore = c(0.5, 0.5, 0.5)
  )
  zones_pu <- list(protect = pu_all, restore = pu_all)
  budgets  <- c(protect = 20, restore = 15)

  expect_no_error(
    cal <- elsar_calibrate_weights(
      features = features, impacts = impacts, zones_pu = zones_pu,
      planning_units = pu_all, budgets = budgets,
      solver = "highs", gap = 0.001, freeze_patience = 2, max_iter = 30,
      verbose = FALSE
    )
  )
  # the two well-defined features still calibrate; the zero-total one is not NaN-propagated
  expect_equal(nrow(cal$weights), 3L)
  expect_true(all(is.finite(cal$weights$weight)))
})

test_that("elsar_calibrate_weights validates inputs", {
  skip_if_not_installed("prioritizr")
  r0 <- terra::rast(nrows = 8, ncols = 8)
  features <- c(terra::init(r0, runif), terra::init(r0, runif))
  names(features) <- c("a", "b")
  pu <- terra::ifel(!is.na(features[[1]]), 1, NA)
  impacts <- data.frame(feature = c("a", "b"), protect = c(1, 1))

  # zone missing from impacts
  expect_error(
    elsar_calibrate_weights(features, impacts,
      zones_pu = list(protect = pu, restore = pu),
      planning_units = pu, budgets = c(protect = 10, restore = 10),
      solver = "default", verbose = FALSE),
    "impact column"
  )
  # impacts$feature mismatch
  expect_error(
    elsar_calibrate_weights(features,
      impacts = data.frame(feature = c("a", "x"), protect = c(1, 1)),
      zones_pu = list(protect = pu),
      planning_units = pu, budgets = c(protect = 10),
      solver = "default", verbose = FALSE),
    "match names\\(features\\)"
  )
})

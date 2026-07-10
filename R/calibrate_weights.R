#' Calibrate feature weights for an ELSA prioritization
#'
#' Iteratively computes a set of per-feature weights so that all features achieve
#' more even representation relative to what each could attain on its own (its
#' single-feature maximum). Under-represented features receive larger weights;
#' well-represented features receive none. This is the pre-calibration step of the
#' ELSA workflow: the resulting weights are multiplied by any user-supplied weights
#' when the deployed tool solves the prioritization.
#'
#' The routine reproduces the ELSA calibration framework exactly (it is not a new
#' objective): (1) each feature's maximum attainable representation `a_max` is
#' obtained by solving the zoned maximum-utility problem with that feature alone
#' weighted; (2) the problem is solved with equal weights to obtain each feature's
#' baseline representation and its relative shortfall
#' `delta = (a - a_max) / a_max * 100`; (3) weights are adjusted iteratively with the
#' additive, increase-only rule
#' `adj = wgt_scale - (delta - min(delta)) / (max(delta) - min(delta)) * wgt_scale`,
#' re-solving and recomputing shortfalls, until the allocation stops changing. The
#' weight set giving the most even representation (smallest spread of `delta`) is
#' returned.
#'
#' Requires the `prioritizr` package (and a supported solver, e.g. `gurobi` or
#' `highs`); both are optional dependencies.
#'
#' @param features A `SpatRaster` of feature layers, ideally normalised to `[0, 1]`.
#'   Layer names are used as the feature identifiers throughout.
#' @param impacts A `data.frame` mapping features to action zones. Must contain a
#'   `feature` column (matching `names(features)`) and one numeric column per action
#'   zone giving that feature's zone-contribution ("impact"), typically in `[0, 1.5]`.
#'   Zone column names must match `names(zones_pu)`.
#' @param zones_pu A named `list` of single-layer `SpatRaster` planning-unit masks
#'   (`1` = available, `NA` = unavailable), one per action zone. If lock-ins are to be
#'   enforced, pass masks that already exclude lock-in cells from other zones. Names
#'   define the action zones and their order.
#' @param planning_units A single-layer `SpatRaster` of all planning units in the
#'   study region (`1`/`NA`). Used as the objective budget and the denominator of
#'   area targets.
#' @param budgets A named numeric vector of per-zone area caps, each a percentage of
#'   the study-area planning-unit count. Names must match `names(zones_pu)`.
#' @param lockins Optional named `list` of `SpatRaster` lock-in areas per action zone.
#'   Used only to raise a zone's target to its locked-in coverage when the lock-in
#'   exceeds the nominal budget (feasibility floor).
#' @param feature_totals Optional named numeric vector of raw (non-impact-weighted)
#'   feature totals used as the representation denominator. Computed from `features`
#'   via `terra::global(..., "sum")` when `NULL` (the default).
#' @param wgt_scale Maximum weight increment applied per iteration (default `2`).
#' @param gap Solver optimality-gap tolerance (default `0.05`). Note: the ELSA
#'   max-utility problem is typically solved to an exact optimum at the root node, so
#'   this tolerance rarely binds.
#' @param solver Solver backend: `"gurobi"` (default), `"highs"`, or `"default"`
#'   (prioritizr's best available). `"gurobi"` requires the `gurobi` package.
#' @param threads Number of solver threads (default `4`).
#' @param time_limit Per-solve time limit in seconds (default `3600`).
#' @param solve_retries Number of times to retry a failed solve (default `2`), to
#'   survive transient solver or cloud-connection errors (e.g. a dropped connection
#'   to a Gurobi Compute Server). A short pause separates attempts.
#' @param progress_file Optional path to a CSV. When set, each iteration's
#'   trajectory row is appended as it completes, so a long-running calibration can
#'   be monitored live (e.g. by a UI polling the file). The function is otherwise
#'   side-effect-free. Any existing file at this path is overwritten.
#' @param freeze_tol Convergence threshold on the maximum per-feature change in
#'   representation between iterations (default `0.01`). When the allocation stops
#'   changing by more than this, further additive reweighting cannot move the (exactly
#'   solved) optimum.
#' @param freeze_patience Number of consecutive frozen iterations required to stop
#'   (default `3`).
#' @param max_iter Safety cap on the number of calibration iterations (default `200`).
#' @param verbose Logical; emit progress messages via [log_message()] (default `TRUE`).
#'
#' @return A named `list` of class `elsar_calibration` with elements:
#'   \describe{
#'     \item{weights}{A `tibble` with columns `feature` and `weight` (the calibrated
#'       per-feature weight).}
#'     \item{weight_matrix}{The feature x zone weight matrix actually used (the
#'       `do_nothing` column is `0`).}
#'     \item{representation}{A `tibble` with per-feature `a` (achieved representation),
#'       `a_max`, `ratio = a / a_max`, and `delta_pct` shortfall.}
#'     \item{a_max}{Named numeric vector of single-feature maximum representations.}
#'     \item{trajectory}{A `tibble` of per-iteration diagnostics (spread, max change,
#'       weight range, shortfall summary).}
#'     \item{converged}{Logical; `TRUE` if the loop stopped on the freeze rule rather
#'       than `max_iter`.}
#'     \item{iterations}{Number of calibration iterations run.}
#'     \item{meta}{A list of run metadata (solver, gap, wgt_scale, package versions).}
#'   }
#'
#' @examples
#' \dontrun{
#' cal <- elsar_calibrate_weights(
#'   features       = feature_stack,
#'   impacts        = impact_df, # cols: feature, protect, restore, manage
#'   zones_pu       = list(protect = pu_protect, restore = pu_restore, manage = pu_manage),
#'   planning_units = pu_all,
#'   budgets        = c(protect = 30, restore = 18.564, manage = 5),
#'   solver         = "gurobi"
#' )
#' cal$weights
#' }
#'
#' @export
elsar_calibrate_weights <- function(features,
                                    impacts,
                                    zones_pu,
                                    planning_units,
                                    budgets,
                                    lockins = NULL,
                                    feature_totals = NULL,
                                    wgt_scale = 2,
                                    gap = 0.05,
                                    solver = c("gurobi", "highs", "default"),
                                    threads = 4,
                                    time_limit = 3600,
                                    solve_retries = 2,
                                    progress_file = NULL,
                                    freeze_tol = 0.01,
                                    freeze_patience = 3,
                                    max_iter = 200,
                                    verbose = TRUE) {
  solver <- match.arg(solver)

  # ---- Dependency + input validation ----
  assertthat::assert_that(
    requireNamespace("prioritizr", quietly = TRUE),
    msg = "'prioritizr' is required for calibration. Install it with install.packages('prioritizr')."
  )
  if (solver == "gurobi") {
    assertthat::assert_that(
      requireNamespace("gurobi", quietly = TRUE),
      msg = "solver = 'gurobi' requires the 'gurobi' R package (from your Gurobi install). Use solver = 'highs' or 'default' otherwise."
    )
  }
  assertthat::assert_that(inherits(features, "SpatRaster"),
    msg = "'features' must be a SpatRaster."
  )
  assertthat::assert_that(inherits(planning_units, "SpatRaster") && terra::nlyr(planning_units) == 1,
    msg = "'planning_units' must be a single-layer SpatRaster."
  )
  assertthat::assert_that(is.data.frame(impacts) && "feature" %in% names(impacts),
    msg = "'impacts' must be a data.frame with a 'feature' column and one column per action zone."
  )
  assertthat::assert_that(is.list(zones_pu) && !is.null(names(zones_pu)),
    msg = "'zones_pu' must be a named list of single-layer SpatRaster zone masks."
  )
  assertthat::assert_that(is.numeric(budgets) && !is.null(names(budgets)),
    msg = "'budgets' must be a named numeric vector of per-zone area-cap percentages."
  )

  zone_names <- names(zones_pu)
  feat_names <- names(features)
  assertthat::assert_that(all(zone_names %in% names(impacts)),
    msg = "every zone in 'zones_pu' must have a matching impact column in 'impacts'."
  )
  assertthat::assert_that(all(zone_names %in% names(budgets)),
    msg = "every zone in 'zones_pu' must have a matching entry in 'budgets'."
  )
  assertthat::assert_that(setequal(impacts$feature, feat_names),
    msg = "'impacts$feature' must match names(features) exactly."
  )

  # order impacts to feature order
  impacts <- impacts[match(feat_names, impacts$feature), , drop = FALSE]

  n_feat <- terra::nlyr(features)
  # do_nothing is an explicit zero-value zone appended after the action zones
  all_zone_names <- c(zone_names, "do_nothing")
  n_zone <- length(all_zone_names)
  dn_idx <- which(all_zone_names == "do_nothing")

  if (verbose) {
    log_message("Starting weight calibration: {n_feat} features, {length(zone_names)} action zones ({paste(zone_names, collapse=', ')}) + do_nothing.")
  }

  # ---- Raw feature totals (representation denominator) ----
  if (is.null(feature_totals)) {
    feature_totals <- stats::setNames(
      terra::global(features, "sum", na.rm = TRUE)[[1]], feat_names
    )
  }
  feature_totals <- feature_totals[feat_names]

  # ---- Build the (weightless) zoned max-utility problem ----
  base_problem <- .elsar_calibration_problem(
    features       = features,
    impacts        = impacts,
    zones_pu       = zones_pu,
    planning_units = planning_units,
    budgets        = budgets,
    lockins        = lockins,
    zone_names     = zone_names,
    all_zone_names = all_zone_names,
    gap            = gap,
    solver         = solver,
    threads        = threads,
    time_limit     = time_limit
  )

  # ---- Helper closures over the built problem ----
  one_hot <- function(i) {
    w <- matrix(0, n_feat, n_zone)
    w[i, ] <- 1
    w[, dn_idx] <- 0
    w
  }
  overall_rep <- function(weight_matrix) {
    .elsar_zone_representation(base_problem, weight_matrix, feature_totals, feat_names,
      retries = solve_retries
    )
  }

  # ---- Step 1: single-feature maxima a_max ----
  if (verbose) log_message("Step 1/3: single-feature maxima...")
  a_max <- numeric(n_feat)
  for (i in seq_len(n_feat)) {
    if (verbose) log_message("  a_max feature {i}/{n_feat}: {feat_names[i]}")
    a_max[i] <- overall_rep(one_hot(i))[feat_names[i]]
  }
  names(a_max) <- feat_names

  # ---- Step 2: equal-weight baseline ----
  if (verbose) log_message("Step 2/3: equal-weight baseline...")
  wgta <- matrix(1, n_feat, n_zone)
  wgta[, dn_idx] <- 0
  util <- overall_rep(wgta)[feat_names]
  delta <- (util - a_max) / a_max * 100

  valid0 <- is.finite(delta)
  spread_prev <- max(delta[valid0]) - min(delta[valid0])
  spread_best <- spread_prev
  wgta_best <- wgta
  delta_best <- delta
  util_best <- util
  if (verbose) log_message("  baseline spread {round(spread_prev, 4)} over {sum(valid0)}/{n_feat} features.")

  # Optional live progress: append each trajectory row to progress_file as it lands.
  if (!is.null(progress_file) && file.exists(progress_file)) file.remove(progress_file)
  write_progress <- function(row) {
    if (is.null(progress_file)) {
      return(invisible())
    }
    ex <- file.exists(progress_file)
    utils::write.table(row, progress_file, sep = ",", append = ex,
      col.names = !ex, row.names = FALSE, qmethod = "double")
  }

  traj <- list(.elsar_traj_row(0L, NA_real_, spread_prev, NA_real_, delta, wgta))
  write_progress(traj[[1L]])

  # ---- Step 3: iterative calibration ----
  if (verbose) log_message("Step 3/3: iterative calibration (freeze_tol={freeze_tol}, patience={freeze_patience}, max_iter={max_iter}).")
  calib <- TRUE
  iter <- 1L
  freeze_count <- 0L
  converged <- FALSE

  while (calib) {
    # adjustments over features with a defined shortfall; undefined get none
    valid <- is.finite(delta)
    if (sum(valid) < 2) {
      if (verbose) log_message("Fewer than two features have a defined shortfall; stopping.")
      converged <- TRUE
      break
    }
    d_min <- min(delta[valid])
    d_rng <- max(delta[valid]) - d_min
    if (d_rng < 1e-10) {
      if (verbose) log_message("All features with a defined shortfall are balanced; converged.")
      converged <- TRUE
      break
    }
    adj <- wgt_scale - (delta - d_min) / d_rng * wgt_scale
    adj[!is.finite(adj)] <- 0
    adj_mat <- matrix(adj, n_feat, n_zone)

    wgtb <- wgta + adj_mat
    wgtb[, dn_idx] <- 0

    util_new <- overall_rep(wgtb)[feat_names]
    delta_new <- (util_new - a_max) / a_max * 100
    max_change <- max(abs(util_new - util))

    valid_new <- is.finite(delta_new)
    spread_new <- max(delta_new[valid_new]) - min(delta_new[valid_new])

    traj[[length(traj) + 1L]] <- .elsar_traj_row(iter, spread_prev, spread_new, max_change, delta_new, wgtb)
    write_progress(traj[[length(traj)]])

    # keep the best-seen configuration (lowest spread)
    if (is.finite(spread_new) && spread_new < spread_best - 1e-6) {
      wgta_best <- wgtb
      delta_best <- delta_new
      util_best <- util_new
      spread_best <- spread_new
    }

    # freeze stop: allocation no longer changing
    if (is.finite(max_change) && max_change < freeze_tol) {
      freeze_count <- freeze_count + 1L
    } else {
      freeze_count <- 0L
    }

    # accept and continue exploring
    wgta <- wgtb
    delta <- delta_new
    util <- util_new
    spread_prev <- spread_new
    iter <- iter + 1L

    if (verbose) {
      log_message("  iter {iter - 1}: spread {round(spread_new, 4)} | max change {round(max_change, 4)} | frozen {freeze_count}/{freeze_patience} | best {round(spread_best, 4)}")
    }

    if (freeze_count >= freeze_patience) {
      if (verbose) log_message("Stopping: allocation frozen for {freeze_patience} consecutive iterations.")
      converged <- TRUE
      calib <- FALSE
    } else if (iter > max_iter) {
      if (verbose) log_message("Stopping: reached max_iter ({max_iter}) without a frozen allocation.")
      calib <- FALSE
    }
  }

  if (verbose) log_message("Calibration complete after {iter - 1} iterations (best spread {round(spread_best, 4)}).")

  # ---- Assemble result (best-seen) ----
  weights_vec <- as.numeric(wgta_best[, 1])
  a_vec <- as.numeric(util_best[feat_names])
  amax_vec <- as.numeric(a_max[feat_names])
  result <- list(
    weights = tibble::tibble(feature = feat_names, weight = weights_vec),
    weight_matrix = wgta_best,
    representation = tibble::tibble(
      feature   = feat_names,
      a         = a_vec,
      a_max     = amax_vec,
      ratio     = a_vec / amax_vec,
      delta_pct = as.numeric(delta_best)
    ),
    a_max = a_max,
    trajectory = dplyr::bind_rows(traj),
    converged = converged,
    iterations = iter - 1L,
    meta = list(
      solver = solver, gap = gap, wgt_scale = wgt_scale,
      freeze_tol = freeze_tol, freeze_patience = freeze_patience,
      zones = all_zone_names,
      prioritizr_version = as.character(getNamespaceVersion("prioritizr"))
    )
  )
  class(result) <- c("elsar_calibration", "list")
  return(result)
}


#' Build the ELSA zoned maximum-utility problem for calibration
#'
#' Internal. Constructs the (weightless) `prioritizr` problem used by calibration:
#' impacts baked into per-zone feature rasters, a `do_nothing` zone, a maximum-utility
#' objective, per-zone area-cap linear constraints, and mandatory allocation. Feature
#' weights are attached per solve by the calibration loop.
#'
#' @keywords internal
#' @noRd
.elsar_calibration_problem <- function(features, impacts, zones_pu, planning_units,
                                       budgets, lockins, zone_names, all_zone_names,
                                       gap, solver, threads, time_limit) {
  feat_names <- names(features)

  # zone features with impacts baked in; do_nothing is zeroed
  zone_features <- lapply(zone_names, function(z) features * impacts[[z]])
  names(zone_features) <- zone_names
  zone_features[["do_nothing"]] <- features * 0

  # per-zone planning-unit stack (matching all_zone_names order)
  pu_stack <- c(zones_pu, list(do_nothing = planning_units * 0))
  pu_stack <- terra::rast(pu_stack[all_zone_names])

  zones_obj <- do.call(
    prioritizr::zones,
    c(zone_features[all_zone_names], list(feature_names = feat_names))
  )

  problem <- prioritizr::problem(pu_stack, zones_obj, run_checks = FALSE)

  # solver
  problem <- switch(solver,
    gurobi = prioritizr::add_gurobi_solver(problem,
      gap = gap, threads = threads,
      time_limit = time_limit, verbose = FALSE
    ),
    highs = prioritizr::add_highs_solver(problem,
      gap = gap,
      time_limit = time_limit, verbose = FALSE
    ),
    default = prioritizr::add_default_solver(problem, gap = gap, verbose = FALSE)
  )

  # objective: maximise utility, budget = total PU count
  problem <- prioritizr::add_max_utility_objective(
    problem, terra::global(planning_units, "sum", na.rm = TRUE)[[1]]
  )

  # per-zone targets (raise to lock-in coverage where a lock-in exceeds the budget)
  targets <- stats::setNames(as.numeric(budgets[zone_names]), zone_names)
  if (!is.null(lockins) && length(lockins) > 0) {
    for (z in zone_names) {
      if (!is.null(lockins[[z]])) {
        cov <- get_coverage(lockins[[z]], planning_units)
        if (is.finite(cov) && cov > targets[[z]]) targets[[z]] <- cov
      }
    }
  }

  # per-zone area-cap constraints: count only cells assigned to that zone
  for (z in zone_names) {
    dat <- lapply(all_zone_names, function(other) {
      if (identical(other, z)) pu_stack[[z]] else pu_stack[[other]] * 0
    })
    problem <- prioritizr::add_linear_constraints(
      problem,
      threshold = get_target(planning_units, targets[[z]]),
      sense = "<=",
      data = terra::rast(dat)
    )
  }

  problem <- prioritizr::add_mandatory_allocation_constraints(problem)
  return(problem)
}


#' Solve with a weight matrix and return per-feature overall representation
#'
#' Internal. Numerator is prioritizr's impact-weighted `absolute_held`; denominator is
#' the raw feature total (matching the deployed ELSA representation metric). Returns a
#' named numeric vector of overall (summed across action zones) representation.
#'
#' @keywords internal
#' @noRd
.elsar_zone_representation <- function(base_problem, weight_matrix, feature_totals,
                                       feat_names, retries = 2) {
  # The do_nothing zone holds an all-zero coefficient block, over which prioritizr
  # harmlessly calls min() on an empty slot ("no non-missing arguments to min").
  # Muffle only that specific, benign warning.
  do_solve <- function() {
    withCallingHandlers(
      base_problem %>%
        prioritizr::add_feature_weights(as.matrix(weight_matrix)) %>%
        prioritizr::solve.ConservationProblem(force = TRUE, run_checks = FALSE),
      warning = function(w) {
        if (grepl("no non-missing arguments to min", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  # Retry on transient solver / cloud-connection errors.
  attempt <- 0L
  repeat {
    sol <- tryCatch(do_solve(), error = function(e) e)
    if (!inherits(sol, "error")) break
    if (attempt >= retries) stop(sol)
    attempt <- attempt + 1L
    log_message("Solve failed ({conditionMessage(sol)}); retry {attempt}/{retries}...")
    Sys.sleep(5)
  }

  rep_summary <- prioritizr::eval_feature_representation_summary(base_problem, sol)

  # Overall representation = sum of per-zone relative_held across action zones. The
  # do_nothing zone holds zero of every feature (relative_held = 0), so summing over
  # all non-"overall" zones is equivalent to summing over the action zones only.
  overall <- rep_summary %>%
    dplyr::filter(.data$summary != "overall") %>%
    dplyr::mutate(
      total_amount = feature_totals[.data$feature],
      relative_held = dplyr::if_else(.data$total_amount > 0,
        .data$absolute_held / .data$total_amount, 0.0
      )
    ) %>%
    dplyr::group_by(.data$feature) %>%
    dplyr::summarise(overall = sum(.data$relative_held, na.rm = TRUE), .groups = "drop")

  stats::setNames(overall$overall, overall$feature)[feat_names]
}


#' Build one trajectory-log row for the calibration loop
#'
#' @keywords internal
#' @noRd
.elsar_traj_row <- function(iter, spread_prev, spread_new, max_change, delta, wmat) {
  finite <- is.finite(delta)
  tibble::tibble(
    iter                = iter,
    spread_prev         = spread_prev,
    spread_new          = spread_new,
    delta_spread        = spread_new - spread_prev,
    max_abs_util_change = max_change,
    min_delta_pct       = if (any(finite)) min(delta[finite]) else NA_real_,
    max_delta_pct       = if (any(finite)) max(delta[finite]) else NA_real_,
    mean_delta_pct      = if (any(finite)) mean(delta[finite]) else NA_real_,
    weight_max          = max(wmat),
    weight_mean         = mean(wmat)
  )
}

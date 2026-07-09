#' Diagnose your elsar setup
#'
#' Reports, tier by tier, what elsar needs and whether it is present on your
#' machine, with the exact command to fix anything that is missing. It is safe
#' to run any time: it makes no changes, needs no credentials, and never errors
#' out - a failed probe is reported as a finding, not thrown.
#'
#' elsar has three tiers with different setup requirements (see the package's
#' `SUPPORT.md`):
#'
#' * **Core** - boundaries, planning units, normalisation, zone builders from
#'   local rasters, and plotting. Needs only the geospatial R stack (GDAL, GEOS,
#'   PROJ, installed with `sf`/`terra`). Works offline.
#' * **Protected areas** - `make_protected_areas(from_wdpca = TRUE)`, which needs
#'   `wdpar`, a Chromium browser, and the `libarchive` system library.
#' * **Earth Engine** - the `download_*` functions, which need `reticulate`,
#'   `googledrive`, a conda/Python environment with `earthengine-api`, and a
#'   Google Earth Engine account with an approved Cloud project.
#'
#' If you are filing a bug report about the `download_*` functions, paste the
#' output of `elsar_check_setup()` into the report.
#'
#' @param tiers Character vector naming which tiers to check. Any of
#'   `"core"`, `"protected_areas"`, `"earth_engine"`. Defaults to all three.
#' @param quiet Logical. If `TRUE`, suppress the printed report and only return
#'   the results invisibly. Default `FALSE`.
#'
#' @return Invisibly, a `data.frame` with one row per check and the columns
#'   `tier`, `item`, `status` (`"ok"`, `"missing"`, `"warn"`, or `"info"`),
#'   `detail`, and `fix`. Returned invisibly so it can be inspected
#'   programmatically while the human-readable report prints to the console.
#'
#' @examples
#' # Safe to run anywhere - no changes, no credentials required.
#' elsar_check_setup()
#'
#' # Just the core geospatial stack:
#' elsar_check_setup(tiers = "core")
#'
#' # Capture the results without printing:
#' res <- elsar_check_setup(quiet = TRUE)
#' res[res$status != "ok", ]
#'
#' @export
elsar_check_setup <- function(tiers = c("core", "protected_areas", "earth_engine"),
                              quiet = FALSE) {
  tiers <- match.arg(tiers, several.ok = TRUE)

  records <- list()
  if ("core" %in% tiers) {
    records <- c(records, check_tier_core())
  }
  if ("protected_areas" %in% tiers) {
    records <- c(records, check_tier_protected_areas())
  }
  if ("earth_engine" %in% tiers) {
    records <- c(records, check_tier_earth_engine())
  }

  results <- do.call(rbind, c(records, list(stringsAsFactors = FALSE)))
  rownames(results) <- NULL

  if (!quiet) {
    print_setup_report(results)
  }

  invisible(results)
}

# ---- record helper ---------------------------------------------------------

#' Build a single setup-check record
#'
#' @param tier,item,status,detail,fix Single strings describing one check.
#'   `status` is one of `"ok"`, `"missing"`, `"warn"`, `"info"`.
#' @return A one-row `data.frame`.
#' @keywords internal
setup_record <- function(tier, item, status, detail = "", fix = "") {
  data.frame(
    tier = tier,
    item = item,
    status = status,
    detail = unname(detail),
    fix = unname(fix),
    stringsAsFactors = FALSE
  )
}

# Safely probe something that might error; return `default` if it does.
safe_try <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default, warning = function(w) default)
}

# ---- Tier A: core ----------------------------------------------------------

#' @keywords internal
check_tier_core <- function() {
  tier <- "Core"
  recs <- list()

  # R version
  recs <- c(recs, list(setup_record(
    tier, "R", "ok",
    detail = paste0("R ", getRversion())
  )))

  # sf + the geospatial system libraries it bundles
  if (requireNamespace("sf", quietly = TRUE)) {
    vers <- safe_try(sf::sf_extSoftVersion())
    lib_note <- if (!is.null(vers)) {
      sprintf("GDAL %s, GEOS %s, PROJ %s",
              vers[["GDAL"]], vers[["GEOS"]], vers[["PROJ"]])
    } else {
      "system library versions unavailable"
    }
    recs <- c(recs, list(setup_record(
      tier, "sf", "ok",
      detail = sprintf("sf %s (%s)", utils::packageVersion("sf"), lib_note)
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "sf", "missing",
      detail = "sf is not installed",
      fix = 'install.packages("sf")'
    )))
  }

  # terra
  if (requireNamespace("terra", quietly = TRUE)) {
    gdal_v <- safe_try(terra::gdal(), default = "")
    recs <- c(recs, list(setup_record(
      tier, "terra", "ok",
      detail = sprintf("terra %s%s", utils::packageVersion("terra"),
                       if (nzchar(gdal_v)) sprintf(" (GDAL %s)", gdal_v) else "")
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "terra", "missing",
      detail = "terra is not installed",
      fix = 'install.packages("terra")'
    )))
  }

  # elsar itself
  recs <- c(recs, list(setup_record(
    tier, "elsar", "ok",
    detail = paste0("elsar ", safe_try(utils::packageVersion("elsar"), "dev"))
  )))

  recs
}

# ---- Tier B: protected areas -----------------------------------------------

#' @keywords internal
check_tier_protected_areas <- function() {
  tier <- "Protected areas"
  recs <- list()

  # wdpar
  if (requireNamespace("wdpar", quietly = TRUE)) {
    recs <- c(recs, list(setup_record(
      tier, "wdpar", "ok",
      detail = paste0("wdpar ", utils::packageVersion("wdpar"))
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "wdpar", "missing",
      detail = "wdpar is not installed",
      fix = 'install.packages("wdpar")'
    )))
  }

  # A Chromium-based browser (via chromote)
  if (requireNamespace("chromote", quietly = TRUE)) {
    chrome <- safe_try(chromote::find_chrome())
    if (!is.null(chrome) && nzchar(chrome)) {
      recs <- c(recs, list(setup_record(
        tier, "Chromium browser", "ok",
        detail = chrome
      )))
    } else {
      recs <- c(recs, list(setup_record(
        tier, "Chromium browser", "missing",
        detail = "no Chrome/Chromium browser found",
        fix = "install Google Chrome or Chromium (used by wdpar to fetch data)"
      )))
    }
  } else {
    recs <- c(recs, list(setup_record(
      tier, "Chromium browser", "warn",
      detail = "chromote not installed, cannot check for a browser",
      fix = 'install.packages("chromote"), then install Chrome/Chromium'
    )))
  }

  # libarchive (needed to build/use the archive package that wdpar depends on)
  if (requireNamespace("archive", quietly = TRUE)) {
    recs <- c(recs, list(setup_record(
      tier, "libarchive", "ok",
      detail = "archive package present (libarchive available)"
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "libarchive", "missing",
      detail = "archive package not installed",
      fix = paste0("install the libarchive system library ",
                   "(libarchive-dev / libarchive-devel), then ",
                   'install.packages("archive")')
    )))
  }

  recs
}

# ---- Tier C: Earth Engine --------------------------------------------------

#' @keywords internal
check_tier_earth_engine <- function() {
  tier <- "Earth Engine"
  recs <- list()

  # reticulate
  have_reticulate <- requireNamespace("reticulate", quietly = TRUE)
  if (have_reticulate) {
    recs <- c(recs, list(setup_record(
      tier, "reticulate", "ok",
      detail = paste0("reticulate ", utils::packageVersion("reticulate"))
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "reticulate", "missing",
      detail = "reticulate is not installed",
      fix = 'install.packages("reticulate")'
    )))
  }

  # conda
  conda_bin <- NULL
  if (have_reticulate) {
    conda_bin <- safe_try(reticulate::conda_binary())
  }
  conda_base <- safe_try(find_conda_base())
  if (!is.null(conda_bin) && nzchar(conda_bin)) {
    recs <- c(recs, list(setup_record(
      tier, "conda", "ok",
      detail = conda_bin
    )))
  } else if (!is.null(conda_base)) {
    recs <- c(recs, list(setup_record(
      tier, "conda", "ok",
      detail = conda_base
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "conda", "missing",
      detail = "no conda installation found",
      fix = "run elsar_setup_gee() to install a conda environment for you"
    )))
  }

  # An Earth Engine conda environment (elsar's own env first, then the other
  # names initialize_earthengine() recognises). Detect it two ways - via
  # reticulate::conda_list() and by scanning <conda_base>/envs directly - because
  # conda_list() can miss envs when RETICULATE_PYTHON points elsewhere, while the
  # filesystem scan is what the runtime actually relies on.
  ee_env_names <- c(elsar_gee_env(), "ee_compat", "ee", "gee", "earthengine", "earth-engine")
  found_env <- NULL
  env_list <- if (have_reticulate) safe_try(reticulate::conda_list()) else NULL
  if (!is.null(env_list) && nrow(env_list) > 0) {
    hit <- env_list$name %in% ee_env_names
    if (any(hit)) found_env <- env_list$name[hit][1]
  }
  if (is.null(found_env) && !is.null(conda_base)) {
    for (nm in ee_env_names) {
      if (dir.exists(file.path(conda_base, "envs", nm))) {
        found_env <- nm
        break
      }
    }
  }
  if (!is.null(found_env)) {
    recs <- c(recs, list(setup_record(
      tier, "Earth Engine conda env", "ok",
      detail = sprintf("conda env '%s'", found_env)
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "Earth Engine conda env", "missing",
      detail = paste0("no conda env named one of: ",
                      paste(ee_env_names, collapse = ", ")),
      fix = "run elsar_setup_gee() to create it and install earthengine-api"
    )))
  }

  # googledrive
  if (requireNamespace("googledrive", quietly = TRUE)) {
    recs <- c(recs, list(setup_record(
      tier, "googledrive", "ok",
      detail = paste0("googledrive ", utils::packageVersion("googledrive"))
    )))
    # Is a Drive token already cached?
    has_token <- isTRUE(safe_try(googledrive::drive_has_token(), default = FALSE))
    recs <- c(recs, list(setup_record(
      tier, "Google Drive auth", if (has_token) "ok" else "info",
      detail = if (has_token) "Drive token cached" else "no Drive token cached yet",
      fix = if (has_token) "" else
        "you'll be prompted to authenticate in the browser on first use"
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "googledrive", "missing",
      detail = "googledrive is not installed",
      fix = 'install.packages("googledrive")'
    )))
  }

  # Earth Engine credentials. The Python earthengine-api and rappdirs disagree
  # on the path across platforms, so check both candidates and report whichever
  # exists.
  cred_candidates <- unique(c(
    file.path(safe_try(rappdirs::user_config_dir("earthengine"), default = ""),
              "credentials"),
    file.path(path.expand("~"), ".config", "earthengine", "credentials")
  ))
  cred_candidates <- cred_candidates[nzchar(dirname(cred_candidates))]
  cred_found <- cred_candidates[file.exists(cred_candidates)]
  if (length(cred_found) > 0) {
    recs <- c(recs, list(setup_record(
      tier, "Earth Engine auth", "ok",
      detail = cred_found[1]
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "Earth Engine auth", "info",
      detail = "no Earth Engine credentials found yet",
      fix = "run elsar_setup_gee(), or authenticate on first use of a download_* function"
    )))
  }

  # gdalsrsinfo CLI (needed to convert CRS to the WKT1 that GEE requires)
  gdalsrsinfo <- safe_try(Sys.which("gdalsrsinfo"), default = "")
  if (nzchar(gdalsrsinfo)) {
    recs <- c(recs, list(setup_record(
      tier, "gdalsrsinfo", "ok",
      detail = gdalsrsinfo
    )))
  } else {
    recs <- c(recs, list(setup_record(
      tier, "gdalsrsinfo", "warn",
      detail = "gdalsrsinfo command-line tool not found",
      fix = paste0("install the GDAL command-line tools; without it, GEE ",
                   "exports fall back to WKT2 and may fail")
    )))
  }

  recs
}

# ---- reporting -------------------------------------------------------------

#' Print a human-readable setup report
#'
#' @param results The `data.frame` produced by [elsar_check_setup()].
#' @return `invisible(NULL)`, called for its printed output.
#' @keywords internal
print_setup_report <- function(results) {
  tag <- c(ok = "[ ok ]", missing = "[MISS]", warn = "[warn]", info = "[ .. ]")

  cat("\nelsar setup check\n")
  cat("=================\n")

  for (tr in unique(results$tier)) {
    block <- results[results$tier == tr, , drop = FALSE]
    n_bad <- sum(block$status %in% c("missing", "warn"))
    status_word <- if (n_bad == 0) "ready" else
      sprintf("%d item%s need%s attention", n_bad,
              if (n_bad == 1) "" else "s", if (n_bad == 1) "s" else "")
    cat(sprintf("\n%s - %s\n", tr, status_word))

    for (i in seq_len(nrow(block))) {
      row <- block[i, ]
      cat(sprintf("  %s %-24s %s\n",
                  tag[[row$status]], row$item, row$detail))
      if (row$status != "ok" && nzchar(row$fix)) {
        cat(sprintf("         -> %s\n", row$fix))
      }
    }
  }

  n_missing <- sum(results$status == "missing")
  n_warn <- sum(results$status == "warn")
  cat("\n")
  if (n_missing == 0 && n_warn == 0) {
    cat("All checked items are present.\n\n")
  } else {
    cat(sprintf("%d missing, %d warning(s). See the '->' lines above for fixes.\n\n",
                n_missing, n_warn))
  }

  invisible(NULL)
}

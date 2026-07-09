#' Set up the Earth Engine environment for elsar
#'
#' One-call bootstrap for the Google Earth Engine (GEE) workflow (the
#' `download_*` functions). It:
#'
#' 1. finds a conda installation, and - if none exists and you allow it -
#'    installs a self-contained miniconda via `reticulate::install_miniconda()`;
#' 2. creates a **persistent** conda environment ([elsar_gee_env()], default
#'    `"elsar_ee"`) with `earthengine-api` installed, reusing it if it already
#'    exists so the large download happens only once; and
#' 3. optionally walks you through Earth Engine and Google Drive authentication.
#'
#' Run this once, ahead of time, rather than letting the first `download_*` call
#' build the environment mid-analysis. Afterwards, [elsar_check_setup()] should
#' report the Earth Engine tier as ready.
#'
#' You still need your **own** Google Earth Engine account and an approved Cloud
#' project - this function configures your machine, not your Google access. See
#' the package's `SUPPORT.md` for what that entails.
#'
#' @param python_version Character. Python version for a newly created
#'   environment. Default `"3.12"`.
#' @param install_conda Logical. If no conda installation is found, may this
#'   function install miniconda for you (via `reticulate::install_miniconda()`)?
#'   Defaults to `TRUE` in an interactive session and `FALSE` otherwise, so an
#'   unattended run never silently downloads a conda installer.
#' @param authenticate Logical. Run the interactive Earth Engine and Google
#'   Drive authentication flows after the environment is ready? Defaults to
#'   `TRUE` in an interactive session, `FALSE` otherwise (the flows open a
#'   browser and cannot complete unattended). See `authenticate_drive`.
#' @param authenticate_drive Logical. Whether the authentication step also
#'   covers Google Drive (used to transfer GEE exports). Default `TRUE`. Ignored
#'   when `authenticate` is `FALSE`.
#' @param gee_project Character or `NULL`. If supplied, the Earth Engine session
#'   is initialised against this Cloud project ID as a final check that
#'   everything works. Default `NULL` (authenticate only).
#'
#' @return Invisibly, a list with `env` (the environment name), `conda` (the
#'   conda base directory), and `authenticated` (logical).
#'
#' @examples
#' \dontrun{
#' # Full guided setup, including authentication (interactive session):
#' elsar_setup_gee()
#'
#' # Just build the environment now; authenticate later on first use:
#' elsar_setup_gee(authenticate = FALSE)
#'
#' # Build and verify against a specific Cloud project:
#' elsar_setup_gee(gee_project = "my-ee-project")
#' }
#'
#' @seealso [elsar_check_setup()] to verify the result.
#' @export
elsar_setup_gee <- function(python_version = "3.12",
                            install_conda = interactive(),
                            authenticate = interactive(),
                            authenticate_drive = TRUE,
                            gee_project = NULL) {
  # rlang::is_bool() (unlike assertthat::is.flag()) rejects NA as well as
  # non-logical / non-scalar input.
  assertthat::assert_that(assertthat::is.string(python_version))
  assertthat::assert_that(rlang::is_bool(install_conda))
  assertthat::assert_that(rlang::is_bool(authenticate))
  assertthat::assert_that(rlang::is_bool(authenticate_drive))
  assertthat::assert_that(is.null(gee_project) || assertthat::is.string(gee_project))

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop('Package "reticulate" is required. Install it with install.packages("reticulate").',
         call. = FALSE)
  }

  # 1. Ensure a conda installation exists.
  conda_base <- resolve_conda(install = install_conda)

  # 2. Create or reuse the persistent Earth Engine environment.
  env_name <- create_gee_conda_env(python_version = python_version)

  # 3. Optionally authenticate.
  authenticated <- FALSE
  if (isTRUE(authenticate)) {
    authenticated <- authenticate_gee(
      gee_project = gee_project,
      include_drive = authenticate_drive
    )
  } else {
    log_message(paste0(
      "Environment ready. Skipping authentication; you'll be prompted in the ",
      "browser on first use of a download_* function (or run ",
      "elsar_setup_gee(authenticate = TRUE) now)."
    ))
  }

  log_message("elsar Earth Engine setup complete. Run elsar_check_setup() to confirm.")
  invisible(list(env = env_name, conda = conda_base, authenticated = authenticated))
}

#' Find a conda installation, optionally installing one
#'
#' @param install Logical. If no conda is found and `install` is `TRUE`, install
#'   miniconda via `reticulate::install_miniconda()`.
#' @return Path to a conda base directory. Errors (with actionable guidance) if
#'   none is found and `install` is `FALSE`, or if installation fails.
#' @keywords internal
resolve_conda <- function(install = interactive()) {
  base <- find_conda_base()
  if (!is.null(base)) {
    log_message("Found conda installation: {base}")
    return(base)
  }

  if (!isTRUE(install)) {
    stop(
      paste0(
        "No conda installation found.\n",
        "  - In an interactive session, run elsar_setup_gee() and it can ",
        "install miniconda for you, or\n",
        "  - call elsar_setup_gee(install_conda = TRUE), or\n",
        "  - install miniconda/anaconda yourself, then re-run."
      ),
      call. = FALSE
    )
  }

  log_message("No conda found. Installing miniconda via reticulate (one-off, ~400 MB)...")
  reticulate::install_miniconda()

  base <- find_conda_base()
  if (is.null(base)) {
    stop(
      paste0("Installed miniconda but could not locate it afterwards. ",
             "Restart R and try elsar_setup_gee() again."),
      call. = FALSE
    )
  }
  log_message("Miniconda installed at: {base}")
  base
}

#' Authenticate to Earth Engine (and optionally Google Drive)
#'
#' Runs the interactive OAuth flows. Assumes the conda environment is already
#' active (via [create_gee_conda_env()]).
#'
#' @param gee_project Character or `NULL`. If supplied, initialise Earth Engine
#'   against this Cloud project as a check.
#' @param include_drive Logical. Also authenticate Google Drive.
#' @return `TRUE` if authentication ran without error.
#' @keywords internal
authenticate_gee <- function(gee_project = NULL, include_drive = TRUE) {
  log_message("Authenticating Earth Engine (a browser window may open)...")
  ee <- reticulate::import("ee")

  if (!any(file.exists(earthengine_cred_paths()))) {
    ee$Authenticate()
  } else {
    log_message("Existing Earth Engine credentials found; skipping re-auth.")
  }

  if (!is.null(gee_project)) {
    log_message("Initialising Earth Engine against project '{gee_project}'...")
    ee$Initialize(project = gee_project)
    log_message("Earth Engine initialised successfully.")
  }

  if (isTRUE(include_drive)) {
    if (requireNamespace("googledrive", quietly = TRUE)) {
      if (!isTRUE(tryCatch(googledrive::drive_has_token(), error = function(e) FALSE))) {
        log_message("Authenticating Google Drive (a browser window may open)...")
        googledrive::drive_auth()
      } else {
        log_message("Google Drive token already cached; skipping.")
      }
    } else {
      log_message('Skipping Drive auth: package "googledrive" is not installed.')
    }
  }

  TRUE
}

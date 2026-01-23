# GEE Download Helpers
#
# This file contains functions for downloading raster data from Google Earth Engine (GEE)
# and processing it into Cloud-Optimized GeoTIFFs (COGs). The functions handle authentication,
# export management, file downloading, and data processing.

#' Create temporary conda environment for GEE
#'
#' Creates a temporary conda environment with earthengine-api installed.
#' Handles conda path detection and environment activation.
#'
#' @return Character. Name of the created environment.
#' @keywords internal
create_gee_conda_env <- function() {
  temp_env <- paste0("gee_temp_env_", Sys.getpid())
  conda_base <- find_conda_base()

  if (is.null(conda_base)) {
    stop("Could not find conda installation. Please install miniconda or anaconda.", call. = FALSE)
  }

  # Find conda/mamba executable
  is_windows <- .Platform$OS.type == "windows"
  conda_exe <- if (is_windows) {
    # Windows conda locations
    candidates <- c(
      file.path(conda_base, "Scripts", "mamba.exe"),
      file.path(conda_base, "Scripts", "conda.exe"),
      file.path(conda_base, "condabin", "mamba.bat"),
      file.path(conda_base, "condabin", "conda.bat")
    )
    found <- candidates[file.exists(candidates)]
    if (length(found) > 0) found[1] else NULL
  } else {
    # Unix conda locations
    candidates <- c(
      file.path(conda_base, "bin", "mamba"),
      file.path(conda_base, "bin", "conda")
    )
    found <- candidates[file.exists(candidates)]
    if (length(found) > 0) found[1] else NULL
  }

  if (is.null(conda_exe)) {
    stop("Could not find conda or mamba executable in conda installation.", call. = FALSE)
  }

  log_message("Using conda/mamba: {conda_exe}")
  log_message("Creating conda environment '{temp_env}' in {conda_base}...")

  # Create environment using direct system call to avoid reticulate's env caching issues
  create_cmd <- glue::glue('"{conda_exe}" create --yes --name {temp_env} python=3.12 earthengine-api -c conda-forge --quiet')
  log_message("Running: {create_cmd}")

  result <- system(create_cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (result != 0) {
    stop(glue::glue("Failed to create conda environment. Exit code: {result}"), call. = FALSE)
  }

  # Wait briefly for filesystem to sync
  Sys.sleep(2)

  # Check if the env directory was actually created
  env_dir <- file.path(conda_base, "envs", temp_env)
  if (!dir.exists(env_dir)) {
    log_message("Environment directory not found at expected location: {env_dir}")
    existing_envs <- list.dirs(file.path(conda_base, "envs"), full.names = FALSE, recursive = FALSE)
    log_message("Existing envs in {conda_base}/envs: {paste(existing_envs, collapse=', ')}")
    stop(glue::glue("Conda environment was not created at {env_dir}"), call. = FALSE)
  }

  # Find Python executable
  env_python <- find_env_python(conda_base, temp_env)

  if (is.null(env_python)) {
    env_contents <- list.files(env_dir, recursive = FALSE)
    log_message("Contents of {env_dir}: {paste(env_contents, collapse=', ')}")
    stop(glue::glue("Failed to find Python in new environment at: {env_dir}"), call. = FALSE)
  }

  log_message("Using Python at: {env_python}")
  reticulate::use_python(env_python, required = TRUE)
  log_message("Temporary Conda environment created: {temp_env}")

  temp_env
}


#' Find conda base directory
#'
#' Searches common locations for conda/miniconda/anaconda installations.
#' Works on Windows, macOS, and Linux.
#'
#' @return Path to conda base directory, or NULL if not found
#' @keywords internal
find_conda_base <- function() {
  is_windows <- .Platform$OS.type == "windows"

  if (is_windows) {
    # Windows conda locations
    userprofile <- Sys.getenv("USERPROFILE")
    localappdata <- Sys.getenv("LOCALAPPDATA")
    conda_candidates <- c(
      file.path(userprofile, "miniconda3"),
      file.path(userprofile, "anaconda3"),
      file.path(userprofile, "miniforge3"),
      file.path(localappdata, "miniconda3"),
      file.path(localappdata, "anaconda3"),
      "C:/miniconda3",
      "C:/anaconda3",
      "C:/ProgramData/miniconda3",
      "C:/ProgramData/anaconda3"
    )
  } else {
    # Unix-like (Linux/macOS) conda locations
    home <- Sys.getenv("HOME")
    conda_candidates <- c(
      file.path(home, "miniconda3"),
      file.path(home, "anaconda3"),
      file.path(home, "miniforge3"),
      file.path(home, ".conda"),
      "/opt/conda",
      "/opt/miniconda3",
      "/opt/anaconda3"
    )
  }

  for (cand in conda_candidates) {
    if (dir.exists(file.path(cand, "envs"))) {
      return(cand)
    }
  }

  NULL
}


#' Convert CRS to WKT1 format for GEE
#'
#' GEE requires CRS in WKT1 (OGC) format, not WKT2 or PROJ4 strings.
#' Modern GDAL/PROJ versions output WKT2 by default, which GEE cannot parse.
#' This helper extracts the CRS and converts it to WKT1 format using gdalsrsinfo.
#'
#' @param x A SpatRaster, SpatVector, or sf object
#' @return Character string with WKT1 CRS definition
#' @keywords internal
get_crs_wkt <- function(x) {
  # Extract WKT (will be WKT2 format from modern GDAL/PROJ)
  if (inherits(x, c("SpatRaster", "SpatVector"))) {
    wkt2 <- terra::crs(x, describe = FALSE)
  } else if (inherits(x, c("sf", "sfc"))) {
    wkt2 <- sf::st_crs(x)$wkt
  } else {
    stop("Cannot extract CRS from object of class: ", class(x)[1])
  }

  if (is.null(wkt2) || !nzchar(wkt2)) {
    stop("Object has no CRS defined")
  }

  # Convert WKT2 to WKT1 using gdalsrsinfo
  # GEE only accepts WKT1 (OGC) format, not the newer WKT2 format
  tmp_file <- tempfile(fileext = ".wkt")
  on.exit(unlink(tmp_file), add = TRUE)
  writeLines(wkt2, tmp_file)

  # Check if gdalsrsinfo is available
  gdalsrsinfo_path <- Sys.which("gdalsrsinfo")
  if (!nzchar(gdalsrsinfo_path)) {
    # Fall back to WKT2 and hope GEE accepts it
    log_message("Warning: gdalsrsinfo not found, using WKT2 format (may fail in GEE)")
    return(wkt2)
  }

  # Convert to WKT1 format
  wkt1 <- tryCatch({
    result <- system2("gdalsrsinfo", args = c("-o", "wkt1", tmp_file),
                      stdout = TRUE, stderr = TRUE)
    if (length(result) > 0 && !any(grepl("^ERROR", result))) {
      paste(result, collapse = "\n")
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(wkt1) || !nzchar(wkt1)) {
    log_message("Warning: Failed to convert CRS to WKT1, using WKT2 format")
    return(wkt2)
  }

  wkt1
}


#' Find Python executable in conda environment
#'
#' Locates the Python executable for a given conda environment.
#' Handles differences between Windows and Unix paths.
#'
#' @param conda_base Path to conda base directory
#' @param env_name Name of the conda environment
#' @return Path to Python executable, or NULL if not found
#' @keywords internal
find_env_python <- function(conda_base, env_name) {
  is_windows <- .Platform$OS.type == "windows"

  if (is_windows) {
    candidates <- c(
      file.path(conda_base, "envs", env_name, "python.exe"),
      file.path(conda_base, "envs", env_name, "Scripts", "python.exe")
    )
  } else {
    candidates <- c(
      file.path(conda_base, "envs", env_name, "bin", "python"),
      file.path(conda_base, "envs", env_name, "bin", "python3")
    )
  }

  for (cand in candidates) {
    if (file.exists(cand)) {
      return(cand)
    }
  }

  NULL
}


#' Safely get info from GEE objects (Windows compatibility)
#'
#' Wraps getInfo() calls to handle Python integer overflow on Windows.
#' On Windows, C long is 32-bit even on 64-bit systems, causing overflow
#' for integers > 2^31-1 (e.g., timestamps, large counts).
#'
#' @param gee_obj A GEE object with a getInfo() method
#' @return The result of getInfo(), with large integers safely converted
#' @keywords internal
safe_gee_getinfo <- function(gee_obj) {
  tryCatch({
    gee_obj$getInfo()
  }, error = function(e) {
    if (grepl("too large to convert to C long", e$message, fixed = TRUE)) {
      # On Windows, convert via JSON string to avoid integer overflow
      # This handles timestamps and large counts that exceed 32-bit int range
      json_str <- tryCatch(gee_obj$serialize(), error = function(e2) NULL)
      if (!is.null(json_str)) {
        return(jsonlite::fromJSON(json_str))
      }
      # Fallback: return NA and warn
      warning("GEE integer overflow - returning NA. Consider updating reticulate.", call. = FALSE)
      return(NA)
    }
    stop(e)
  })
}


#' Safely get task status (Windows compatibility)
#'
#' Wraps task status() calls to handle Python integer overflow on Windows.
#' Task status contains timestamps that can exceed 32-bit integer range.
#'
#' @param task A GEE task object
#' @return List with state and description fields, or NULL on error
#' @keywords internal
safe_task_status <- function(task) {
  tryCatch({
    status <- task$status()
    # Access only the fields we need to avoid triggering overflow on timestamps
    list(
      state = tryCatch(as.character(status$state), error = function(e) NA_character_),
      description = tryCatch(as.character(status$description), error = function(e) NA_character_)
    )
  }, error = function(e) {
    if (grepl("too large to convert to C long", e$message, fixed = TRUE)) {
      # Try accessing fields individually with conversion
      tryCatch({
        list(
          state = as.character(reticulate::py_to_r(task$status()$state)),
          description = as.character(reticulate::py_to_r(task$status()$description))
        )
      }, error = function(e2) {
        warning("Could not retrieve task status due to integer overflow.", call. = FALSE)
        NULL
      })
    } else {
      warning(paste("Error getting task status:", e$message), call. = FALSE)
      NULL
    }
  })
}


#' Initialize Google Earth Engine
#'
#' Sets up Python environment and initializes the Earth Engine API.
#' Handles authentication and temporary conda environments as needed.
#'
#' @param gee_project Character. Google Earth Engine cloud project ID.
#'   This is required and must be a valid GEE project you have access to.
#'   You can find your project ID in the [GEE Code Editor](https://code.earthengine.google.com/)
#'   or [Google Cloud Console](https://console.cloud.google.com/).
#' @return A list containing the `ee` module and temporary environment name (if created)
#' @keywords internal
#' @examples
#' \dontrun{
#' env_info <- initialize_earthengine("my-gee-project")
#' ee <- env_info$ee
#' }
initialize_earthengine <- function(gee_project) {

  assertthat::assert_that(
    assertthat::is.string(gee_project) && nchar(gee_project) > 0,
    msg = "gee_project is required and must be a non-empty string (your GEE cloud project ID)"
  )

  log_message("Initializing Earth Engine...")

  temp_env <- NULL
  ee_env_found <- FALSE
  conda_base <- find_conda_base()

  # Set LD_LIBRARY_PATH for OpenSSL resolution (must be before Python init)
  if (!is.null(conda_base) && .Platform$OS.type != "windows") {
    for (env_name in c("ee_compat", "ee", "gee", "earthengine")) {
      env_lib <- file.path(conda_base, "envs", env_name, "lib")
      if (dir.exists(env_lib)) {
        current_ld_path <- Sys.getenv("LD_LIBRARY_PATH")
        if (!grepl(env_lib, current_ld_path, fixed = TRUE)) {
          new_ld_path <- if (nzchar(current_ld_path)) paste0(env_lib, ":", current_ld_path) else env_lib
          Sys.setenv(LD_LIBRARY_PATH = new_ld_path)
        }
        break
      }
    }
  }

  # Check if reticulate already initialized with ee module
  if (reticulate::py_available(initialize = FALSE)) {
    if (reticulate::py_module_available("ee")) {
      ee_env_found <- TRUE
    } else {
      log_message("Python initialized but missing 'ee' module. Restart R with RETICULATE_PYTHON set to ee environment.")
    }
  }

  # Check RETICULATE_PYTHON environment variable
  reticulate_python <- Sys.getenv("RETICULATE_PYTHON")
  if (!ee_env_found && nzchar(reticulate_python) && file.exists(reticulate_python)) {
    tryCatch({
      reticulate::use_python(reticulate_python, required = TRUE)
      if (!reticulate::py_available(initialize = FALSE)) reticulate::py_config()
      ee_test <- tryCatch({
        reticulate::import("ee", delay_load = FALSE)
        TRUE
      }, error = function(e) {
        if (grepl("OPENSSL", e$message, ignore.case = TRUE)) {
          env_lib <- file.path(dirname(dirname(reticulate_python)), "lib")
          log_message("OpenSSL mismatch. Try: LD_LIBRARY_PATH={env_lib} R")
        }
        FALSE
      })
      if (ee_test) ee_env_found <- TRUE
    }, error = function(e) NULL)
  }

  # Search for conda environments with Earth Engine

  if (!ee_env_found && !is.null(conda_base)) {
    candidate_envs <- c("ee_compat", "ee", "gee", "earthengine", "earth-engine")
    for (env_name in candidate_envs) {
      env_python <- find_env_python(conda_base, env_name)
      if (!is.null(env_python)) {
        tryCatch({
          reticulate::use_python(env_python, required = TRUE)
          if (!reticulate::py_available(initialize = FALSE)) reticulate::py_config()
          ee_test <- tryCatch({
            reticulate::import("ee", delay_load = FALSE)
            TRUE
          }, error = function(e) {
            if (grepl("OPENSSL", e$message, ignore.case = TRUE)) {
              env_lib <- file.path(conda_base, "envs", env_name, "lib")
              log_message("OpenSSL mismatch for '{env_name}'. Try: LD_LIBRARY_PATH={env_lib} R")
            }
            FALSE
          })
          if (ee_test) {
            log_message("Using conda environment: {env_name}")
            ee_env_found <- TRUE
            break
          }
        }, error = function(e) NULL)
      }
    }
  }

  # Create temporary environment if none found
  if (!ee_env_found) {
    log_message("Creating temporary conda environment for Earth Engine...")
    temp_env <- create_gee_conda_env()
  }

  # Import and initialize Earth Engine
  ee <- reticulate::import("ee")

  cred_path <- file.path(rappdirs::user_config_dir("earthengine"), "credentials")
  if (!file.exists(cred_path)) {
    log_message("Starting Earth Engine authentication...")
    ee$Authenticate()
  }

  ee$Initialize(project = gee_project)
  log_message("Earth Engine initialized (project: {gee_project})")

  return(list(ee = ee, temp_env = temp_env))
}

#' Clean up Earth Engine environment
#'
#' Removes temporary conda environment if one was created during initialization.
#' Should be called at the end of any function that uses initialize_earthengine().
#'
#' @param env_info List returned from initialize_earthengine() containing temp_env info
#' @keywords internal
#' @examples
#' \dontrun{
#' env_info <- initialize_earthengine()
#' # ... do work with GEE ...
#' cleanup_earthengine(env_info)
#' }
cleanup_earthengine <- function(env_info) {
  if (!is.null(env_info$temp_env)) {
    log_message("Cleaning up temporary conda environment: {env_info$temp_env}")
    reticulate::conda_remove(env_info$temp_env)
  }
}

#' Download files from Google Drive
#'
#' Downloads all files matching a specified prefix from a Google Drive folder.
#' Files are filtered to only include .tif files matching the prefix pattern.
#'
#' @param drive_folder Character or NULL. Google Drive folder name/path, or NULL for root.
#' @param file_prefix Character. Prefix to search for in filenames.
#' @param local_path Character. Local directory path where files should be downloaded.
#'
#' @return Invisible list with `success` (logical) and `files` (character vector of
#'   successfully downloaded file paths). Returns `success = FALSE` if any download fails.
#' @keywords internal
#' @examples
#' \dontrun{
#' result <- download_from_drive("my_gee_exports", "esri_lulc_2024_GHA", "/tmp/downloads")
#' if (!result$success) warning("Some downloads failed")
#' }
download_from_drive <- function(drive_folder, file_prefix, local_path) {
  # Input validation
  assertthat::assert_that(
    is.character(file_prefix) && nchar(file_prefix) > 0,
    msg = "'file_prefix' must be a non-empty character string."
  )

  assertthat::assert_that(
    is.character(local_path) && dir.exists(local_path),
    msg = "'local_path' must be an existing directory path."
  )

  log_message("Searching Google Drive for files matching '{file_prefix}'...")

  # List all files in the specified Drive folder
  files <- tryCatch(
    googledrive::drive_ls(path = drive_folder),
    error = function(e) {
      log_message("Error accessing Google Drive folder: {e$message}")
      return(NULL)
    }
  )

 if (is.null(files) || nrow(files) == 0) {
    log_message("No files found in Google Drive folder")
    return(invisible(list(success = FALSE, files = character(0))))
  }

  # Filter to only .tif files that match the prefix pattern
  matching_files <- files[grepl(paste0("^", file_prefix), files$name) & grepl("\\.tif$", files$name), ]

  if (nrow(matching_files) == 0) {
    log_message("No .tif files matching prefix '{file_prefix}' found")
    return(invisible(list(success = FALSE, files = character(0))))
  }

  # Download each matching file with error handling
 downloaded_files <- character(0)
  failed_downloads <- character(0)

  for (i in seq_len(nrow(matching_files))) {
    file_path <- file.path(local_path, matching_files$name[i])

    result <- tryCatch({
      googledrive::drive_download(
        googledrive::as_id(matching_files$id[i]),
        path = file_path,
        overwrite = TRUE
      )
      log_message("Downloaded: {matching_files$name[i]}")
      TRUE
    }, error = function(e) {
      log_message("Failed to download {matching_files$name[i]}: {e$message}")
      FALSE
    })

    if (result) {
      downloaded_files <- c(downloaded_files, file_path)
    } else {
      failed_downloads <- c(failed_downloads, matching_files$name[i])
    }
  }

  if (length(failed_downloads) > 0) {
    warning(
      glue::glue("Failed to download {length(failed_downloads)} file(s): {paste(failed_downloads, collapse = ', ')}"),
      call. = FALSE
    )
  }

  invisible(list(
    success = length(failed_downloads) == 0,
    files = downloaded_files
  ))
}

#' Merge downloaded GEE tiles into a single Cloud-Optimized GeoTIFF
#'
#' Takes multiple downloaded GeoTIFF tiles and merges them into a single
#' Cloud-Optimized GeoTIFF (COG) with appropriate compression and optimization.
#'
#' @param local_path Character. Path to directory containing downloaded tile files
#' @param output_file Character. Full path for the output merged COG file
#' @param datatype Character. GDAL datatype for output raster (e.g., "INT1U", "FLT4S")
#' @return A `SpatRaster` object of the merged and written COG
#' @keywords internal
#' @examples
#' \dontrun{
#' merged_raster <- merge_tiles("/tmp/tiles", "/output/merged.tif", "INT1U")
#' }
merge_tiles <- function(local_path, output_file, datatype) {
  # Input validation
  assertthat::assert_that(
    is.character(local_path) && dir.exists(local_path),
    msg = "'local_path' must be an existing directory path."
  )

  assertthat::assert_that(
    is.character(output_file) && nchar(output_file) > 0,
    msg = "'output_file' must be a non-empty character string."
  )

  assertthat::assert_that(
    is.character(datatype) && datatype %in% c("INT1U", "INT2U", "INT4U", "FLT4S", "FLT8S"),
    msg = "'datatype' must be a valid GDAL datatype (e.g., 'INT1U', 'FLT4S')."
  )

  # Find all .tif files in the local directory
  tile_files <- list.files(local_path, pattern = "\\.tif$", full.names = TRUE)

  if (length(tile_files) == 0) {
    stop(
      glue::glue("No downloaded tiles found in '{local_path}'. Check that GEE export completed successfully."),
      call. = FALSE
    )
  }

  log_message("Merging {length(tile_files)} tile(s) into final output: {output_file}")

  # Handle single tile vs multiple tiles
  if (length(tile_files) == 1) {
    r <- terra::rast(tile_files)
  } else {
    # Use virtual raster for efficient merging of multiple tiles
    r <- terra::vrt(tile_files)
  }

  # Set predictor based on datatype for optimal compression
  predictor <- if (grepl("^INT", datatype)) "2" else "3"

  # Write as Cloud-Optimized GeoTIFF with compression
  terra::writeRaster(
    r,
    output_file,
    filetype = "COG",
    datatype = datatype,
    gdal = c(
      "COMPRESS=ZSTD",              # Modern compression algorithm
      "NUM_THREADS=ALL_CPUS",       # Use all available CPU cores
      "BIGTIFF=IF_SAFER",           # Handle large files
      glue::glue("PREDICTOR={predictor}"), # Optimize compression for data type
      "OVERVIEWS=NONE"              # Let terra handle overviews
    ),
    overwrite = TRUE
  )

  log_message("COG written to disk.")
  return(terra::rast(output_file))
}

#' Download and Process a GEE Raster Layer into a Cloud-Optimized GeoTIFF
#'
#' Generic function for downloading any raster data from Google Earth Engine.
#' Handles authentication, export management, file downloading, and processing
#' into a local Cloud-Optimized GeoTIFF. Exports to Google Drive root to avoid
#' GEE folder duplication bugs.
#'
#' For most use cases, prefer the higher-level wrapper functions like
#' [download_esri_lulc_data()] or [download_global_pasture_data()] which
#' have sensible defaults for common datasets.
#'
#' @param boundary_layer An `sf` object defining the spatial boundary of interest.
#' @param iso3 Character. Three-letter ISO country code for filename generation.
#' @param gee_project Character. Google Earth Engine cloud project ID. This is
#'   required and must be a valid GEE project you have access to. Find your
#'   project ID in the [GEE Code Editor](https://code.earthengine.google.com/).
#' @param asset_id Character. Earth Engine ImageCollection asset ID
#'   (e.g., "projects/sat-io/open-datasets/...").
#' @param file_prefix Character. Prefix for export filename and GEE task description.
#' @param output_dir Character. Path to directory for saving the final raster file.
#'   Defaults to project root via `here::here()`.
#' @param scale Numeric. Resolution of the exported image in meters. Default is 10.
#'   Ignored when `aggregate_to_pus = TRUE` (uses PU resolution instead).
#' @param datatype Character. Output datatype (GDAL style), e.g., "INT1U" or "FLT4S".
#'   Default is "INT1U".
#' @param googledrive_folder Character or NULL. Google Drive folder name for exports.
#'   Currently defaults to NULL (Drive root) to avoid a GEE folder duplication bug.
#' @param wait_time Numeric. Maximum time in minutes to wait for the GEE export
#'   to appear in Google Drive. Default is 5. Increase for large exports.
#' @param band Character or NULL. Specific band to select from the image. If NULL,
#'   uses all bands. Useful for datasets like Dynamic World ("label") or
#'   ESA WorldCover ("Map").
#' @param composite_method Character. Method for creating temporal composite:
#'   "mosaic" (default), "mode" (most frequent value), "median", or "mean".
#'   Use "mode" for categorical data like Dynamic World.
#' @param year_override Numeric or NULL. If provided, use this year instead of
#'   automatically detecting the most recent year with data.
#' @param pus SpatRaster or NULL. Planning units raster for GEE-side aggregation.
#'   When provided with `aggregate_to_pus = TRUE`, the export will be resampled
#'   to match the PU resolution and projection in GEE before download.
#' @param aggregate_to_pus Logical. If TRUE and `pus` is provided, aggregate the
#'   data to planning unit resolution in GEE before export. This significantly
#'   speeds up processing by eliminating local resampling. Default is FALSE.
#' @param aggregation_reducer Character. Reducer to use for GEE-side aggregation:
#'   "mode" (default, for categorical data), "mean", or "sum".
#'
#' @return A `SpatRaster` object written to disk, or NULL if export timed out.
#'   When `aggregate_to_pus = TRUE`, the result has attribute `pre_aggregated = TRUE`.
#'
#' @seealso [download_esri_lulc_data()], [download_global_pasture_data()],
#'   [download_lulc_data()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download ESRI LULC data for Ghana
#' lulc <- download_gee_layer(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project",
#'   asset_id = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
#'   file_prefix = "esri_10m_lulc",
#'   scale = 10,
#'   datatype = "INT1U"
#' )
#'
#' # Download with GEE-side aggregation to planning units
#' lulc_agg <- download_gee_layer(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project",
#'   asset_id = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
#'   file_prefix = "esri_10m_lulc_agg",
#'   pus = planning_units,
#'   aggregate_to_pus = TRUE,
#'   aggregation_reducer = "mode"
#' )
#' }
download_gee_layer <- function(
    boundary_layer,
    iso3,
    gee_project,
    asset_id,
    file_prefix,
    output_dir = here::here(),
    scale = 10,
    datatype = "INT1U",
    googledrive_folder = NULL,
    wait_time = 5,
    band = NULL,
    composite_method = c("mosaic", "mode", "median", "mean"),
    year_override = NULL,
    pus = NULL,
    aggregate_to_pus = FALSE,
    aggregation_reducer = c("mode", "mean", "sum")
) {
  composite_method <- match.arg(composite_method)
  aggregation_reducer <- match.arg(aggregation_reducer)

  # Input validation
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.", call. = FALSE)
  }
  assertthat::assert_that(inherits(boundary_layer, "sf"), msg = "boundary_layer must be an sf object")
  assertthat::assert_that(
    is.numeric(wait_time) && length(wait_time) == 1 && wait_time %% 1 == 0,
    msg = "wait_time must be a single whole number (integer or numeric)"
  )

  # Validate aggregation parameters
  if (aggregate_to_pus) {
    assertthat::assert_that(
      !is.null(pus) && inherits(pus, "SpatRaster"),
      msg = "When aggregate_to_pus = TRUE, 'pus' must be a SpatRaster object"
    )
  }

  # Set up folder structure (export to Drive root to avoid GEE folder duplication bug)
  export_folder <- if (is.null(googledrive_folder)) NULL else paste0(googledrive_folder, "_", iso3)

  # Create temporary local directory for downloads
  temp_dir <- file.path(Sys.getenv("HOME"), glue::glue("gee_download_{iso3}_{Sys.getpid()}"))
  dir.create(temp_dir, showWarnings = FALSE)

  # Initialize Earth Engine
  env_info <- tryCatch({
    initialize_earthengine(gee_project)
  }, error = function(e) {
    unlink(temp_dir, recursive = TRUE)
    stop(glue::glue("Earth Engine initialization failed: {e$message}"), call. = FALSE)
  })
  ee <- env_info$ee

  # Ensure googledrive is authenticated before we suppress messages later
  # This allows the account selection prompt to appear if needed
  if (!googledrive::drive_has_token()) {
    googledrive::drive_auth()
  }

  # Load the dataset
  ic <- tryCatch({
    ee$ImageCollection(asset_id)
  }, error = function(e) {
    cleanup_earthengine(env_info)
    unlink(temp_dir, recursive = TRUE)
    stop(glue::glue("Failed to load GEE asset '{asset_id}': {e$message}"), call. = FALSE)
  })

  # Determine year to use
  if (!is.null(year_override)) {
    year <- year_override
  } else {
    year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    # Search backwards for a year with data
    while (ic$filterDate(ee$Date(glue::glue("{year}-01-01")), ee$Date(glue::glue("{year}-12-31")))$size()$getInfo() == 0) {
      year <- year - 1
      if (year < 2000) {
        cleanup_earthengine(env_info)
        unlink(temp_dir, recursive = TRUE)
        stop("No data found for any recent year (back to 2000) in the collection.", call. = FALSE)
      }
    }
  }

  # Build export filename and check if local file already exists
  file_name <- glue::glue("{file_prefix}_{year}_{iso3}")
  output_file <- file.path(output_dir, glue::glue("{file_name}.tif"))

  if (file.exists(output_file)) {
    log_message("Using existing file: {basename(output_file)}")
    cleanup_earthengine(env_info)
    unlink(temp_dir, recursive = TRUE)
    return(terra::rast(output_file))
  }

  # Ensure export folder exists (if not exporting to root)
  if (!is.null(export_folder)) {
    existing_folders <- googledrive::drive_ls(path = NULL, type = "folder")
    if (!(export_folder %in% existing_folders$name)) {
      googledrive::drive_mkdir(export_folder)
    }
  }

  # Check for existing exported files in Drive
  exact_file <- paste0(file_name, ".tif")
  existing_files <- tryCatch({
    # Suppress googledrive messages about files not found
    suppressMessages(googledrive::drive_get(exact_file))
  }, error = function(e) {
    data.frame(name = character(0))
  })

  # If not found, try drive_find as fallback
  if (nrow(existing_files) == 0) {
    all_files <- suppressMessages(googledrive::drive_find(q = paste0("name contains '", file_name, "'")))
    existing_files <- all_files[grepl(paste0("^", file_name, ".*\\.tif$"), all_files$name), ]
  }

  if (nrow(existing_files) > 0) {
    matching_files <- existing_files
    log_message("Found in Drive: {file_name}")
  } else {
    log_message("Not found in Drive, will export from GEE: {file_name}")
    log_message("Preparing export task...")

    # Check for running export tasks with the same description
    tasks <- ee$batch$Task$list()
    existing_task <- purrr::detect(tasks, function(t) {
      status <- safe_task_status(t)
      if (is.null(status)) return(FALSE)
      status$state %in% c("READY", "RUNNING") && status$description == file_name
    })

    if (is.null(existing_task)) {
      # Prepare geometry for export in WGS84
      if (aggregate_to_pus && !is.null(pus)) {
        pu_ext <- terra::ext(pus)
        corners <- sf::st_sfc(
          sf::st_point(c(pu_ext$xmin, pu_ext$ymin)),
          sf::st_point(c(pu_ext$xmax, pu_ext$ymax)),
          crs = terra::crs(pus, proj = TRUE)
        )
        corners_wgs84 <- sf::st_transform(corners, crs = 4326)
        coords <- sf::st_coordinates(corners_wgs84)
        ee_bounding_box <- ee$Geometry$Rectangle(
          c(coords[1, "X"], coords[1, "Y"], coords[2, "X"], coords[2, "Y"]),
          proj = "EPSG:4326", geodesic = FALSE
        )
      } else {
        boundary_layer <- sf::st_transform(boundary_layer, crs = 4326)
        bounding_box <- sf::st_bbox(boundary_layer)
        ee_bounding_box <- ee$Geometry$Rectangle(
          c(bounding_box$xmin, bounding_box$ymin, bounding_box$xmax, bounding_box$ymax),
          proj = "EPSG:4326", geodesic = FALSE
        )
      }

      # Filter collection and create composite
      start_date <- ee$Date(glue::glue("{year}-01-01"))
      end_date <- ee$Date(glue::glue("{year}-12-31"))
      filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)
      source_projection <- filtered_data$first()$projection()

      composite <- switch(
        composite_method,
        mosaic = filtered_data$mosaic(),
        mode = filtered_data$mode(),
        median = filtered_data$median(),
        mean = filtered_data$mean(),
        filtered_data$mosaic()
      )

      if (!is.null(band)) composite <- composite$select(band)

      # Prepare export parameters
      log_message("Building export parameters...")
      region_coords <- ee_bounding_box$getInfo()[["coordinates"]]
      export_params <- list(
        image = composite,
        description = file_name,
        folder = export_folder,
        fileNamePrefix = file_name,
        region = region_coords,
        maxPixels = reticulate::r_to_py(1e13),
        fileFormat = "GeoTIFF"
      )

      # Handle GEE-side aggregation to planning units
      if (aggregate_to_pus && !is.null(pus)) {
        pu_crs <- get_crs_wkt(pus)
        pu_ext <- terra::ext(pus)
        pu_res <- terra::res(pus)
        export_params$crs <- "EPSG:4326"
        export_params$scale <- pu_res[1]
      } else {
        export_params$scale <- scale
      }

      # Submit export task
      task <- do.call(ee$batch$Export$image$toDrive, export_params)
      task$start()
      log_message("Export task submitted")
    }

    # Wait for files to appear in Drive
    log_message("Waiting for export to complete (this may take several minutes)...")
    start_time <- Sys.time()
    prev_file_count <- 0
    stable_count <- 0

    repeat {
      all_tifs <- suppressMessages(googledrive::drive_find(
        q = paste0("name contains '", file_name, "'"),
        n_max = 500  # Ensure we get all tiles for large exports
      ))
      matching_files <- all_tifs[grepl(paste0("^", file_name, ".*\\.tif$"), all_tifs$name), ]
      current_count <- nrow(matching_files)

      elapsed <- difftime(Sys.time(), start_time, units = "mins")

      # Check if file count has stabilized (same count for 2 consecutive checks)
      # This handles multi-tile exports where tiles appear gradually
      if (current_count > 0) {
        if (current_count == prev_file_count) {
          stable_count <- stable_count + 1
          if (stable_count >= 2) {
            log_message("Found {current_count} tile(s) ready for download")
            break
          }
        } else {
          stable_count <- 0
          log_message("Export in progress: {current_count} tile(s) found so far...")
        }
        prev_file_count <- current_count
      }

      if (elapsed > wait_time) {
        # If we have some files but timed out waiting for more, use what we have
        if (current_count > 0) {
          log_message("Timeout reached with {current_count} tile(s) - proceeding with available files")
          warning(
            "GEE export may be incomplete. Found ", current_count, " tiles but more may exist.\n",
            "Check https://code.earthengine.google.com/tasks for task status.",
            call. = FALSE
          )
          break
        }
        cleanup_earthengine(env_info)
        unlink(temp_dir, recursive = TRUE)
        stop(glue::glue(
          "GEE export timeout after {as.integer(wait_time)} min.\n",
          "The export is still running in GEE. Please:\n",
          "  1. Check status at: https://code.earthengine.google.com/tasks\n",
          "  2. Wait for the export to complete\n",
          "  3. Run the pipeline again - it will download the completed file"
        ), call. = FALSE)
      }

      if (elapsed >= 0.5 && current_count == 0) {  # Only show after 30 seconds if no files yet
        log_message("Still waiting... ({round(elapsed, 1)} min elapsed)")
      }
      Sys.sleep(30)
    }
  }

  # Download files
  for (i in seq_len(nrow(matching_files))) {
    local_path <- file.path(temp_dir, matching_files$name[i])
    googledrive::drive_download(matching_files[i, ], path = local_path, overwrite = TRUE)
    log_message("Downloaded: {matching_files$name[i]}")
  }

  output_raster <- merge_tiles(temp_dir, output_file, datatype)

  # Cleanup temporary files and environments
  unlink(temp_dir, recursive = TRUE)
  cleanup_earthengine(env_info)

  # Reproject to PU CRS locally if GEE-side aggregation was used
  # GEE exports in WGS84, so we reproject to PU CRS here
  if (aggregate_to_pus && !is.null(pus)) {
    resample_method <- switch(
      aggregation_reducer,
      mode = "mode",
      mean = "bilinear",
      sum = "sum",
      "near"  # Default fallback
    )
    output_raster <- terra::project(output_raster, pus, method = resample_method)
    # Overwrite the file with reprojected version
    terra::writeRaster(output_raster, output_file, overwrite = TRUE,
                       filetype = "COG", gdal = c("COMPRESS=DEFLATE"))
    attr(output_raster, "pre_aggregated") <- TRUE
    attr(output_raster, "aggregation_reducer") <- aggregation_reducer
  }

  return(output_raster)
}

#' Download the ESRI 10m Land Use/Land Cover Time Series (LULC) Layer
#'
#' Retrieves the ESRI Global LULC Time Series at 10m resolution from Earth Engine.
#' Downloads the most recent year available and returns the result as a local
#' Cloud-Optimized GeoTIFF with appropriate layer naming.
#'
#' @inheritParams download_gee_layer
#' @param output_dir Character. Local output directory. Defaults to project root via `here::here()`
#' @param ... Additional arguments passed to [download_gee_layer()].
#'
#' @return A `SpatRaster` object of the downloaded LULC data, or NULL if download failed
#'
#' @seealso [download_lulc_data()], [get_lulc_classes()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download LULC data for Ghana
#' lulc <- download_esri_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project"
#' )
#'
#' # Download with GEE-side aggregation to planning units
#' lulc_agg <- download_esri_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project",
#'   pus = planning_units,
#'   aggregate_to_pus = TRUE
#' )
#' }
download_esri_lulc_data <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    pus = NULL,
    aggregate_to_pus = FALSE,
    wait_time = 5,
    ...
) {
  # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )

  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )

  assertthat::assert_that(
    is.character(gee_project) && nchar(gee_project) > 0,
    msg = "'gee_project' must be a non-empty character string (your GEE cloud project ID)."
  )

  log_message("Downloading ESRI 10m LULC data for {iso3}...")

  result <- download_gee_layer(
    boundary_layer = boundary_layer,
    iso3 = iso3,
    gee_project = gee_project,
    output_dir = output_dir,
    asset_id = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
    file_prefix = "esri_10m_lulc",
    scale = 10,
    datatype = "INT1U",
    pus = pus,
    aggregate_to_pus = aggregate_to_pus,
    aggregation_reducer = "mode",
    wait_time = wait_time,
    ...
  )

  # Set appropriate layer name for the result
  if (!is.null(result)) {
    names(result) <- "ESRI_Global-LULC_10m_TS"
    attr(result, "lulc_product") <- "esri_10m"
  }

  return(result)
}

#' Download Global Pasture Watch Grassland Probability Layer
#'
#' Downloads either the cultivated or natural/semi-natural grassland probability
#' layer from the Global Pasture Watch dataset hosted in Earth Engine. Returns
#' the result as a local Cloud-Optimized GeoTIFF with appropriate layer naming.
#'
#' @inheritParams download_gee_layer
#' @param layer_type Character. One of "cultivated" (default) or "natural" to specify
#'   which grassland probability layer to download
#' @param output_dir Character. Local output directory. Defaults to project root via `here::here()`
#' @param ... Additional arguments passed to [download_gee_layer()].
#'
#' @return A `SpatRaster` object of the downloaded grassland data, or NULL if download failed
#' @export
#' @examples
#' \dontrun{
#' # Download cultivated grassland data for Nepal
#' pasture <- download_global_pasture_data(
#'   boundary_layer = nepal_boundary,
#'   iso3 = "NPL",
#'   layer_type = "cultivated",
#'   gee_project = "my-gee-project"
#' )
#' }
download_global_pasture_data <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    layer_type = c("cultivated", "natural"),
    ...
) {
  layer_type <- match.arg(layer_type)

  # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )

  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )

  assertthat::assert_that(
    is.character(gee_project) && nchar(gee_project) > 0,
    msg = "'gee_project' must be a non-empty character string (your GEE cloud project ID)."
  )

  log_message("Downloading Global Pasture Watch ({layer_type}) data for {iso3}...")

  # Set asset ID and filename prefix based on layer type
  asset_id <- switch(
    layer_type,
    cultivated = "projects/global-pasture-watch/assets/ggc-30m/v1/cultiv-grassland_p",
    natural    = "projects/global-pasture-watch/assets/ggc-30m/v1/nat-semi-grassland_p"
  )
  file_prefix <- switch(
    layer_type,
    cultivated = "gpw_cultiv-grassland_p",
    natural    = "gpw_nat-semi-grassland_p"
  )

  result <- download_gee_layer(
    boundary_layer = boundary_layer,
    iso3 = iso3,
    gee_project = gee_project,
    output_dir = output_dir,
    asset_id = asset_id,
    file_prefix = file_prefix,
    scale = 30,  # Global Pasture Watch is 30m resolution
    datatype = "FLT4S",  # Probability values are floating point
    ...
  )

  # Set appropriate layer name for the result
  if (!is.null(result)) {
    names(result) <- file_prefix
  }

  return(result)
}


#' Download Google Dynamic World Annual Composite
#'
#' Downloads an annual mode composite from Google Dynamic World. Creates a
#' pixel-wise mode (most frequent class) composite for the specified year.
#' Dynamic World provides near-real-time land use/land cover classification
#' at 10m resolution.
#'
#' @inheritParams download_gee_layer
#' @param year Numeric or NULL. Year for the annual composite. If NULL (default),
#'   uses the previous calendar year.
#' @param output_dir Character. Local output directory. Defaults to project root via `here::here()`.
#' @param ... Additional arguments passed to [download_gee_layer()].
#'
#' @return A `SpatRaster` object of the downloaded Dynamic World data, or NULL if download failed.
#'
#' @details
#' Google Dynamic World class values:
#' \itemize{
#'   \item 0: Water
#'   \item 1: Trees
#'   \item 2: Grass
#'   \item 3: Flooded vegetation
#'   \item 4: Crops
#'   \item 5: Shrub & Scrub
#'   \item 6: Built Area
#'   \item 7: Bare ground
#'
#'   \item 8: Snow & Ice
#' }
#'
#' Use `get_lulc_classes("dynamic_world")` to retrieve these mappings programmatically.
#'
#' @seealso [download_lulc_data()], [get_lulc_classes()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download Dynamic World annual composite for Ghana
#' dw_lulc <- download_dynamic_world_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project",
#'   year = 2023
#' )
#'
#' # Download with GEE-side aggregation to planning units
#' dw_lulc_agg <- download_dynamic_world_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project",
#'   pus = planning_units,
#'   aggregate_to_pus = TRUE
#' )
#' }
download_dynamic_world_data <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    year = NULL,
    pus = NULL,
    aggregate_to_pus = FALSE,
    wait_time = 5,
    ...
) {
  # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )

  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )

  assertthat::assert_that(
    !is.null(gee_project) && nchar(gee_project) > 0,
    msg = "'gee_project' is required for Dynamic World downloads."
  )

  # Default to previous year if not specified
  if (is.null(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  }

  log_message("Downloading Dynamic World annual composite for {iso3} ({year})...")

  result <- download_gee_layer(
    boundary_layer = boundary_layer,
    iso3 = iso3,
    gee_project = gee_project,
    output_dir = output_dir,
    asset_id = "GOOGLE/DYNAMICWORLD/V1",
    file_prefix = glue::glue("dynamic_world_lulc_{year}"),
    scale = 10,
    datatype = "INT1U",
    band = "label",
    composite_method = "mode",
    year_override = year,
    pus = pus,
    aggregate_to_pus = aggregate_to_pus,
    aggregation_reducer = "mode",
    wait_time = wait_time,
    ...
  )

  if (!is.null(result)) {
    names(result) <- "Dynamic_World_LULC"
    attr(result, "lulc_product") <- "dynamic_world"
  }

  return(result)
}


#' Download ESA WorldCover Data
#'
#' Downloads ESA WorldCover 10m land cover data from Google Earth Engine.
#' WorldCover v200 is a static 2021 baseline product without annual updates.
#'
#' @inheritParams download_gee_layer
#' @param output_dir Character. Local output directory. Defaults to project root via `here::here()`.
#' @param ... Additional arguments passed to [download_gee_layer()].
#'
#' @return A `SpatRaster` object of the downloaded ESA WorldCover data, or NULL if download failed.
#'
#' @details
#' ESA WorldCover class values:
#' \itemize{
#'   \item 10: Tree cover
#'   \item 20: Shrubland
#'   \item 30: Grassland
#'   \item 40: Cropland
#'   \item 50: Built-up
#'   \item 60: Bare / sparse vegetation
#'   \item 70: Snow and ice
#'   \item 80: Permanent water bodies
#'   \item 90: Herbaceous wetland
#'   \item 95: Mangroves
#'   \item 100: Moss and lichen
#' }
#'
#' Use `get_lulc_classes("esa_worldcover")` to retrieve these mappings programmatically.
#'
#' @note ESA WorldCover v200 is a static 2021 baseline product. Unlike ESRI LULC
#'   or Dynamic World, it does not have annual updates.
#'
#' @seealso [download_lulc_data()], [get_lulc_classes()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download ESA WorldCover for Ghana
#' esa_lulc <- download_esa_worldcover_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project"
#' )
#'
#' # Download with GEE-side aggregation to planning units
#' esa_lulc_agg <- download_esa_worldcover_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project",
#'   pus = planning_units,
#'   aggregate_to_pus = TRUE
#' )
#' }
download_esa_worldcover_data <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    pus = NULL,
    aggregate_to_pus = FALSE,
    wait_time = 5,
    ...
) {
  # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )

  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )

  assertthat::assert_that(
    !is.null(gee_project) && nchar(gee_project) > 0,
    msg = "'gee_project' is required for ESA WorldCover downloads."
  )

  log_message("Downloading ESA WorldCover data for {iso3}...")
  log_message("Note: ESA WorldCover v200 is a static 2021 baseline (no annual updates).")

  result <- download_gee_layer(
    boundary_layer = boundary_layer,
    iso3 = iso3,
    gee_project = gee_project,
    output_dir = output_dir,
    asset_id = "ESA/WorldCover/v200",
    file_prefix = "esa_worldcover_lulc",
    scale = 10,
    datatype = "INT1U",
    band = "Map",
    composite_method = "mosaic",
    year_override = 2021,
    pus = pus,
    aggregate_to_pus = aggregate_to_pus,
    aggregation_reducer = "mode",
    wait_time = wait_time,
    ...
  )

  if (!is.null(result)) {
    names(result) <- "ESA_WorldCover_v200"
    attr(result, "lulc_product") <- "esa_worldcover"
  }

  return(result)
}


#' Load User-Provided Local LULC Data
#'
#' Loads and validates a user-provided LULC GeoTIFF or Cloud-Optimized GeoTIFF (COG) file.
#' Ensures the data is properly formatted and optionally validates coverage against
#' a boundary layer.
#'
#' @param local_file Character. Path to the local GeoTIFF or COG file.
#' @param boundary_layer An `sf` object for extent validation (optional but recommended).
#'
#' @return A `SpatRaster` object with attribute `lulc_product = "local"`.
#'
#' @details
#' When using a local LULC file, you must provide your own class mappings to
#' consumer functions (e.g., `make_manage_zone()`, `make_protect_zone()`) via
#' the `agriculture_lulc_value`, `built_area_lulc_value`, and similar parameters.
#'
#' @seealso [download_lulc_data()], [get_lulc_classes()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Load a local LULC file
#' local_lulc <- load_local_lulc_data(
#'   local_file = "path/to/my_lulc.tif",
#'   boundary_layer = ghana_boundary
#' )
#'
#' # Use with custom class mappings
#' manage_zone <- make_manage_zone(
#'   lulc_raster = local_lulc,
#'   lulc_product = "local",
#'   agriculture_lulc_value = 12,
#'   built_area_lulc_value = 15,
#'   ...
#' )
#' }
load_local_lulc_data <- function(
    local_file,
    boundary_layer = NULL
) {
  # Input validation
  assertthat::assert_that(
    !is.null(local_file),
    msg = "'local_file' is required."
  )
  assertthat::assert_that(
    file.exists(local_file),
    msg = glue::glue("Local LULC file not found: {local_file}")
  )

  log_message("Loading local LULC file: {local_file}")

  result <- terra::rast(local_file)

  # Validate coverage against boundary if provided
  if (!is.null(boundary_layer)) {
    assertthat::assert_that(
      inherits(boundary_layer, "sf"),
      msg = "'boundary_layer' must be an sf object."
    )

    boundary_ext <- boundary_layer %>%
      sf::st_transform(terra::crs(result)) %>%
      sf::st_bbox() %>%
      terra::ext()

    raster_ext <- terra::ext(result)

    # Check if raster contains the boundary extent
    if (raster_ext$xmin > boundary_ext$xmin ||
        raster_ext$xmax < boundary_ext$xmax ||
        raster_ext$ymin > boundary_ext$ymin ||
        raster_ext$ymax < boundary_ext$ymax) {
      warning(
        "Local LULC file may not fully cover the boundary extent. ",
        "Results may contain NA values in uncovered areas.",
        call. = FALSE
      )
    }
  }

  names(result) <- "Local_LULC"
  attr(result, "lulc_product") <- "local"

  log_message("Local LULC file loaded successfully.")

  return(result)
}


#' Download LULC Class Proportion Raster from GEE
#'
#' Downloads a pre-computed class proportion raster from Google Earth Engine.
#' This function computes the proportion of a specific LULC class (e.g., agriculture,
#' built area) within each planning unit cell directly in GEE, avoiding the need
#' for expensive local resampling.
#'
#' @inheritParams download_gee_layer
#' @param lulc_product Character. LULC product to use: "esri_10m" (default),
#'   "dynamic_world", or "esa_worldcover".
#' @param class_name Character. Name of the class to extract proportion for.
#'   Common values: "agriculture", "built_area", "trees", "forest_managed".
#' @param class_values Integer vector or NULL. Explicit class values to use.
#'   If NULL, automatically determined from `lulc_product` and `class_name`.
#' @param year Numeric or NULL. Year for LULC data. If NULL, uses most recent.
#' @param pus SpatRaster. Planning units raster (required for aggregation).
#' @param ... Additional arguments passed to [download_gee_layer()].
#'
#' @return A `SpatRaster` with class proportion values (0-1) at PU resolution,
#'   with attribute `pre_aggregated = TRUE`.
#'
#' @details
#' This function:
#' 1. Loads the LULC ImageCollection in GEE
#' 2. Creates a binary mask where target class = 1, other classes = 0
#' 3. Aggregates using mean reducer to get proportion per PU cell
#' 4. Exports at PU resolution using CRS transform
#'
#' The resulting raster can be used directly in consumer functions via the
#' `agricultural_areas_input`, `built_areas_input`, etc. parameters without
#' additional local processing.
#'
#' @seealso [download_lulc_data()], [get_lulc_class_value()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download agriculture proportion at PU resolution
#' ag_prop <- download_lulc_class_proportion(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-project",
#'   lulc_product = "esri_10m",
#'   class_name = "agriculture",
#'   pus = planning_units
#' )
#'
#' # Use directly in make_protect_zone
#' protect <- make_protect_zone(
#'   agricultural_areas_input = ag_prop,
#'   ...
#' )
#'
#' # Download built area proportion with explicit class values
#' built_prop <- download_lulc_class_proportion(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-project",
#'   lulc_product = "esri_10m",
#'   class_name = "built_area",
#'   class_values = 7,  # ESRI built area class
#'   pus = planning_units
#' )
#' }
download_lulc_class_proportion <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    lulc_product = c("esri_10m", "dynamic_world", "esa_worldcover"),
    class_name,
    class_values = NULL,
    year = NULL,
    pus,
    wait_time = 5,
    ...
) {
  lulc_product <- match.arg(lulc_product)

 # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )
  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )
  assertthat::assert_that(
    !is.null(gee_project) && nchar(gee_project) > 0,
    msg = "'gee_project' is required."
  )
  assertthat::assert_that(
    inherits(pus, "SpatRaster"),
    msg = "'pus' must be a SpatRaster object."
  )

  # Resolve class values if not provided
  if (is.null(class_values)) {
    class_values <- get_lulc_class_value(lulc_product, class_name)
  }

  log_message("Downloading {class_name} proportion ({lulc_product})")

  # Get asset ID for the product
 asset_id <- switch(
    lulc_product,
    esri_10m = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
    dynamic_world = "GOOGLE/DYNAMICWORLD/V1",
    esa_worldcover = "ESA/WorldCover/v200"
  )

  # Get band name for the product
  band <- switch(
    lulc_product,
    esri_10m = NULL,  # ESRI uses default band
    dynamic_world = "label",
    esa_worldcover = "Map"
  )

  # Default to previous year if not specified
  if (is.null(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    # ESA WorldCover is fixed at 2021
    if (lulc_product == "esa_worldcover") {
      year <- 2021
    }
  }

  # Build file prefix
  file_prefix <- glue::glue("{lulc_product}_{class_name}_prop")

  # Initialize GEE
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.", call. = FALSE)
  }

  env_info <- initialize_earthengine(gee_project)
  ee <- env_info$ee

  # Create temporary directory for downloads
  temp_dir <- tempfile(pattern = "gee_")
  dir.create(temp_dir, showWarnings = FALSE)

  tryCatch({
    # Load the ImageCollection
    ic <- ee$ImageCollection(asset_id)

    # Use PU extent for bounding box (more precise for aggregation)
    pu_ext <- terra::ext(pus)
    corners <- sf::st_sfc(
      sf::st_point(c(pu_ext$xmin, pu_ext$ymin)),
      sf::st_point(c(pu_ext$xmax, pu_ext$ymax)),
      crs = terra::crs(pus, proj = TRUE)
    )
    corners_wgs84 <- sf::st_transform(corners, crs = 4326)
    coords <- sf::st_coordinates(corners_wgs84)
    ee_bounding_box <- ee$Geometry$Rectangle(
      c(coords[1, "X"], coords[1, "Y"], coords[2, "X"], coords[2, "Y"]),
      proj = "EPSG:4326",
      geodesic = FALSE
    )
    log_message("Using PU extent for GEE bounding box")

    # Filter to year and bounds
    start_date <- ee$Date(glue::glue("{year}-01-01"))
    end_date <- ee$Date(glue::glue("{year}-12-31"))
    filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)

    # Check if any images exist for this year
    image_count <- filtered_data$size()$getInfo()

    if (image_count == 0) {
      log_message("No {lulc_product} data available for {year}, finding most recent year...")

      # Find the most recent year with data by searching backwards from current year
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      latest_year <- NULL

      for (test_year in seq(current_year, 2017, by = -1)) {
        test_start <- ee$Date(glue::glue("{test_year}-01-01"))
        test_end <- ee$Date(glue::glue("{test_year}-12-31"))
        test_count <- ic$filterDate(test_start, test_end)$filterBounds(ee_bounding_box)$size()$getInfo()
        if (test_count > 0) {
          latest_year <- test_year
          break
        }
      }

      if (is.null(latest_year)) {
        stop(glue::glue("No {lulc_product} data available for the specified region"), call. = FALSE)
      }

      log_message("Using most recent available year: {latest_year}")

      # Update year
      year <- latest_year

      # Re-filter with the correct year
      start_date <- ee$Date(glue::glue("{year}-01-01"))
      end_date <- ee$Date(glue::glue("{year}-12-31"))
      filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)
    }

    # Create composite (mosaic for ESRI/ESA, mode for Dynamic World)
    composite <- if (lulc_product == "dynamic_world") {
      filtered_data$mode()
    } else {
      filtered_data$mosaic()
    }

    # Select band if needed (download_lulc_class_proportion)
    if (!is.null(band)) {
      composite <- composite$select(band)
    }

    # Create binary mask for target class(es)
    # For single class value
    if (length(class_values) == 1) {
      binary_mask <- composite$eq(class_values)
    } else {
      # For multiple class values, create OR mask
      binary_mask <- composite$eq(class_values[1])
      for (cv in class_values[-1]) {
        binary_mask <- binary_mask$Or(composite$eq(cv))
      }
    }

    # Convert to float for mean aggregation
    binary_mask <- binary_mask$toFloat()

    # Extract CRS and transform from planning units (WKT format required by GEE)
    pu_crs <- get_crs_wkt(pus)
    pu_ext <- terra::ext(pus)
    pu_res <- terra::res(pus)
    crs_transform <- c(pu_ext$xmin, pu_res[1], 0, pu_ext$ymax, 0, -pu_res[2])

    # Determine native scale for the LULC product
    native_scale <- switch(
      lulc_product,
      esri_10m = 10,
      dynamic_world = 10,
      esa_worldcover = 10
    )

    log_message("Aggregating to PU resolution ({pu_res[1]}m) using mean reducer...")

    # Clip to PU extent first to limit data
    # ee_bounding_box was already set to PU extent above (in WGS84)
    binary_mask_clipped <- binary_mask$clip(ee_bounding_box)

    # Reduce resolution in WGS84 at PU-equivalent scale (in meters)
    # GEE's atScale() handles meter-to-degree conversion internally
    intermediate_projection <- ee$Projection("EPSG:4326")$atScale(pu_res[1])

    proportion_raster <- binary_mask_clipped$reproject(
      crs = intermediate_projection
    )$reduceResolution(
      reducer = ee$Reducer$mean(),
      maxPixels = reticulate::r_to_py(65536L)
    )

    # Export will handle final reprojection to PU CRS via crs and crsTransform params

    # Build filename
    file_name <- glue::glue("{file_prefix}_{year}_{iso3}")
    output_file <- file.path(output_dir, glue::glue("{file_name}.tif"))

    # Check if file already exists
    if (file.exists(output_file)) {
      log_message("File already exists locally: {output_file}")
      cleanup_earthengine(env_info)
      result <- terra::rast(output_file)
      attr(result, "pre_aggregated") <- TRUE
      attr(result, "class_name") <- class_name
      attr(result, "lulc_product") <- lulc_product
      return(result)
    }

    # Prepare export parameters
    export_params <- list(
      image = proportion_raster,
      description = file_name,
      folder = NULL,  # Export to Drive root
      fileNamePrefix = file_name,
      region = ee_bounding_box$getInfo()[["coordinates"]],
      crs = pu_crs,
      crsTransform = crs_transform,
      maxPixels = reticulate::r_to_py(1e13),
      fileFormat = "GeoTIFF"
    )

    # Submit export task
    task <- do.call(ee$batch$Export$image$toDrive, export_params)
    task$start()
    log_message("Export task started. Waiting for files to appear in Google Drive...")
    log_message("Monitor task progress at: https://code.earthengine.google.com/tasks")

    # Wait for files to appear in Drive
    start_time <- Sys.time()
    repeat {
      Sys.sleep(30)  # Check every 30 seconds

      # Search for files in Drive
      drive_files <- googledrive::drive_find(
        pattern = file_name,
        type = "tif"
      )

      if (nrow(drive_files) > 0) {
        log_message("Found {nrow(drive_files)} file(s) in Google Drive.")
        break
      }

      # Check timeout
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      if (elapsed > wait_time) {
        cleanup_earthengine(env_info)
        unlink(temp_dir, recursive = TRUE)
        stop(
          glue::glue(
            "Timed out waiting for GEE export after {wait_time} minutes.\n",
            "Check task status at: https://code.earthengine.google.com/tasks"
          ),
          call. = FALSE
        )
      }

      log_message("Still waiting for export... ({round(elapsed, 1)} min) - check: https://code.earthengine.google.com/tasks")
    }

    # Download files from Drive
    for (i in seq_len(nrow(drive_files))) {
      local_path <- file.path(temp_dir, drive_files$name[i])
      googledrive::drive_download(drive_files$id[i], path = local_path, overwrite = TRUE)
      googledrive::drive_trash(drive_files$id[i])
    }

    # Merge tiles if multiple
    output_raster <- merge_tiles(temp_dir, output_file, "FLT4S")

    # Cleanup
    unlink(temp_dir, recursive = TRUE)
    cleanup_earthengine(env_info)

    # Set attributes
    names(output_raster) <- glue::glue("{class_name}_proportion")
    attr(output_raster, "pre_aggregated") <- TRUE
    attr(output_raster, "class_name") <- class_name
    attr(output_raster, "lulc_product") <- lulc_product

    return(output_raster)

  }, error = function(e) {
    cleanup_earthengine(env_info)
    unlink(temp_dir, recursive = TRUE)
    stop(glue::glue("GEE export failed: {e$message}"), call. = FALSE)
  })
}


#' Download LULC Class Proportions from GEE
#'
#' Downloads individual proportion rasters from Google Earth Engine where each
#' raster represents the proportion (0-1) of a specific LULC class within each
#' planning unit cell. This is more efficient than downloading categorical LULC
#' and computing proportions locally, as aggregation happens in GEE.
#'
#' @inheritParams download_gee_layer
#' @param lulc_product Character. LULC product to use: "esri_10m" (default),
#'   "dynamic_world", or "esa_worldcover".
#' @param class_names Character vector. Names of classes to extract proportions for.
#'   Default is c("agriculture", "built_area"). Must be valid class names for
#'   the chosen `lulc_product` (see [get_lulc_classes()]).
#' @param year Numeric or NULL. Year for LULC data. If NULL, uses most recent.
#' @param pus SpatRaster. Planning units raster (required for resolution).
#' @param force_download Logical. If TRUE, skip local and Drive cache checks and
#'   force a new export from GEE. Default is FALSE.
#'
#' @return A named list of `SpatRaster` objects, one per class. Each raster contains
#'   proportion values (0-1) at PU resolution. Files are also saved to `output_dir`
#'   with names like `{product}_proportion_{year}_{iso3}_{class}.tif`.
#'
#' @details
#' This function:
#' 1. Loads the LULC ImageCollection in GEE
#' 2. For each class, creates a binary mask (class = 1, other = 0)
#' 3. Exports each class proportion as a separate GeoTIFF
#' 4. Reprojects locally to PU CRS using bilinear interpolation
#'
#' The resulting rasters can be used directly in consumer functions:
#' - Use `props$agriculture` for `agricultural_areas_input`
#' - Use `props$built_area` for `built_areas_input`
#' - Values are already 0-1 proportions, ready for threshold comparison
#'
#' @seealso [download_lulc_data()], [get_lulc_classes()], [download_lulc_class_proportion()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download agriculture and built_area proportions
#' props <- download_lulc_proportions(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-project",
#'   lulc_product = "esri_10m",
#'   class_names = c("agriculture", "built_area"),
#'   pus = planning_units
#' )
#'
#' # Use directly in make_protect_zone
#' protect <- make_protect_zone(
#'   iso3 = "GHA",
#'   pus = planning_units,
#'   agricultural_areas_input = props$agriculture,
#'   built_areas_input = props$built_area,
#'   ...
#' )
#' }
download_lulc_proportions <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    lulc_product = c("esri_10m", "dynamic_world", "esa_worldcover"),
    class_names = c("agriculture", "built_area"),
    year = NULL,
    pus,
    wait_time = 30,
    force_download = FALSE
) {
  lulc_product <- match.arg(lulc_product)

  # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )
  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )
  assertthat::assert_that(
    !is.null(gee_project) && nchar(gee_project) > 0,
    msg = "'gee_project' is required."
  )
  assertthat::assert_that(
    inherits(pus, "SpatRaster"),
    msg = "'pus' must be a SpatRaster object."
  )
  assertthat::assert_that(
    is.character(class_names) && length(class_names) > 0,
    msg = "'class_names' must be a non-empty character vector."
  )

  # Validate all class names exist for this product
  available_classes <- names(get_lulc_classes(lulc_product))
  invalid_classes <- setdiff(class_names, available_classes)
  if (length(invalid_classes) > 0) {
    stop(
      glue::glue(
        "Invalid class name(s) for product '{lulc_product}': {paste(invalid_classes, collapse=', ')}. ",
        "Available classes: {paste(available_classes, collapse=', ')}"
      ),
      call. = FALSE
    )
  }

  log_message("Downloading LULC proportions ({lulc_product}): {paste(class_names, collapse=', ')}")

  # Get asset ID and band for the product
  asset_id <- switch(
    lulc_product,
    esri_10m = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
    dynamic_world = "GOOGLE/DYNAMICWORLD/V1",
    esa_worldcover = "ESA/WorldCover/v200"
  )

  band <- switch(
    lulc_product,
    esri_10m = NULL,
    dynamic_world = "label",
    esa_worldcover = "Map"
  )

  # Default to previous year if not specified
  if (is.null(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    if (lulc_product == "esa_worldcover") {
      year <- 2021
    }
  }

  # Build file prefix for individual class files
  file_prefix <- glue::glue("{lulc_product}_proportion_{year}_{iso3}")

  # Check if all class files already exist locally
  existing_rasters <- list()
  all_exist <- TRUE
  for (cn in class_names) {
    class_file <- file.path(output_dir, glue::glue("{file_prefix}_{cn}.tif"))
    if (file.exists(class_file) && !force_download) {
      existing_rasters[[cn]] <- class_file
    } else {
      all_exist <- FALSE
    }
  }

  if (all_exist && !force_download) {
    log_message("All {length(class_names)} class proportion files already exist locally")
    result <- lapply(existing_rasters, function(f) {
      r <- terra::rast(f)
      attr(r, "pre_aggregated") <- TRUE
      attr(r, "lulc_product") <- lulc_product
      r
    })
    names(result) <- class_names
    return(result)
  }

  # Initialize GEE
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required but not installed.", call. = FALSE)
  }

  env_info <- initialize_earthengine(gee_project)
  ee <- env_info$ee

  # Ensure googledrive is authenticated before we suppress messages later

  # This allows the account selection prompt to appear if needed
  if (!googledrive::drive_has_token()) {
    googledrive::drive_auth()
  }

  # Create temporary directory for downloads
  temp_dir <- file.path(Sys.getenv("HOME"), glue::glue("gee_props_{iso3}_{Sys.getpid()}"))
  dir.create(temp_dir, showWarnings = FALSE)

  tryCatch({
    # Load the ImageCollection
    ic <- ee$ImageCollection(asset_id)

    # Use PU extent for bounding box
    pu_ext <- terra::ext(pus)
    pu_res <- terra::res(pus)
    corners <- sf::st_sfc(
      sf::st_point(c(pu_ext$xmin, pu_ext$ymin)),
      sf::st_point(c(pu_ext$xmax, pu_ext$ymax)),
      crs = terra::crs(pus, proj = TRUE)
    )
    corners_wgs84 <- sf::st_transform(corners, crs = 4326)
    coords <- sf::st_coordinates(corners_wgs84)
    ee_bounding_box <- ee$Geometry$Rectangle(
      c(coords[1, "X"], coords[1, "Y"], coords[2, "X"], coords[2, "Y"]),
      proj = "EPSG:4326",
      geodesic = FALSE
    )

    # Filter to year and bounds
    start_date <- ee$Date(glue::glue("{year}-01-01"))
    end_date <- ee$Date(glue::glue("{year}-12-31"))
    filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)

    # Check if any images exist for this year
    image_count <- filtered_data$size()$getInfo()

    if (image_count == 0) {
      log_message("No {lulc_product} data available for {year}, finding most recent year...")

      # Find the most recent year with data by searching backwards from current year
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      latest_year <- NULL

      for (test_year in seq(current_year, 2017, by = -1)) {
        test_start <- ee$Date(glue::glue("{test_year}-01-01"))
        test_end <- ee$Date(glue::glue("{test_year}-12-31"))
        test_count <- ic$filterDate(test_start, test_end)$filterBounds(ee_bounding_box)$size()$getInfo()
        if (test_count > 0) {
          latest_year <- test_year
          break
        }
      }

      if (is.null(latest_year)) {
        stop(glue::glue("No {lulc_product} data available for the specified region"), call. = FALSE)
      }

      log_message("Using most recent available year: {latest_year}")

      # Update year and file prefix
      year <- latest_year
      file_prefix <- glue::glue("{lulc_product}_proportion_{year}_{iso3}")

      # Re-filter with the correct year
      start_date <- ee$Date(glue::glue("{year}-01-01"))
      end_date <- ee$Date(glue::glue("{year}-12-31"))
      filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)
    }

    # Create composite (mosaic for ESRI/ESA, mode for Dynamic World)
    composite <- if (lulc_product == "dynamic_world") {
      filtered_data$mode()
    } else {
      filtered_data$mosaic()
    }

    # Select band if needed (download_lulc_proportions)
    if (!is.null(band)) {
      composite <- composite$select(band)
    }

    # Get class values for each class name
    class_values_list <- lapply(class_names, function(cn) {
      get_lulc_class_value(lulc_product, cn)
    })
    names(class_values_list) <- class_names

    # Get native scale based on LULC product (known values)
    # ESRI 10m and ESA WorldCover are 10m, Dynamic World is ~10m
    native_scale_m <- switch(lulc_product,
      "esri_10m" = 10,
      "esa_worldcover" = 10,
      "dynamic_world" = 10,
      10  # default
    )

    # Track export tasks
    export_tasks <- list()

    for (cn in class_names) {
      class_vals <- class_values_list[[cn]]

      # Check if this class file already exists locally
      output_file <- file.path(output_dir, glue::glue("{file_prefix}_{cn}.tif"))
      if (file.exists(output_file) && !force_download) {
        export_tasks[[cn]] <- list(status = "exists_local", file = output_file)
        next
      }

      # Create binary mask for this class
      if (length(class_vals) == 1) {
        class_mask <- composite$eq(class_vals)$toFloat()
      } else {
        class_mask <- composite$eq(class_vals[1])
        for (v in class_vals[-1]) {
          class_mask <- class_mask$Or(composite$eq(v))
        }
        class_mask <- class_mask$toFloat()
      }

      # Set the default projection with EPSG:4326 and native scale
      # This is required for reduceResolution to work properly
      class_mask <- class_mask$setDefaultProjection(crs = "EPSG:4326", scale = native_scale_m)

      # Aggregate to PU resolution using mean reducer to get true proportions
      # This computes the fraction of source pixels that are this class
      target_proj <- ee$Projection("EPSG:4326")$atScale(pu_res[1])
      class_mask <- class_mask$reduceResolution(
        reducer = ee$Reducer$mean(),
        bestEffort = TRUE
      )$reproject(crs = target_proj)

      # File name for GEE export (without path)
      gee_file_name <- glue::glue("{file_prefix}_{cn}")

      # Check for existing file in Drive (unless force_download)
      existing_in_drive <- data.frame(name = character(0))
      if (!force_download) {
        existing_in_drive <- tryCatch({
          # Suppress googledrive messages about files not found
          suppressMessages(googledrive::drive_get(paste0(gee_file_name, ".tif")))
        }, error = function(e) {
          data.frame(name = character(0))
        })
      }

      if (nrow(existing_in_drive) > 0 && !force_download) {
        log_message("Found '{cn}' in Drive, will download")
        export_tasks[[cn]] <- list(status = "exists_drive", drive_file = existing_in_drive,
                                   output_file = output_file, gee_file_name = gee_file_name)
      } else {
        log_message("'{cn}' not found locally or in Drive, will export from GEE")
        # Export this class
        export_params <- list(
          image = class_mask,
          description = gee_file_name,
          folder = NULL,
          fileNamePrefix = gee_file_name,
          region = ee_bounding_box$getInfo()[["coordinates"]],
          crs = "EPSG:4326",
          scale = pu_res[1],
          maxPixels = reticulate::r_to_py(1e13),
          fileFormat = "GeoTIFF"
        )

        task <- do.call(ee$batch$Export$image$toDrive, export_params)
        task$start()
        export_tasks[[cn]] <- list(status = "exporting", task = task,
                                   output_file = output_file, gee_file_name = gee_file_name)
      }
    }

    # Count how many need downloading
    need_download <- sum(sapply(export_tasks, function(x) x$status != "exists_local"))
    if (need_download > 0) {
      log_message("Export tasks started ({need_download} classes)")
    }
    start_time <- Sys.time()
    result_rasters <- list()

    for (cn in class_names) {
      task_info <- export_tasks[[cn]]

      # If already exists locally, just load it
      if (task_info$status == "exists_local") {
        r <- terra::rast(task_info$file)
        attr(r, "pre_aggregated") <- TRUE
        attr(r, "lulc_product") <- lulc_product
        result_rasters[[cn]] <- r
        next
      }

      output_file <- task_info$output_file
      gee_file_name <- task_info$gee_file_name
      temp_file <- file.path(temp_dir, paste0(gee_file_name, ".tif"))

      # If exists in Drive, download it
      if (task_info$status == "exists_drive") {
        googledrive::drive_download(task_info$drive_file[1, ], path = temp_file, overwrite = TRUE)
      } else {
        # Wait for export to complete
        repeat {
          all_files <- googledrive::drive_find(
            q = paste0("name contains '", gee_file_name, "'")
          )
          matching <- all_files[grepl(paste0("^", gee_file_name, ".*\\.tif$"), all_files$name), ]

          if (nrow(matching) > 0) {
            googledrive::drive_download(matching[1, ], path = temp_file, overwrite = TRUE)
            break
          }

          elapsed <- difftime(Sys.time(), start_time, units = "mins")
          if (elapsed > wait_time) {
            cleanup_earthengine(env_info)
            unlink(temp_dir, recursive = TRUE)
            stop(glue::glue(
              "GEE export timeout: '{cn}' not available after {as.integer(wait_time)} min.\n",
              "The export is still running in GEE. Please:\n",
              "  1. Check status at: https://code.earthengine.google.com/tasks\n",
              "  2. Wait for the export to complete\n",
              "  3. Run the pipeline again - it will download the completed file"
            ), call. = FALSE)
          }

          log_message("Waiting for export... ({round(elapsed, 1)} min)")
          Sys.sleep(30)
        }
      }

      # Reproject to PU CRS and save
      r <- terra::rast(temp_file)
      r <- terra::project(r, pus, method = "bilinear")
      terra::writeRaster(r, output_file, overwrite = TRUE,
                         filetype = "COG", gdal = c("COMPRESS=DEFLATE"))
      log_message("Downloaded: {cn}")

      attr(r, "pre_aggregated") <- TRUE
      attr(r, "lulc_product") <- lulc_product
      result_rasters[[cn]] <- r
    }

    # Cleanup
    unlink(temp_dir, recursive = TRUE)
    cleanup_earthengine(env_info)

    return(result_rasters)

  }, error = function(e) {
    cleanup_earthengine(env_info)
    unlink(temp_dir, recursive = TRUE)
    stop(glue::glue("GEE export failed: {e$message}"), call. = FALSE)
  })
}


#' Download LULC Data from Multiple Sources
#'
#' Unified interface for downloading Land Use/Land Cover (LULC) data from various
#' sources including ESRI Global LULC, Google Dynamic World, ESA WorldCover, or
#' loading local files.
#'
#' @param boundary_layer An `sf` object defining the spatial boundary of interest.
#' @param iso3 Character. Three-letter ISO country code.
#' @param product Character. LULC product to download: "esri_10m" (default),
#'   "dynamic_world", "esa_worldcover", or "local".
#' @param gee_project Character or NULL. Google Earth Engine cloud project ID.
#'   Required for GEE-sourced products (esri_10m, dynamic_world, esa_worldcover).
#' @param output_dir Character. Directory for saving output. Defaults to `here::here()`.
#' @param local_file Character or NULL. Path to local GeoTIFF/COG file.
#'   Required when `product = "local"`.
#' @param year Numeric or NULL. Specific year to download. If NULL, uses most recent.
#'   For Dynamic World, this creates an annual mode composite. Ignored for ESA WorldCover
#'   (fixed at 2021).
#' @param pus SpatRaster or NULL. Planning units raster for GEE-side aggregation.
#' @param aggregate_to_pus Logical. If TRUE and `pus` is provided, aggregate the
#'   data to planning unit resolution in GEE before export. Default is FALSE.
#' @param wait_time Numeric. Maximum wait time for GEE exports in minutes. Default is 5.
#' @param ... Additional arguments passed to underlying download functions.
#'
#' @return A `SpatRaster` object of the LULC data with attribute `lulc_product`
#'   indicating the source, or NULL if download failed.
#'
#' @seealso [get_lulc_classes()], [get_lulc_class_value()], [list_lulc_products()],
#'   [download_esri_lulc_data()], [download_dynamic_world_data()],
#'   [download_esa_worldcover_data()], [load_local_lulc_data()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Download ESRI LULC (default)
#' lulc <- download_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-project"
#' )
#'
#' # Download Dynamic World annual composite
#' lulc_dw <- download_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   product = "dynamic_world",
#'   gee_project = "my-project",
#'   year = 2023
#' )
#'
#' # Download ESA WorldCover
#' lulc_esa <- download_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   product = "esa_worldcover",
#'   gee_project = "my-project"
#' )
#'
#' # Use local file
#' lulc_local <- download_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   product = "local",
#'   local_file = "path/to/custom_lulc.tif"
#' )
#'
#' # Download with GEE-side aggregation to planning units
#' lulc_agg <- download_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   product = "esri_10m",
#'   gee_project = "my-project",
#'   pus = planning_units,
#'   aggregate_to_pus = TRUE
#' )
#' }
download_lulc_data <- function(
    boundary_layer,
    iso3,
    product = c("esri_10m", "dynamic_world", "esa_worldcover", "local"),
    gee_project = NULL,
    output_dir = here::here(),
    local_file = NULL,
    year = NULL,
    pus = NULL,
    aggregate_to_pus = FALSE,
    wait_time = 5,
    ...
) {
  product <- match.arg(product)

  # Input validation
  assertthat::assert_that(
    inherits(boundary_layer, "sf"),
    msg = "'boundary_layer' must be an sf object."
  )
  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code."
  )

  # Validate GEE project for GEE-based products
  if (product != "local" && (is.null(gee_project) || nchar(gee_project) == 0)) {
    stop(
      glue::glue("'gee_project' is required for product '{product}'."),
      call. = FALSE
    )
  }

  # Validate local_file for local product
  if (product == "local" && is.null(local_file)) {
    stop("'local_file' is required when product = 'local'.", call. = FALSE)
  }

  # Route to appropriate handler
  result <- switch(
    product,
    esri_10m = download_esri_lulc_data(
      boundary_layer = boundary_layer,
      iso3 = iso3,
      gee_project = gee_project,
      output_dir = output_dir,
      pus = pus,
      aggregate_to_pus = aggregate_to_pus,
      wait_time = wait_time,
      ...
    ),
    dynamic_world = download_dynamic_world_data(
      boundary_layer = boundary_layer,
      iso3 = iso3,
      gee_project = gee_project,
      output_dir = output_dir,
      year = year,
      pus = pus,
      aggregate_to_pus = aggregate_to_pus,
      wait_time = wait_time,
      ...
    ),
    esa_worldcover = download_esa_worldcover_data(
      boundary_layer = boundary_layer,
      iso3 = iso3,
      gee_project = gee_project,
      output_dir = output_dir,
      pus = pus,
      aggregate_to_pus = aggregate_to_pus,
      wait_time = wait_time,
      ...
    ),
    local = load_local_lulc_data(
      local_file = local_file,
      boundary_layer = boundary_layer
    )
  )

  # Ensure lulc_product attribute is set
  if (!is.null(result) && is.null(attr(result, "lulc_product"))) {
    attr(result, "lulc_product") <- product
  }

  return(result)
}


#' Check and download GEE-sourced layers based on metadata requirements
#'
#' This function checks for required layer names from metadata, ensures they exist
#' in the output path, and optionally prompts the user to download them from GEE
#' if missing. Used internally by other functions to automatically download
#' required datasets.
#'
#' @param data_info Data.frame containing `data_name` fields to match against requirements.
#' @param iso3 Character. ISO3 country code for the target country.
#' @param input_path Character. Path where data files should exist or be downloaded to.
#' @param gee_project Character. Google Earth Engine project ID.
#' @param boundary_proj An `sf` object representing the country boundary.
#' @param wait_time Numeric. Wait time (in minutes) for Drive export. Default is 5.
#' @param interactive Logical. If TRUE (default), prompts user before downloading.
#'   If FALSE, automatically downloads missing layers without prompting. Set to
#'   FALSE for non-interactive sessions (e.g., batch jobs, CI/CD pipelines).
#' @param lulc_product Character. LULC product to download: "esri_10m" (default),
#'   "dynamic_world", or "esa_worldcover". Affects which product is downloaded
#'   when LULC data is required.
#' @param pus SpatRaster or NULL. Planning units for GEE-side aggregation.
#' @param aggregate_to_pus Logical. If TRUE and `pus` provided, aggregate in GEE
#'   before download. Default is FALSE.
#' @param skip_lulc Logical. If TRUE, skip downloading categorical LULC data.
#'   Use when LULC proportions will be downloaded separately via
#'   [download_lulc_proportions()]. Default is FALSE.
#'
#' @return Invisible NULL. Files are downloaded as a side effect.
#' @export
#' @examples
#' \dontrun{
#' # Interactive mode (default) - prompts user
#' check_and_download_required_layers(
#'   data_info = metadata_df,
#'   iso3 = "GHA",
#'   input_path = "/path/to/data",
#'   gee_project = "my-project",
#'   boundary_proj = ghana_boundary
#' )
#'
#' # Non-interactive mode - auto-downloads without prompts
#' check_and_download_required_layers(
#'   data_info = metadata_df,
#'   iso3 = "GHA",
#'   input_path = "/path/to/data",
#'   gee_project = "my-project",
#'   boundary_proj = ghana_boundary,
#'   interactive = FALSE
#' )
#'
#' # Use Dynamic World instead of ESRI LULC
#' check_and_download_required_layers(
#'   data_info = metadata_df,
#'   iso3 = "GHA",
#'   input_path = "/path/to/data",
#'   gee_project = "my-project",
#'   boundary_proj = ghana_boundary,
#'   lulc_product = "dynamic_world"
#' )
#' }
check_and_download_required_layers <- function(
    data_info,
    iso3,
    input_path,
    gee_project,
    boundary_proj,
    wait_time = 5,
    interactive = TRUE,
    lulc_product = c("esri_10m", "dynamic_world", "esa_worldcover"),
    pus = NULL,
    aggregate_to_pus = FALSE,
    skip_lulc = FALSE
) {
  lulc_product <- match.arg(lulc_product)
  required_layers <- unique(data_info$data_name)

  # Define available GEE layers and their requirements
  gee_layers <- tibble::tibble(
    name = c("LULC", "Pasturelands"),
    # File patterns to search for in local directory
    # LULC pattern matches: lulc_{ISO3}.tif, esri_10m_lulc_2023_AND.tif, dynamic_world_2023_AND.tif, etc.
    pattern = c(
      paste0("(lulc|esri_10m_lulc|dynamic_world|esa_worldcover).*", iso3, "\\.tif$"),
      paste0("(grassland|gpw_cultiv).*", iso3, "\\.tif$")
    ),
    # Data names that require each layer
    required_if = list(
      c("Urban Greening Opportunities", "Restore Zone", "Protect Zone", "Agriculture Areas", "Urban Areas"),
      c("Pasturelands")
    ),
    # Download functions for each layer - now uses download_lulc_data for flexible product selection
    download_fun = list(
      function() elsar::download_lulc_data(
        boundary_layer = boundary_proj,
        iso3 = iso3,
        product = lulc_product,
        gee_project = gee_project,
        output_dir = input_path,
        pus = pus,
        aggregate_to_pus = aggregate_to_pus,
        wait_time = wait_time
      ),
      function() elsar::download_global_pasture_data(
        boundary_layer = boundary_proj,
        iso3 = iso3,
        gee_project = gee_project,
        output_dir = input_path,
        wait_time = wait_time
      )
    )
  )

  # Check each GEE layer to see if it's required and available
  for (i in seq_len(nrow(gee_layers))) {
    # Skip LULC if requested (when using proportions instead)
    if (skip_lulc && gee_layers$name[i] == "LULC") {
      next
    }

    if (any(required_layers %in% gee_layers$required_if[[i]])) {
      # This layer is required - check if we have it locally
      all_dat <- list.files(input_path)
      has_data <- any(grepl(gee_layers$pattern[i], all_dat, ignore.case = TRUE) & grepl(iso3, all_dat, ignore.case = TRUE))

      if (!has_data) {
        log_message("No {gee_layers$name[i]} data found locally for {iso3}. Will attempt GEE download...")

        if (interactive && base::interactive()) {
          # Interactive mode: prompt user for confirmation
          answer <- readline(glue::glue("Do you want to download {gee_layers$name[i]} data from GEE? [yes/no, default=yes]: "))
          answer <- tolower(trimws(answer))
          if (answer == "") answer <- "yes"

          if (answer %in% c("yes", "y")) {
            answer2 <- readline("Do you have GEE access and an internet connection? [yes/no, default=yes]: ")
            answer2 <- tolower(trimws(answer2))
            if (answer2 == "") answer2 <- "yes"

            if (answer2 %in% c("yes", "y")) {
              gee_layers$download_fun[[i]]()
            } else {
              message(glue::glue("Cannot proceed without GEE access for {gee_layers$name[i]}."))
            }
          } else {
            message(glue::glue("Skipping {gee_layers$name[i]}. Ensure it is provided manually."))
          }
        } else {
          # Non-interactive mode: auto-download without prompting
          tryCatch({
            gee_layers$download_fun[[i]]()
          }, error = function(e) {
            warning(
              glue::glue("Failed to download {gee_layers$name[i]} for {iso3}: {e$message}"),
              call. = FALSE
            )
          })
        }
      }
    }
  }

  invisible(NULL)
}

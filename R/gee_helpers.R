# GEE Download Helpers
#
# This file contains functions for downloading raster data from Google Earth Engine (GEE)
# and processing it into Cloud-Optimized GeoTIFFs (COGs). The functions handle authentication,
# export management, file downloading, and data processing.

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

  log_message("Checking Python environment for Earth Engine API...")

  # Handle Python environment setup - check if RETICULATE_PYTHON is set
  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)

    # Check if earthengine-api is available in existing environment
    if (reticulate::py_module_available("ee")) {
      log_message("Using existing Python environment with Earth Engine API installed.")
      temp_env <- NULL  # No temporary environment needed
    } else {
      log_message("Earth Engine not available in RETICULATE_PYTHON environment. Creating temporary conda env.")
      temp_env <- paste0("gee_temp_env_", Sys.getpid())
      reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
      reticulate::use_condaenv(temp_env, required = TRUE)
      log_message(glue::glue("Temporary Conda environment created: {temp_env}"))
    }
  } else {
    # No RETICULATE_PYTHON set - create new temporary environment
    log_message("RETICULATE_PYTHON not set. Creating temporary conda environment.")
    temp_env <- paste0("gee_temp_env_", Sys.getpid())
    reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
    reticulate::use_condaenv(temp_env, required = TRUE)
    log_message(glue::glue("Temporary Conda environment created: {temp_env}"))
  }

  # Import Earth Engine module
  ee <- reticulate::import("ee")

  # Handle authentication - only authenticate if credentials don't exist
  cred_path <- file.path(rappdirs::user_config_dir("earthengine"), "credentials")
  if (!file.exists(cred_path)) {
    log_message("No Earth Engine credentials found. Starting authentication...")
    ee$Authenticate()
  }

  # Initialize Earth Engine with specified project
  ee$Initialize(project = gee_project)
  log_message("Google Earth Engine initialized.")

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
    log_message(glue::glue("Cleaning up temporary conda environment: {env_info$temp_env}"))
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
  # List all files in the specified Drive folder
  files <- tryCatch(
    googledrive::drive_ls(path = drive_folder),
    error = function(e) {
      log_message(glue::glue("Error accessing Google Drive folder: {e$message}"))
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
    log_message(glue::glue("No .tif files matching prefix '{file_prefix}' found"))
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
      log_message(glue::glue("Downloaded: {matching_files$name[i]}"))
      TRUE
    }, error = function(e) {
      log_message(glue::glue("Failed to download {matching_files$name[i]}: {e$message}"))
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
  # Find all .tif files in the local directory
  tile_files <- list.files(local_path, pattern = "\\.tif$", full.names = TRUE)

  if (length(tile_files) == 0) {
    stop("No downloaded tiles found!", call. = FALSE)
  }

  log_message(glue::glue("Merging {length(tile_files)} tile(s) into final output: {output_file}"))

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
#' @param datatype Character. Output datatype (GDAL style), e.g., "INT1U" or "FLT4S".
#'   Default is "INT1U".
#' @param googledrive_folder Character or NULL. Google Drive folder name for exports.
#'   Currently defaults to NULL (Drive root) to avoid a GEE folder duplication bug.
#' @param wait_time Numeric. Maximum time in minutes to wait for the GEE export
#'   to appear in Google Drive. Default is 5. Increase for large exports.
#'
#' @return A `SpatRaster` object written to disk, or NULL if export timed out.
#'
#' @seealso [download_esri_lulc_data()], [download_global_pasture_data()]
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
    wait_time = 5
) {

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

  # Set up folder structure
  # NOTE: Currently exporting to Drive root to avoid GEE folder duplication bug
  # TODO: Revert to country-specific folders when GEE bug is fixed
  if (is.null(googledrive_folder)) {
    export_folder <- NULL  # Export to Drive root
    log_message("Exporting to Google Drive root (no folder) to avoid GEE folder duplication bug")
  } else {
    export_folder <- paste0(googledrive_folder, "_", iso3)
    log_message(glue::glue("Using country-specific folder: {export_folder}"))
  }

  # Create temporary local directory for downloads
  temp_dir <- file.path(Sys.getenv("HOME"), glue::glue("gee_download_{iso3}_{Sys.getpid()}"))
  dir.create(temp_dir, showWarnings = FALSE)
  log_message(glue::glue("Temporary directory created at: {temp_dir}"))

  # Initialize Earth Engine
  env_info <- initialize_earthengine(gee_project)
  ee <- env_info$ee

  # Load the dataset and find the most recent year with data
  ic <- ee$ImageCollection(asset_id)
  year <- as.numeric(format(Sys.Date(), "%Y")) - 1

  # Search backwards for a year with data
  while (ic$filterDate(ee$Date(glue::glue("{year}-01-01")), ee$Date(glue::glue("{year}-12-31")))$size()$getInfo() == 0) {
    log_message(glue::glue("No valid data found for {year}, trying {year - 1}..."))
    year <- year - 1
    # Prevent infinite loop - stop if we go too far back
    if (year < 2000) {
      stop("No data found for any recent year in the collection", call. = FALSE)
    }
  }
  log_message(glue::glue("Using data for year: {year}"))

  # Build export filename and check if local file already exists
  file_name <- glue::glue("{file_prefix}_{year}_{iso3}")
  output_file <- file.path(output_dir, glue::glue("{file_name}.tif"))

  if (file.exists(output_file)) {
    log_message(glue::glue("File already exists locally: {output_file}"))
    log_message("Loading existing file...")
    cleanup_earthengine(env_info)
    unlink(temp_dir, recursive = TRUE)
    return(terra::rast(output_file))
  }

  log_message(glue::glue("Checking Google Drive for existing files: {file_name}*"))

  # Simple folder creation function (only used if not exporting to root)
  ensure_drive_folder <- function(folder_name) {
    existing_folders <- googledrive::drive_ls(path = NULL, type = "folder")
    if (!(folder_name %in% existing_folders$name)) {
      log_message(glue::glue("Folder '{folder_name}' does not exist. Creating it in Google Drive..."))
      googledrive::drive_mkdir(folder_name)
    } else {
      log_message(glue::glue("Folder '{folder_name}' already exists in Google Drive."))
    }
  }

  # Ensure export folder exists (if not exporting to root)
  if (!is.null(export_folder)) {
    ensure_drive_folder(export_folder)
  }

  # Check for existing exported files
  if (is.null(export_folder)) {
    # Search in Drive root
    files <- googledrive::drive_ls(path = NULL)
  } else {
    # Search in specified folder
    files <- googledrive::drive_ls(path = export_folder)
  }

  existing_files <- files[grepl(paste0("^", file_name), files$name) & grepl("\\.tif$", files$name), ]

  if (nrow(existing_files) > 0) {
    log_message(glue::glue("Existing files for {iso3} found on Google Drive. Downloading instead of exporting from GEE..."))
  } else {
    log_message(glue::glue("No existing exported files found for {iso3} on Google Drive."))
    log_message("Checking for existing export tasks that are still running...")

    # Check for running export tasks with the same description
    tasks <- ee$batch$Task$list()
    existing_task <- purrr::detect(tasks, function(t) {
      t$status()$state %in% c("READY", "RUNNING") && t$status()$description == file_name
    })

    if (!is.null(existing_task)) {
      log_message(glue::glue("Existing export task '{file_name}' is still running. Waiting instead of starting a new one."))
    } else {
      log_message("No similar running tasks found. Proceeding with new GEE export.")

      # Prepare geometry for export - ensure it's in WGS84
      boundary_layer <- sf::st_transform(boundary_layer, crs = 4326)
      bounding_box <- sf::st_bbox(boundary_layer)
      ee_bounding_box <- ee$Geometry$Rectangle(
        c(bounding_box$xmin, bounding_box$ymin, bounding_box$xmax, bounding_box$ymax),
        proj = "EPSG:4326",
        geodesic = FALSE
      )

      # Filter collection to date range and geographic bounds, then mosaic
      start_date <- ee$Date(glue::glue("{year}-01-01"))
      end_date <- ee$Date(glue::glue("{year}-12-31"))
      filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)

      # Submit export task to Google Drive
      task <- ee$batch$Export$image$toDrive(
        image = filtered_data$mosaic(),
        description = file_name,
        folder = export_folder,  # NULL for root, folder name otherwise
        fileNamePrefix = file_name,
        scale = scale,
        region = ee_bounding_box$getInfo()[["coordinates"]],
        maxPixels = reticulate::r_to_py(1e13),  # Very large pixel limit
        fileFormat = "GeoTIFF"
      )
      task$start()
      log_message("Export task started. Waiting for files to appear in Google Drive...")
    }

    # Wait for files to appear in Drive
    start_time <- Sys.time()
    repeat {
      # Re-check for files (handles both new exports and existing running tasks)
      if (is.null(export_folder)) {
        files <- googledrive::drive_ls(path = NULL)
      } else {
        files <- googledrive::drive_ls(path = export_folder)
      }

      matching_files <- files[grepl(paste0("^", file_name), files$name) & grepl("\\.tif$", files$name), ]

      if (nrow(matching_files) > 0) {
        log_message("Files found. Proceeding with download.")
        break
      }

      # Check for timeout
      if (difftime(Sys.time(), start_time, units = "mins") > wait_time) {
        message(glue::glue(
          "Timeout: No files are yet available for download after {as.integer(wait_time)} minutes.\n",
          "This is not unexpected as GEE exports can take time. You can check the status of exports via the GEE web console.\n",
          "Please try running again later..."
        ))
        cleanup_earthengine(env_info)
        unlink(temp_dir, recursive = TRUE)
        return(NULL)
      }

      log_message("File not yet available, waiting 30 seconds before re-trying...")
      Sys.sleep(30)
    }
  }

  # Download files and process into final COG
  if (is.null(export_folder)) {
    download_from_drive(NULL, file_name, temp_dir)  # Download from root
  } else {
    download_from_drive(export_folder, file_name, temp_dir)  # Download from folder
  }

  output_raster <- merge_tiles(temp_dir, output_file, datatype)

  # Cleanup temporary files and environments
  unlink(temp_dir, recursive = TRUE)
  cleanup_earthengine(env_info)
  log_message("Temporary files and Conda environment deleted.")

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
#' @return A `SpatRaster` object of the downloaded LULC data, or NULL if download failed
#' @export
#' @examples
#' \dontrun{
#' # Download LULC data for Ghana
#' lulc <- download_esri_lulc_data(
#'   boundary_layer = ghana_boundary,
#'   iso3 = "GHA",
#'   gee_project = "my-gee-project"
#' )
#' }
download_esri_lulc_data <- function(boundary_layer, iso3, gee_project, output_dir = here::here(), ...) {
  result <- download_gee_layer(
    boundary_layer = boundary_layer,
    iso3 = iso3,
    gee_project = gee_project,
    output_dir = output_dir,
    asset_id = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
    file_prefix = "esri_10m_lulc",
    scale = 10,
    datatype = "INT1U",
    ...
  )

  # Set appropriate layer name for the result
  if (!is.null(result)) {
    names(result) <- "ESRI_Global-LULC_10m_TS"
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
#'
#' @return Invisible NULL. Files are downloaded as a side effect.
#' @keywords internal
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
#' }
check_and_download_required_layers <- function(
    data_info,
    iso3,
    input_path,
    gee_project,
    boundary_proj,
    wait_time = 5,
    interactive = TRUE
) {
  required_layers <- unique(data_info$data_name)

  # Define available GEE layers and their requirements
  gee_layers <- tibble::tibble(
    name = c("LULC", "Pasturelands"),
    # File patterns to search for in local directory
    pattern = c(
      paste0("lulc_.*_", iso3, "\\.tif$"),
      paste0("grassland_.*_", iso3, "\\.tif$")
    ),
    # Data names that require each layer
    required_if = list(
      c("Urban Greening Opportunities", "Restoration Zone", "Protection Zone", "Agriculture Areas", "Urban Areas"),
      c("Pasturelands")
    ),
    # Download functions for each layer
    download_fun = list(
      function() elsar::download_esri_lulc_data(boundary_layer = boundary_proj, iso3 = iso3, gee_project = gee_project, output_dir = input_path, wait_time = wait_time),
      function() elsar::download_global_pasture_data(boundary_layer = boundary_proj, iso3 = iso3, gee_project = gee_project, output_dir = input_path, wait_time = wait_time)
    )
  )

  # Check each GEE layer to see if it's required and available
  for (i in seq_len(nrow(gee_layers))) {
    if (any(required_layers %in% gee_layers$required_if[[i]])) {
      # This layer is required - check if we have it locally
      all_dat <- list.files(input_path)
      has_data <- any(grepl(gee_layers$pattern[i], all_dat, ignore.case = TRUE) & grepl(iso3, all_dat, ignore.case = TRUE))

      if (!has_data) {
        log_message(glue::glue("No {gee_layers$name[i]} data found for {iso3}."))

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
              log_message(glue::glue("Starting download of {gee_layers$name[i]} for {iso3}..."))
              gee_layers$download_fun[[i]]()
            } else {
              message(glue::glue("Cannot proceed without GEE access for {gee_layers$name[i]}."))
            }
          } else {
            message(glue::glue("Skipping {gee_layers$name[i]}. Ensure it is provided manually."))
          }
        } else {
          # Non-interactive mode: auto-download without prompting
          log_message(glue::glue("Non-interactive mode: automatically downloading {gee_layers$name[i]} for {iso3}..."))
          tryCatch({
            gee_layers$download_fun[[i]]()
          }, error = function(e) {
            warning(
              glue::glue("Failed to download {gee_layers$name[i]} for {iso3}: {e$message}"),
              call. = FALSE
            )
          })
        }
      } else {
        log_message(glue::glue("Found existing {gee_layers$name[i]} data for {iso3}, skipping download."))
      }
    }
  }

  invisible(NULL)
}

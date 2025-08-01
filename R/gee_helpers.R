#' Initialize Google Earth Engine
#'
#' Sets up Python environment and initializes the Earth Engine API.
#' Handles authentication and temporary conda environments as needed.
#'
#' @param gee_project GEE cloud project ID
#' @return A list containing the `ee` module and temporary environment name (if created)
#' @keywords internal
initialize_earthengine <- function(gee_project = "unbl-misc") {

  log_msg("Checking Python environment for Earth Engine API...")

  # Handle Python environment setup
  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)

    if (reticulate::py_module_available("ee")) {
      log_msg("Using existing Python environment with Earth Engine API installed.")
      temp_env <- NULL
    } else {
      log_msg("Earth Engine not available in RETICULATE_PYTHON environment. Creating temporary conda env.")
      temp_env <- paste0("gee_temp_env_", Sys.getpid())
      reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
      reticulate::use_condaenv(temp_env, required = TRUE)
      log_msg(glue::glue("Temporary Conda environment created: {temp_env}"))
    }
  } else {
    log_msg("RETICULATE_PYTHON not set. Creating temporary conda environment.")
    temp_env <- paste0("gee_temp_env_", Sys.getpid())
    reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
    reticulate::use_condaenv(temp_env, required = TRUE)
    log_msg(glue::glue("Temporary Conda environment created: {temp_env}"))
  }

  # Import Earth Engine
  ee <- reticulate::import("ee")

  # Handle authentication
  cred_path <- file.path(rappdirs::user_config_dir("earthengine"), "credentials")
  if (!file.exists(cred_path)) {
    log_msg("No Earth Engine credentials found. Starting authentication...")
    ee$Authenticate()
  }

  # Initialize with project
  ee$Initialize(project = gee_project)
  log_msg("Google Earth Engine initialized.")

  return(list(ee = ee, temp_env = temp_env))
}

#' Clean up Earth Engine environment
#'
#' Removes temporary conda environment if one was created
#'
#' @param env_info List returned from initialize_earthengine()
#' @keywords internal
cleanup_earthengine <- function(env_info) {
  if (!is.null(env_info$temp_env)) {
    log_msg(glue::glue("Cleaning up temporary conda environment: {env_info$temp_env}"))
    reticulate::conda_remove(env_info$temp_env)
  }
}

#' Download files from Google Drive (improved version)
#'
#' @param drive_folder List with folder name and ID
#' @param file_prefix File prefix to search for
#' @param local_path Local destination folder
#' @return Vector of downloaded file paths
#' @keywords internal
download_from_drive <- function(drive_folder, file_prefix, local_path) {
  files <- googledrive::drive_ls(path = drive_folder)
  matching_files <- files[grepl(paste0("^", file_prefix), files$name) & grepl("\\.tif$", files$name), ]

  for (i in seq_len(nrow(matching_files))) {
    file_path <- file.path(local_path, matching_files$name[i])
    googledrive::drive_download(
      googledrive::as_id(matching_files$id[i]),
      path = file_path, overwrite = TRUE
    )
    log_msg(glue::glue("Downloaded: {matching_files$name[i]}"))
  }
}

#' Merge downloaded tiles into a single COG
#'
#' @param tiles List of GeoTIFF file paths
#' @param output_file Final merged COG file path
#' @param datatype Output raster datatype
#' @return A `SpatRaster` object
#' @keywords internal
merge_tiles <- function(local_path, output_file, datatype) {
  tile_files <- list.files(local_path, pattern = "\\.tif$", full.names = TRUE)
  if (length(tile_files) == 0) {
    stop("No downloaded tiles found!", call. = FALSE)
  }

  log_msg(glue::glue("Merging {length(tile_files)} tile(s) into final output: {output_file}"))

  if (length(tile_files) == 1) {
    r <- terra::rast(tile_files)
  } else {
    r <- terra::vrt(tile_files)
  }

  predictor <- if (grepl("^INT", datatype)) "2" else "3"
  terra::writeRaster(
    r, output_file, filetype = "COG", datatype = datatype,
    gdal = c("COMPRESS=ZSTD", "NUM_THREADS=ALL_CPUS", "BIGTIFF=IF_SAFER",
             glue::glue("PREDICTOR={predictor}"), "OVERVIEWS=NONE"),
    overwrite = TRUE
  )
  log_msg("COG written to disk.")
  return(terra::rast(output_file))
}

#' Download and Process a GEE Raster Layer into a Cloud-Optimized GeoTIFF
#'
#' Generic function for downloading any raster data from Google Earth Engine.
#' Uses standalone initialization function for cleaner code organization.
#'
#' @param boundary_layer An `sf` object defining the spatial boundary of interest.
#' @param iso3 Three-letter ISO country code used to generate the export filename.
#' @param output_dir Path to directory for saving the final raster file.
#' @param asset_id Earth Engine ImageCollection asset ID (e.g., "projects/...").
#' @param file_prefix Prefix for export filename and GEE task description.
#' @param scale Resolution of the exported image in meters.
#' @param datatype Output datatype (GDAL style), e.g., "INT1U" or "FLT4S".
#' @param gee_project Earth Engine Cloud project ID.
#' @param googledrive_folder Base name of Google Drive folder (country code will be appended).
#' @param wait_time Max time (in minutes) to wait for Drive export to appear.
#'
#' @return A `SpatRaster` object written to disk, or NULL if export failed.
#' @keywords internal
download_gee_layer <- function(
    boundary_layer,
    iso3,
    output_dir = here::here(),
    asset_id,
    file_prefix,
    scale = 10,
    datatype = "INT1U",
    gee_project = "unbl-misc",
    googledrive_folder = "gee_exports",
    wait_time = 5
) {

  # Validation
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required but not installed.")
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  assertthat::assert_that(inherits(boundary_layer, "sf"), msg = "boundary_layer must be an sf object")
  assertthat::assert_that(
    is.numeric(wait_time) && length(wait_time) == 1 && wait_time %% 1 == 0,
    msg = "wait_time must be a single whole number (integer or numeric)"
  )

  # Create country-specific folder to avoid GEE bug
  country_folder <- paste0(googledrive_folder)#, "_", iso3)
  log_msg(glue::glue("Using country-specific folder: {country_folder}"))

  # Create temporary directory
  temp_dir <- file.path(Sys.getenv("HOME"), glue::glue("{country_folder}_{Sys.getpid()}"))
  dir.create(temp_dir, showWarnings = FALSE)
  log_msg(glue::glue("Temporary directory created at: {temp_dir}"))

  # Initialize Earth Engine (now a clean standalone function)
  env_info <- initialize_earthengine(gee_project)
  ee <- env_info$ee

  # Load the dataset and find the most recent year
  ic <- ee$ImageCollection(asset_id)
  year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  while (ic$filterDate(ee$Date(glue::glue("{year}-01-01")), ee$Date(glue::glue("{year}-12-31")))$size()$getInfo() == 0) {
    log_msg(glue::glue("No valid data found for {year}, trying {year - 1}..."))
    year <- year - 1
  }
  log_msg(glue::glue("Using data for year: {year}"))

  # Build filename and check if local file exists
  file_name <- glue::glue("{file_prefix}_{year}_{iso3}")
  output_file <- file.path(output_dir, glue::glue("{file_name}.tif"))

  if (file.exists(output_file)) {
    log_msg(glue::glue("File already exists locally: {output_file}"))
    log_msg("Loading existing file...")
    cleanup_earthengine(env_info)
    unlink(temp_dir, recursive = TRUE)
    return(terra::rast(output_file))
  }

  log_msg(glue::glue("Checking Google Drive for existing files: {file_name}*"))

  # Simple folder creation function (from original)
  ensure_drive_folder <- function(folder_name) {
    existing_folders <- googledrive::drive_ls(path = NULL, type = "folder")
    if (!(folder_name %in% existing_folders$name)) {
      log_msg(glue::glue("Folder '{folder_name}' does not exist. Creating it in Google Drive..."))
      googledrive::drive_mkdir(folder_name)
    } else {
      log_msg(glue::glue("Folder '{folder_name}' already exists in Google Drive."))
    }
  }

  # Ensure country folder exists
  ensure_drive_folder(country_folder)

  # Check for existing files
  files <- googledrive::drive_ls(path = country_folder)
  existing_files <- files[grepl(paste0("^", file_name), files$name) & grepl("\\.tif$", files$name), ]

  if (nrow(existing_files) > 0) {
    log_msg(glue::glue("Existing files for {iso3} found on Google Drive. Downloading instead of exporting from GEE..."))
  } else {
    log_msg(glue::glue("No existing exported files found for {iso3} on Google Drive."))
    log_msg("Checking for existing export tasks that are still running...")

    # Check for running tasks
    tasks <- ee$batch$Task$list()
    existing_task <- purrr::detect(tasks, function(t) {
      t$status()$state %in% c("READY", "RUNNING") && t$status()$description == file_name
    })

    if (!is.null(existing_task)) {
      log_msg(glue::glue("Existing export task '{file_name}' is still running. Waiting instead of starting a new one."))
    } else {
      log_msg("No similar running tasks found. Proceeding with new GEE export.")

      # Prepare geometry
      boundary_layer <- sf::st_transform(boundary_layer, crs = 4326)
      bounding_box <- sf::st_bbox(boundary_layer)
      ee_bounding_box <- ee$Geometry$Rectangle(
        c(bounding_box$xmin, bounding_box$ymin, bounding_box$xmax, bounding_box$ymax),
        proj = "EPSG:4326", geodesic = FALSE
      )

      # Filter and export
      start_date <- ee$Date(glue::glue("{year}-01-01"))
      end_date <- ee$Date(glue::glue("{year}-12-31"))
      filtered_data <- ic$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)

      task <- ee$batch$Export$image$toDrive(
        image = filtered_data$mosaic(),
        description = file_name,
        folder = country_folder,
        fileNamePrefix = file_name,
        scale = scale,
        region = ee_bounding_box$getInfo()[["coordinates"]],
        maxPixels = reticulate::r_to_py(1e13),
        fileFormat = "GeoTIFF"
      )
      task$start()
      log_msg("Export task started. Waiting for files to appear in Google Drive...")
    }

    # Wait for files to appear (from original logic)
    start_time <- Sys.time()
    repeat {
      files <- googledrive::drive_ls(path = country_folder)
      matching_files <- files[grepl(paste0("^", file_name), files$name) & grepl("\\.tif$", files$name), ]

      if (nrow(matching_files) > 0) {
        log_msg("Files found. Proceeding with download.")
        break
      }

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

      log_msg("File not yet available, waiting 30 seconds before re-trying...")
      Sys.sleep(30)
    }
  }

  # Download and process
  download_from_drive(country_folder, file_name, temp_dir)
  output_raster <- merge_tiles(temp_dir, output_file, datatype)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
  cleanup_earthengine(env_info)
  log_msg("Temporary files and Conda environment deleted.")

  return(output_raster)
}

#' Download the ESRI 10m Land Use/Land Cover Time Series (LULC) Layer
#'
#' Retrieves the ESRI Global LULC Time Series at 10m resolution from Earth Engine.
#' It downloads the most recent year available and returns the result as a local
#' Cloud-Optimized GeoTIFF. The layer name is taken from the original GEE asset name.
#'
#' @inheritParams download_gee_layer
#' @param output_dir Optional local output directory (default: project root).
#' @export
download_esri_lulc_data <- function(boundary_layer, iso3, gee_project, output_dir = here::here(), ...) {
  ee <- download_gee_layer(
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
  if (!is.null(ee)) {
    names(ee) <- "ESRI_Global-LULC_10m_TS"
  }
  return(ee)
}

#' Download Global Pasture Watch Grassland Probability Layer
#'
#' Downloads either the cultivated or natural/semi-natural grassland probability
#' layer from the Global Pasture Watch dataset hosted in Earth Engine. Result is
#' returned as a local Cloud-Optimized GeoTIFF. The layer name is taken from the
#' original GEE asset name.
#'
#' @inheritParams download_gee_layer
#' @param layer_type One of "cultivated" (default) or "natural".
#' @param output_dir Optional local output directory (default: project root).
#' @export
download_global_pasture_data <- function(
    boundary_layer,
    iso3,
    gee_project,
    output_dir = here::here(),
    layer_type = c("cultivated", "natural"),
    ...
) {
  layer_type <- match.arg(layer_type)
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
  ee <- download_gee_layer(
    boundary_layer = boundary_layer,
    iso3 = iso3,
    gee_project = gee_project,
    output_dir = output_dir,
    asset_id = asset_id,
    file_prefix = file_prefix,
    scale = 30,
    datatype = "FLT4S",
    ...
  )
  if (!is.null(ee)) {
    names(ee) <- file_prefix
  }
  return(ee)
}

#' Check and download GEE-sourced layers (e.g. LULC or Pasturelands)
#'
#' This checks for required layer names from metadata, ensures they exist in the output path,
#' and prompts the user to download them from GEE if missing.
#'
#' @param data_info A dataframe containing `data_name` fields to match
#' @param iso3 ISO3 country code
#' @param input_path Path where data files should exist or be downloaded to
#' @param gee_project Google Earth Engine project ID
#' @param boundary_proj `sf` object representing the country boundary
#' @param wait_time Wait time (in minutes) for Drive export
#' @return NULL (used for side-effect of downloading)
#' @keywords internal
check_and_download_required_layers <- function(data_info, iso3, input_path, gee_project, boundary_proj, wait_time = 5) {
  required_layers <- unique(data_info$data_name)

  gee_layers <- tibble::tibble(
    name = c("LULC", "Pasturelands"),
    pattern = c(
      paste0("lulc_.*_", iso3, "\\.tif$"),
      paste0("grassland_.*_", iso3, "\\.tif$")
    ),
    required_if = list(
      c("Urban Greening Opportunities", "Restoration Zone", "Protection Zone", "Agriculture Areas", "Urban Areas"),
      c("Pasturelands")
    ),
    download_fun = list(
      function() elsar::elsar_download_esri_lulc_data(boundary_layer = boundary_proj, iso3 = iso3, gee_project = gee_project, output_dir = input_path, wait_time = wait_time),
      function() elsar::elsar_download_global_pasture_data(boundary_layer = boundary_proj, iso3 = iso3, gee_project = gee_project, output_dir = input_path, wait_time = wait_time)
    )
  )

  for (i in seq_len(nrow(gee_layers))) {
    if (any(required_layers %in% gee_layers$required_if[[i]])) {
      all_dat <- list.files(input_path)
      has_data <- any(grepl(gee_layers$pattern[i], all_dat, ignore.case = TRUE) & grepl(iso3, all_dat, ignore.case = TRUE))

      if (!has_data) {
        log_msg(glue::glue("No {gee_layers$name[i]} data found for {iso3}."))

        answer <- readline(glue::glue("Do you want to download {gee_layers$name[i]} data from GEE? Press Enter to confirm [default = yes] (yes/no): "))
        answer <- tolower(trimws(answer))
        if (answer == "") answer <- "yes"

        if (answer %in% c("yes", "y")) {
          answer2 <- readline("Do you have GEE access and an internet connection? Press Enter to confirm [default = yes] (yes/no): ")
          answer2 <- tolower(trimws(answer2))
          if (answer2 == "") answer2 <- "yes"

          if (answer2 %in% c("yes", "y")) {
            log_msg(glue::glue("Starting download of {gee_layers$name[i]} for {iso3}..."))
            gee_layers$download_fun[[i]]()
          } else {
            message(glue::glue("Cannot proceed without access for {gee_layers$name[i]}."))
          }
        } else {
          message(glue::glue("Skipping {gee_layers$name[i]}. Ensure it is provided manually."))
        }
      } else {
        log_msg(glue::glue("Found existing {gee_layers$name[i]} data for {iso3}, skipping download."))
      }
    }
  }

  invisible(NULL)
}

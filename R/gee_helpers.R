# GEE Download Helpers

#' Ensure Google Drive folder exists
#'
#' @param folder_name Name of the Google Drive folder to check or create
#' @keywords internal
ensure_drive_folder <- function(folder_name) {
  folders <- tryCatch(
    googledrive::drive_ls(path = NULL, type = "folder"),
    error = function(e) {
      stop("Google Drive API is not available or not enabled. Please enable it via https://console.developers.google.com/apis/api/drive.googleapis.com/")
    }
  )
  if (!(folder_name %in% folders$name)) {
    log_msg(glue::glue("Folder '{folder_name}' does not exist. Creating Google Drive folder: {folder_name}"))
    googledrive::drive_mkdir(folder_name)
  } else {
    log_msg(glue::glue("Folder '{folder_name}' already exists in Google Drive."))
  }
}

#' Authenticate and initialize Earth Engine
#'
#' Sets up a Python environment and initializes the Earth Engine API
#'
#' @param gee_project GEE cloud project ID
#' @return A list containing the `ee` module and temporary environment name (if created)
#' @keywords internal
initialize_earthengine <- function(gee_project) {
  log_msg("Checking Python environment for Earth Engine API...")
  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
    if (!reticulate::py_module_available("ee")) {
      log_msg("Earth Engine not available in RETICULATE_PYTHON environment. Creating temporary conda env.")
      temp_env <- paste0("gee_temp_env_", Sys.getpid())
      reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
      reticulate::use_condaenv(temp_env, required = TRUE)
    } else {
      log_msg("Earth Engine module is available in RETICULATE_PYTHON.")
      temp_env <- NULL
    }
  } else {
    log_msg("RETICULATE_PYTHON not set. Creating new temporary conda environment.")
    temp_env <- paste0("gee_temp_env_", Sys.getpid())
    reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
    reticulate::use_condaenv(temp_env, required = TRUE)
  }
  ee <- reticulate::import("ee")
  cred_path <- file.path(rappdirs::user_config_dir("earthengine"), "credentials")
  if (!file.exists(cred_path)) {
    log_msg("No Earth Engine credentials found. Starting authentication...")
    ee$Authenticate()
  }
  ee$Initialize(project = gee_project)
  log_msg("Earth Engine initialized.")
  list(ee = ee, temp_env = temp_env)
}

#' Check for existing Earth Engine export task
#'
#' @param ee The Earth Engine Python module
#' @param description Description of the export task
#' @return TRUE if a task with matching description is found running or ready
#' @keywords internal
check_existing_ee_export_task <- function(ee, description) {
  log_msg(glue::glue("Checking for running GEE export tasks with description '{description}'..."))
  tasks <- ee$batch$Task$list()
  for (task in tasks) {
    status <- task$status()
    if (status$description == description && status$state %in% c("READY", "RUNNING")) {
      log_msg(glue::glue("Found running task: {description} (state: {status$state})"))
      return(TRUE)
    }
  }
  log_msg("No running tasks with matching description found.")
  return(FALSE)
}

#' Start new Earth Engine export task
#'
#' @param ee Earth Engine module
#' @param image EE Image object to export
#' @param description Description of the task
#' @param region Geometry to export
#' @param folder Google Drive folder name
#' @param file_name Filename prefix
#' @param scale Export resolution
#' @param max_pixels Max pixels allowed
#' @param file_format Export file format
#' @keywords internal
start_gee_export_task <- function(ee, image, description, region, folder, file_name, scale = 10, max_pixels = 1e13, file_format = "GeoTIFF") {
  log_msg(glue::glue("Starting new GEE export task: {description}"))
  task <- ee$batch$Export$image$toDrive(
    image = image,
    description = description,
    folder = folder,
    fileNamePrefix = file_name,
    region = region$getInfo()[["coordinates"]],
    scale = scale,
    maxPixels = as.numeric(max_pixels),
    fileFormat = file_format
  )
  task$start()
  log_msg("Export task submitted.")
  invisible(task)
}

#' Wait for GEE Drive export to appear
#'
#' @param folder Drive folder name
#' @param prefix Filename prefix to search for
#' @param wait_time Max wait time in minutes
#' @keywords internal
wait_for_drive_export <- function(folder, prefix, wait_time = 5) {
  start_time <- Sys.time()
  log_msg(glue::glue("Waiting for file with prefix '{prefix}' to appear in Drive folder '{folder}'..."))
  repeat {
    files <- googledrive::drive_ls(
      path = folder
    )
    matches <- files[grepl(glue::glue("^{prefix}.*\\.tif$"), files$name), ]
    if (nrow(matches) > 0) {
      log_msg("Matching file(s) found in Drive.")
      return(invisible(TRUE))
    }
    if (difftime(Sys.time(), start_time, units = "mins") > wait_time) {
      stop(glue::glue("Timeout: No matching files appeared in Drive after {wait_time} minutes. This should not be entirely unexpected... \nExporting high resoltuion data from GEE over large areas will often take longer than {wait_time} minutes. Please check the status of running tasks at https://code.earthengine.google.com/ before running again."))
    }
    log_msg("Exported file not yet found. Waiting 30 seconds...")
    Sys.sleep(30)
  }
}

#' Download files from Google Drive
#'
#' @param folder Drive folder name
#' @param prefix File prefix to search for
#' @param dest Local destination folder
#' @return Vector of downloaded file paths
#' @keywords internal
download_from_drive <- function(folder, prefix, dest) {
  log_msg(glue::glue("Downloading all files with prefix '{prefix}' from folder '{folder}'..."))
  files <- tryCatch(
    googledrive::drive_ls(
      path = folder
    ),
    error = function(e) {
      stop("Failed to access Google Drive. Make sure the Drive API is enabled and credentials are configured.")
    }
  )
  matches <- files[grepl(glue::glue("^{prefix}.*\\.tif$"), files$name), ]
  if (nrow(matches) == 0) {
    log_msg("No matching files found in Google Drive.")
    return(character(0))
  }
  paths <- character(nrow(matches))
  for (i in seq_len(nrow(matches))) {
    paths[i] <- file.path(dest, matches$name[i])
    log_msg(glue::glue("Downloading {matches$name[i]} to {paths[i]}"))
    googledrive::drive_download(
      googledrive::as_id(matches$id[i]),
      path = paths[i], overwrite = TRUE
    )
  }
  log_msg("All matching files downloaded.")
  return(paths)
}

#' Merge downloaded tiles into a single COG
#'
#' @param tiles List of GeoTIFF file paths
#' @param output_file Final merged COG file path
#' @param datatype Output raster datatype
#' @return A `SpatRaster` object
#' @keywords internal
merge_tiles_to_cog <- function(tiles, output_file, datatype = "FLT4S") {
  if (length(tiles) == 0) stop("No tiles provided.")
  log_msg(glue::glue("Merging {length(tiles)} tile(s) into final output: {output_file}"))
  if (length(tiles) == 1) {
    r <- terra::rast(tiles)
  } else {
    r <- terra::vrt(tiles)
  }
  predictor <- if (grepl("^INT", datatype)) "2" else "3"
  terra::writeRaster(
    r,
    output_file,
    filetype = "COG",
    datatype = datatype,
    gdal = c(
      "COMPRESS=ZSTD",
      "NUM_THREADS=ALL_CPUS",
      "BIGTIFF=IF_SAFER",
      glue::glue("PREDICTOR={predictor}"),
      "OVERVIEWS=NONE"
    ),
    overwrite = TRUE
  )
  log_msg("COG written to disk.")
  return(terra::rast(output_file))
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

# The following functions support downloading and processing Earth Engine raster datasets
# into local Cloud-Optimized GeoTIFFs (COGs). Data is exported to Google Drive,
# checked for existence before triggering an export, and then downloaded and merged
# into a local file. Used by `elsar_download_esri_lulc_data()` and
# `elsar_download_global_pasture_data()`.

#' Download and Process a GEE Raster Layer into a Cloud-Optimized GeoTIFF
#'
#' This function handles downloading raster data from Earth Engine. It checks if the
#' file already exists in a specified Google Drive folder. If not, it checks for
#' running Earth Engine export tasks. If neither exist, it launches a new export
#' based on the most recent available year, waits for it to appear in Drive,
#' downloads the tiles, and merges them into a single COG.
#'
#' It uses exponential backoff to retry the download check up to a given wait time
#' (in minutes), sleeping 30 seconds between attempts.
#'
#' @param boundary_layer An `sf` object defining the spatial boundary of interest.
#' @param iso3 Three-letter ISO country code used to generate the export filename.
#' @param output_dir Path to directory for saving the final raster file.
#' @param asset_id Earth Engine ImageCollection asset ID (e.g., "projects/...").
#' @param file_prefix Prefix for export filename and GEE task description.
#' @param scale Resolution of the exported image in meters.
#' @param datatype Output datatype (GDAL style), e.g., "INT1U" or "FLT4S".
#' @param gee_project Earth Engine Cloud project ID.
#' @param googledrive_folder Name of Google Drive folder to check and export to.
#' @param wait_time Max time (in minutes) to wait for Drive export to appear.
#'
#' @return A `SpatRaster` object written to disk.
#' @keywords internal
elsar_download_gee_layer <- function(
    boundary_layer,
    iso3,
    output_dir = here::here(),
    asset_id,
    file_prefix,
    scale,
    datatype = "INT1U",
    gee_project = gee_project,
    googledrive_folder = "gee_exports",
    wait_time = 5
) {
  file_name <- glue::glue("{file_prefix}_{iso3}")
  log_msg(glue::glue("Checking Google Drive for existing files: {file_name}*."))

  ensure_drive_folder(googledrive_folder)
  drive_files <- googledrive::drive_ls(path = googledrive_folder)
  existing_files <- drive_files[grepl(paste0("^", file_name), drive_files$name) & grepl("\\.tif$", drive_files$name), ]

  temp_dir <- file.path(Sys.getenv("HOME"), glue::glue("{googledrive_folder}_{Sys.getpid()}"))
  dir.create(temp_dir, showWarnings = FALSE)
  log_msg(glue::glue("Temporary directory created at: {temp_dir}."))

  if (nrow(existing_files) > 0) {
    log_msg("File(s) already exist in Drive. Proceeding to download.")
  } else {
    log_msg("No existing files found in Drive. Proceeding with Earth Engine export logic...")

    env <- initialize_earthengine(gee_project)
    ee <- env$ee

    ic <- ee$ImageCollection(asset_id)

    # Get most recent year with valid data
    year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    while (ic$filterDate(ee$Date(glue::glue("{year}-01-01")), ee$Date(glue::glue("{year}-12-31")))$size()$getInfo() == 0) {
      year <- year - 1
    }

    bbox <- sf::st_bbox(sf::st_transform(boundary_layer, crs = 4326))
    ee_geom <- ee$Geometry$Rectangle(
      coords = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
      proj = "EPSG:4326",
      geodesic = FALSE
    )

    is_running <- check_existing_ee_export_task(ee, file_name)
    if (!is_running) {

      image <- ic$filterDate(ee$Date(glue::glue("{year}-01-01")), ee$Date(glue::glue("{year}-12-31")))$
        filterBounds(ee_geom)$mosaic()

      start_gee_export_task(
        ee = ee,
        image = image,
        description = file_name,
        region = ee_geom,
        folder = googledrive_folder,
        file_name = file_name,
        scale = scale,
        max_pixels = 1e13,
        file_format = "GeoTIFF"
      )

    }
    wait_for_drive_export(googledrive_folder, file_name, wait_time)
  }

  tile_paths <- download_from_drive(googledrive_folder, file_name, temp_dir)
  output_file <- file.path(output_dir, glue::glue("{file_name}.tif"))
  out_rast <- merge_tiles_to_cog(tile_paths, output_file, datatype = datatype)

  if (exists("env") && !is.null(env$temp_env)) reticulate::conda_remove(env$temp_env)
  unlink(temp_dir, recursive = TRUE)
  return(out_rast)
}

#' Download the ESRI 10m Land Use/Land Cover Time Series (LULC) Layer
#'
#' Retrieves the ESRI Global LULC Time Series at 10m resolution from Earth Engine.
#' It downloads the most recent year available and returns the result as a local
#' Cloud-Optimized GeoTIFF. The layer name is taken from the original GEE asset name.
#'
#' @inheritParams elsar_download_gee_layer
#' @param output_dir Optional local output directory (default: project root).
#' @param ... Additional arguments passed to lower-level functions (currently unused)
#'
#' @export
elsar_download_esri_lulc_data <- function(boundary_layer, iso3, gee_project, output_dir = here::here(), ...) {
  ee <- elsar_download_gee_layer(
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
  names(ee) <- "ESRI_Global-LULC_10m_TS"
  return(ee)
}

#' Download Global Pasture Watch Grassland Probability Layer
#'
#' Downloads either the cultivated or natural/semi-natural grassland probability
#' layer from the Global Pasture Watch dataset hosted in Earth Engine. Result is
#' returned as a local Cloud-Optimized GeoTIFF. The layer name is taken from the
#' original GEE asset name.
#'
#' @inheritParams elsar_download_gee_layer
#' @param layer_type One of "cultivated" (default) or "natural".
#' @param output_dir Optional local output directory (default: project root).
#' @param ... Additional arguments passed to lower-level functions (currently unused)
#'
#' @export
elsar_download_global_pasture_data <- function(
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
  ee <- elsar_download_gee_layer(
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
  names(ee) <- file_prefix
  return(ee)
}

#' Download and Process ESRI 10m LULC Data from Google Earth Engine
#'
#' This function downloads ESRI 10m Land Use Land Cover (LULC) data from Google Earth Engine (GEE)
#' for a specified national boundary. It exports the image to Google Drive, downloads the resulting
#' tiles, merges them into a single raster, and converts it into a Cloud-Optimized GeoTIFF (COG).
#' Temporary files and environments are cleaned up after processing.
#'
#' @param boundary_layer An `sf` object representing the national boundary.
#' @param iso3 A character string representing the ISO3 country code (e.g., "USA").
#' @param output_dir A character string specifying the output directory for the final COG file.
#'                   Defaults to the project root directory using `here::here()`.
#' @param lulc_data_source A character string specifying the Earth Engine dataset ID for LULC data.
#'                         Defaults to the ESRI 10m LULC dataset.
#' @param gee_project A string specifying the GEE cloud project; default to `unbl-misc`. You must be a member of the project.
#' @param googledrive_folder A string specifying the folder in Google Drive that GEE exports will go to, or where files will be searched for. Default to `gee_exports`. This folder will be created if if does not exist.
#'
#' @return A Cloud-Optimized GeoTIFF file stored in the specified output directory.
#' @export
#'
#' @examples
#' \dontrun{
#' my_boundary <- sf::st_read("my_boundary.gpkg")
#'
#' lulc <- elsar_download_esri_lulc_data(boundary = my_boundary, iso3 = 'NPL')
#' }
elsar_download_esri_lulc_data <- function(
    boundary_layer,
    iso3,
    output_dir = here::here(),
    lulc_data_source = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
    gee_project = "unbl-misc",
    googledrive_folder = 'gee_exports') {

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required but not installed.")
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }

  assertthat::assert_that(inherits(boundary_layer, "sf"), msg = "boundary_layer must be an sf object")

  temp_dir <- file.path(Sys.getenv("HOME"), glue::glue("{googledrive_folder}_{Sys.getpid()}"))
  dir.create(temp_dir, showWarnings = FALSE)
  log_msg(glue::glue("Temporary directory created at: {temp_dir}."))

  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)

    # Check if `earthengine-api` is installed in the existing environment
    if (reticulate::py_module_available("ee")) {
      log_msg("Using existing Python environment with Earth Engine API installed.")
      temp_env <- NULL  # No need to create a temp environment
    } else {
      log_msg(
        "Existing Python environment is missing Earth Engine API. Creating a temporary Conda environment"
      )

      temp_env <- paste0("gee_temp_env_", Sys.getpid())
      reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
      reticulate::use_condaenv(temp_env, required = TRUE)
      log_msg(glue::glue("Temporary Conda environment created: {temp_env}."))
    }
  } else {
    log_msg("RETICULATE_PYTHON is not set. Creating a temporary Conda environment.")
    temp_env <- paste0("gee_temp_env_", Sys.getpid())
    reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
    reticulate::use_condaenv(temp_env, required = TRUE)
    log_msg(glue::glue("Temporary Conda environment created: {temp_env}."))
  }

  ee <- reticulate::import("ee")

  # Detect the correct credentials path for different OS
  cred_path <- file.path(rappdirs::user_config_dir("earthengine"), "credentials")

  # Authenticate only if credentials do not exist
  if (!file.exists(cred_path)) {
    ee$Authenticate()
  }

  ee$Initialize(project = gee_project)
  log_msg("Google Earth Engine initialized.")

  lulc_data_source <- ee$ImageCollection(lulc_data_source)
  log_msg("Loaded ESRI LULC dataset from GEE.")

  YEAR <- as.numeric(format(Sys.Date(), "%Y")) - 1
  while (lulc_data_source$filterDate(ee$Date(glue::glue("{YEAR}-01-01")), ee$Date(glue::glue("{YEAR}-12-31")))$size()$getInfo() == 0) {
    log_msg(glue::glue("No valid bands found for {YEAR}, trying {YEAR - 1}..."))
    YEAR <- YEAR - 1
  }
  log_msg(glue::glue("Using LULC data for year: {YEAR}."))

  boundary_layer <- sf::st_transform(boundary_layer, crs = 4326)
  bounding_box <- sf::st_bbox(boundary_layer)
  ee_bounding_box <- ee$Geometry$Rectangle(
    c(
      bounding_box$xmin,
      bounding_box$ymin,
      bounding_box$xmax,
      bounding_box$ymax
    ),
    proj = "EPSG:4326",
    geodesic = FALSE
  )

  log_msg("Bounding box converted to Earth Engine geometry")

  file_name <- glue::glue("esri_10m_lulc_{YEAR}_{iso3}")
  log_msg(glue::glue("Checking Google Drive for existing files: {file_name}*."))


  # Function to create Google Drive folder if it doesn't exist
  ensure_drive_folder <- function(folder_name) {
    existing_folders <- googledrive::drive_ls(path = NULL, type = "folder")
    if (!(folder_name %in% existing_folders$name)) {
      log_msg(glue::glue("Folder '{folder_name}' does not exist. Creating it in Google Drive..."))
      googledrive::drive_mkdir(folder_name)
    } else {
      log_msg(glue::glue("Folder '{folder_name}' already exists."))
    }
  }

  # Ensure the folder exists before running the GEE export
  ensure_drive_folder(googledrive_folder)

  files <- googledrive::drive_ls(path = googledrive_folder)
  existing_files <- files[grepl(paste0("^", file_name), files$name) &
                            grepl("\\.tif$", files$name), ]

  if (nrow(existing_files) > 0) {
    log_msg("Existing files found on Google Drive. Downloading instead of exporting from GEE...")
  } else {
    log_msg(
      "No existing files found. Proceeding with GEE export.\nChecking for existing export tasks that are still running..."
    )

    # List all current Earth Engine tasks
    tasks <- ee$batch$Task$list()

    # Check if a task with the same description (file_name) is still running
    existing_task <- purrr::detect(tasks, function(t) {
      t$status()$state %in% c("READY", "RUNNING") &&
        t$status()$description == file_name
    })

    if (!is.null(existing_task)) {
      log_msg(
        glue::glue(
          "Existing export task '{file_name}' is still running. Waiting instead of starting a new one."))
    } else {
      log_msg("No running tasks found. Proceeding with new export.")

      start_date <- ee$Date(glue::glue("{YEAR}-01-01"))
      end_date <- ee$Date(glue::glue("{YEAR}-12-31"))
      filtered_lulc <- lulc_data_source$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)

      task <- ee$batch$Export$image$toDrive(
        image = filtered_lulc$mosaic()$int(),
        description = file_name,
        folder = googledrive_folder,
        fileNamePrefix = file_name,
        scale = 10,
        region = ee_bounding_box$getInfo()[["coordinates"]],
        maxPixels = 1e13,
        fileFormat = "GeoTIFF"
      )
      task$start()
      log_msg("Export task started. Waiting for files to appear in Google Drive...")
    }

    start_time <- Sys.time()
    repeat {
      files <- googledrive::drive_ls(path = googledrive_folder)
      matching_files <- files[grepl(paste0("^", file_name), files$name) &
                                grepl("\\.tif$", files$name), ]

      if (nrow(matching_files) > 0) {
        log_msg("Files found. Proceeding with download.")
        break
      }

      if (difftime(Sys.time(), start_time, units = "mins") > 5) {
        stop("Timeout: No files available after 5 minutes, so this process is stopping gracefully...\nThis is NOT unexpected as GEE exports can take time. Try running again later...\nYou can also check the status of exports via the GEE web console.")
      }

      log_msg("File not yet available, waiting 30 seconds before re-trying...")
      Sys.sleep(30)
    }
  }

  download_from_drive <- function(googledrive_folder, file_prefix, local_path) {
    files <- googledrive::drive_ls(path = googledrive_folder)
    matching_files <- files[grepl(paste0("^", file_prefix), files$name) &
                              grepl("\\.tif$", files$name), ]

    for (i in seq_len(nrow(matching_files))) {
      file_path <- file.path(local_path, matching_files$name[i])
      googledrive::drive_download(
        googledrive::as_id(matching_files$id[i]),
        path = file_path,
        overwrite = TRUE
      )
      log_msg(glue::glue("Downloaded: {matching_files$name[i]}."))
    }
  }

  merge_tiles <- function(local_path, output_dir) {
    tile_files <- list.files(local_path, pattern = "\\.tif$", full.names = TRUE)

    if (length(tile_files) == 0)
      stop("No downloaded tiles found!")

    output_file <- here::here(output_dir,
                              glue::glue("esri_10m_lulc_{YEAR}_{iso3}.tif"))

    if (length(tile_files) == 1) {
      log_msg("Only one tile found. Saving it to output.")
      # Optionally rename the file by re-writing
      terra::writeRaster(
        terra::rast(tile_files),
        output_file,
        filetype = "COG",
        datatype = "INT1U",
        gdal = c(
          "COMPRESS=ZSTD",
          "NUM_THREADS=ALL_CPUS",
          "BIGTIFF=IF_SAFER"
        ),
        NAflag = 255,
        overwrite = TRUE
      )
    } else {
      merged_raster <- terra::vrt(tile_files)
      names(merged_raster) <- glue::glue("esri_10m_lulc_{YEAR}_{iso3}")

      terra::writeRaster(
        merged_raster,
        output_file,
        filetype = "COG",
        datatype = "INT1U",
        gdal = c(
          "COMPRESS=ZSTD",
          "NUM_THREADS=ALL_CPUS",
          "BIGTIFF=IF_SAFER"
        ),
        NAflag = 255,
        overwrite = TRUE
      )
    }

    log_msg(glue::glue("COG successfully written to disk at: {output_file}."))
    return(terra::rast(output_file))
  }


  download_from_drive(googledrive_folder = googledrive_folder, file_prefix = file_name, local_path = temp_dir)
  output_raster <- merge_tiles(temp_dir, output_dir)
  unlink(temp_dir, recursive = TRUE)
  if (!is.null(temp_env))
    reticulate::conda_remove(temp_env)
  log_msg("Temporary files and Conda environment deleted.")
  return(output_raster)
}

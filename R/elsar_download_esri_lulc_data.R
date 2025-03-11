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
#'
#' @import sf
#' @import terra
#' @import glue
#' @import googledrive
#' @import reticulate
#' @import here
#'
#' @return A Cloud-Optimized GeoTIFF file stored in the specified output directory.
#' @export
#'
#' @examples
#' my_boundary <- sf::st_read("my_boundary.gpkg")
#'
#' lulc <- elsar_download_esri_lulc_data(boundary = my_boundary, iso3 = 'NPL')
#'
elsar_download_esri_lulc_data <- function(
    boundary_layer,
    iso3,
    output_dir = here::here(),
    lulc_data_source = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS"
) {
  temp_dir <- file.path(Sys.getenv("HOME"), paste0("gee_exports_", Sys.getpid()))
  terra::terraOptions(tempdir = temp_dir)
  dir.create(temp_dir, showWarnings = FALSE)
  cat("Temporary directory created at:", temp_dir, "\n")

  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
    temp_env <- NULL
  } else {
    temp_env <- paste0("gee_temp_env_", Sys.getpid())
    reticulate::conda_create(temp_env, packages = c("python=3.12", "earthengine-api"))
    reticulate::use_condaenv(temp_env, required = TRUE)
    cat("Temporary Conda environment created:", temp_env, "\n")
  }

  ee <- reticulate::import("ee")
  ee$Initialize()
  cat("Google Earth Engine initialized.\n")

  lulc_data_source <- ee$ImageCollection(lulc_data_source)
  cat("Loaded ESRI LULC dataset from GEE.\n")

  YEAR <- as.numeric(format(Sys.Date(), "%Y")) - 1
  while (lulc_data_source$filterDate(ee$Date(glue::glue("{YEAR}-01-01")),
                                     ee$Date(glue::glue("{YEAR}-12-31")))$size()$getInfo() == 0) {
    cat(glue::glue("No valid bands found for {YEAR}, trying {YEAR - 1}...\n"))
    YEAR <- YEAR - 1
  }
  cat("Using LULC data for year:", YEAR, "\n")

  boundary_layer <- sf::st_transform(boundary_layer, crs = 4326)
  bounding_box <- sf::st_bbox(boundary_layer)
  ee_bounding_box <- ee$Geometry$Rectangle(c(bounding_box$xmin, bounding_box$ymin,
                                             bounding_box$xmax, bounding_box$ymax),
                                           proj = "EPSG:4326", geodesic = FALSE)
  cat("Bounding box converted to Earth Engine geometry.\n")

  file_name <- glue::glue("esri_10m_lulc_{YEAR}_{iso3}")
  cat("Checking Google Drive for existing files: ", file_name, "\n")

  files <- googledrive::drive_ls(path = "gee_exports")
  existing_files <- files[grepl(paste0("^", file_name, "-"), files$name) & grepl("\\.tif$", files$name), ]

  if (nrow(existing_files) > 0) {
    cat("Existing files found on Google Drive. Downloading instead of exporting...\n")
  } else {
    cat("No existing files found. Proceeding with GEE export.\n")
    start_date <- ee$Date(glue::glue("{YEAR}-01-01"))
    end_date <- ee$Date(glue::glue("{YEAR}-12-31"))
    filtered_lulc <- lulc_data_source$filterDate(start_date, end_date)$filterBounds(ee_bounding_box)

    task <- ee$batch$Export$image$toDrive(
      image = filtered_lulc$mosaic()$int(),
      description = file_name,
      folder = "gee_exports",
      fileNamePrefix = file_name,
      scale = 10,
      region = ee_bounding_box$getInfo()[["coordinates"]],
      maxPixels = 1e13,
      fileFormat = "GeoTIFF"
    )
    task$start()
    cat("Export task started. Waiting for files to appear in Google Drive...\n")

    start_time <- Sys.time()
    repeat {
      files <- googledrive::drive_ls(path = "gee_exports")
      matching_files <- files[grepl(paste0("^", file_name, "-"), files$name) & grepl("\\.tif$", files$name), ]

      if (nrow(matching_files) > 0) {
        cat("Files found. Proceeding with download.\n")
        break
      }

      if (difftime(Sys.time(), start_time, units = "mins") > 3) {
        stop("Timeout: No files available after 3 minutes. Try running again later.")
      }

      cat("File not yet available, waiting 30 seconds...\n")
      Sys.sleep(30)
    }
  }

  download_from_drive <- function(file_prefix, local_path) {
    files <- googledrive::drive_ls(path = "gee_exports")
    matching_files <- files[grepl(paste0("^", file_prefix, "-"), files$name) & grepl("\\.tif$", files$name), ]

    for (i in seq_len(nrow(matching_files))) {
      file_path <- file.path(local_path, matching_files$name[i])
      googledrive::drive_download(googledrive::as_id(matching_files$id[i]), path = file_path, overwrite = TRUE)
      cat("Downloaded:", matching_files$name[i], "\n")
    }
  }

  merge_tiles <- function(local_path, output_dir) {
    tile_files <- list.files(local_path, pattern = "*.tif$", full.names = TRUE)
    if (length(tile_files) == 0) stop("No downloaded tiles found!")
    merged_raster <- terra::vrt(tile_files)
    names(merged_raster) <- glue::glue("esri_10m_lulc_{YEAR}_{iso3}")
    output_file <- here::here(output_dir, glue::glue("esri_10m_lulc_{YEAR}_{iso3}.tif"))
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
      overwrite = TRUE)
    cat("COG created successfully:", output_file, "\n")
    return(terra::rast(output_file))
  }

  download_from_drive(file_prefix = file_name, local_path = temp_dir)
  output_raster <- merge_tiles(temp_dir, output_dir)
  unlink(temp_dir, recursive = TRUE)
  if (!is.null(temp_env)) reticulate::conda_remove(temp_env)
  cat("Temporary files and Conda environment deleted.\n")
  return(output_raster)
}

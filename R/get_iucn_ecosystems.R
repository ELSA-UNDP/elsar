#' Extract and Filter IUCN GET Ecosystem Vector Layers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and not recommended for current use. The IUCN GET
#' ecosystem data has known quality issues and incomplete coverage. Users should
#' supply their own ecosystem data to functions like [make_threatened_ecosystems_protection()]
#' and [make_underrepresented_ecosystems()].
#'
#' A replacement data source from the Global Ecosystems Atlas is planned for future
#' versions of this package.
#'
#' @details
#' This function reads, intersects, reprojects, and merges `.gpkg` vector files representing IUCN Global Ecosystem Typology (GET) layers from a specified directory.
#' It allows filtering by filename prefixes (e.g., "T1.2", "TF1.4"), excludes specific ecosystem types (e.g., intensive land-use biomes), and optionally removes features
#' with `minor occurrence` flags. The resulting layer is cropped to a country boundary and reprojected to match a planning units raster.
#'
#' The ecosystem layer ID (e.g., "F1.1") is extracted from the source filename and added to each feature as the `get_id` column for tracking.
#'
#' If `output_path` is specified, the final filtered and valid `sf` object is saved as a GeoPackage.
#'
#' @param iucn_get_directory Character. Path to the directory containing IUCN GET `.gpkg` files.
#' @param iso3 Character. ISO3 country code (used for logging and naming the output file).
#' @param pus A `SpatRaster` object. Used for defining spatial extent and target CRS.
#' @param boundary_layer An `sf` polygon layer (typically a country boundary) used to spatially crop features.
#' @param include_minor_occurrence Logical. If `FALSE`, filters out ecosystems marked as minor occurrence (default is `TRUE`).
#' @param iucn_get_prefixes Optional character vector of GET ecosystem IDs to include (e.g., `c("F1.1", "TF1.2")`). If `NULL`, all layers are included.
#' @param excluded_prefixes Optional character vector of ecosystem prefixes to exclude (e.g., `c("T7.1", "T7.2")` for intensive land-use biomes). Default exclude all T7 classes.
#' @param output_path Character or `NULL`. If specified, writes the resulting `sf` object as a GeoPackage to this directory.
#'
#' @return An `sf` object of the merged, reprojected, valid IUCN GET ecosystem features intersecting the target boundary.
#' @export
#'
#' @examples
#' \dontrun{
#' # Define inputs
#' pus <- terra::rast("data/pus_raster.tif")
#' boundary <- sf::st_read("data/country_boundary.gpkg")
#'
#' # Get all layers except intensive land-use (T7) and minor occurrences
#' ecosystems <- get_iucn_ecosystems(
#'   iucn_get_directory = "data/iucn_layers",
#'   iso3 = "KEN",
#'   boundary_layer = boundary,
#'   pus = pus,
#'   include_minor_occurrence = FALSE,
#'   excluded_prefixes = c("T7.1", "T7.2", "T7.3", "T7.4", "T7.5"),
#'   output_path = "outputs"
#' )
#' }
get_iucn_ecosystems <- function(
    iucn_get_directory,
    iso3,
    pus,
    boundary_layer,
    include_minor_occurrence = TRUE,
    iucn_get_prefixes = NULL,
    excluded_prefixes = c("T7.1", "T7.2", "T7.3", "T7.4", "T7.5"), # All T7* Intensive land-use biomes
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory), dir.exists(iucn_get_directory))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(inherits(boundary_layer, "sf"))

  allowed_prefixes <- c( "F1.1", "F1.2", "F1.3", "F1.4", "F1.5", "F1.6", "F1.7", "F2.1", "F2.10", "F2.2", "F2.3", "F2.4", "F2.5", "F2.6", "F2.7", "F2.8", "F2.9", "F3.1", "F3.2", "F3.3", "F3.4", "F3.5", "FM1.1", "FM1.2", "FM1.3", "M1.1", "M1.10", "M1.2", "M1.3", "M1.4", "M1.5", "M1.6", "M1.7", "M1.8", "M1.9", "M2.1", "M2.2", "M2.3", "M2.4", "M2.5", "M3.1", "M3.2", "M3.3", "M3.4", "M3.5", "M3.6", "M3.7", "M4.1", "M4.2", "MFT1.1", "MFT1.2", "MFT1.3", "MT1.1", "MT1.2", "MT1.3", "MT1.4", "MT2.1", "MT2.2", "MT3.1", "S1.1", "S2.1", "SF1.1", "SF1.2", "SF2.1", "SF2.2", "SM1.1", "SM1.2", "SM1.3", "T1.1", "T1.2", "T1.3", "T1.4", "T2.1", "T2.2", "T2.3", "T2.4", "T2.5", "T2.6", "T3.1", "T3.2", "T3.3", "T3.4", "T4.1", "T4.2", "T4.3", "T4.4", "T4.5", "T5.1", "T5.2", "T5.3", "T5.4", "T5.5", "T6.1", "T6.2", "T6.3", "T6.4", "T6.5", "T7.1", "T7.2", "T7.3", "T7.4", "T7.5", "TF1.1", "TF1.2", "TF1.3", "TF1.4", "TF1.5", "TF1.6", "TF1.7" )

  if (!is.null(iucn_get_prefixes)) {
    invalid <- setdiff(iucn_get_prefixes, allowed_prefixes)
    assertthat::assert_that(
      length(invalid) == 0,
      msg = glue::glue(
        "Invalid prefixes: {paste(invalid, collapse = ', ')}.\n",
        "Allowed prefixes: {paste(allowed_prefixes, collapse = ', ')}."
      )
    )

    # Convert e.g. F1.1 → F1_1 to match filenames like F1_1_v2_0.gpkg
    file_prefixes <- gsub("\\.", "_", iucn_get_prefixes)

    # Create pattern to match start of filenames
    pattern <- paste0("^(", paste(file_prefixes, collapse = "|"), ")_.*\\.gpkg$")
  } else {
    pattern <- "\\.gpkg$"
  }

  # Convert excluded prefixes to match filenames
  excluded_file_prefixes <- gsub("\\.", "_", excluded_prefixes)

  # List all matching files and filter out excluded prefixes
  #all_files <- list.files(iucn_get_directory, pattern = pattern, full.names = TRUE)
  all_files <- normalizePath(list.files(iucn_get_directory, pattern = pattern, full.names = TRUE))


  # Identify excluded matches (for logging)
  excluded_matches <- all_files[grepl(paste0("^(", paste(excluded_file_prefixes, collapse = "|"), ")_"),
                                      basename(all_files))]
  if (length(excluded_matches) > 0) {
    log_message(glue::glue("Excluding {length(excluded_matches)} IUCN layers based on excluded_prefixes: {paste(excluded_prefixes, collapse = ', ')}"))
  }

  # Apply exclusion filter
  all_files <- all_files[!basename(all_files) %>%
                           grepl(paste0("^(", paste(excluded_file_prefixes, collapse = "|"), ")_"), .)]

  assertthat::assert_that(
    length(all_files) > 0,
    msg = paste("No matching '.gpkg' files found in", iucn_get_directory)
  )

  # Get bounding box for fast filtering (WGS84)
  pus_bbox <- terra::as.polygons(terra::ext(pus)) %>%
    sf::st_as_sf()
  sf::st_crs(pus_bbox) <- terra::crs(pus)
  pus_bbox_wgs <- sf::st_transform(pus_bbox, crs = "EPSG:4326")

  # Conditionally split boundary
  pus_bbox_wgs <- conditionally_subdivide_bbox(bbox_sf = pus_bbox_wgs) %>%
    terra::vect()

  if (!requireNamespace("future.apply", quietly = TRUE)) stop("Please install the 'future.apply' package.")
  if (!requireNamespace("progressr", quietly = TRUE)) stop("Please install the 'progressr' package.")

  n_cores <- parallel::detectCores(logical = FALSE)
  n_workers <- max(1, floor(n_cores / 2))

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  if (.Platform$OS.type == "unix" && !interactive()) {
    future::plan(future::multicore, workers = n_workers)
  } else {
    future::plan(future::multisession, workers = n_workers)
  }

  progressr::handlers("txtprogressbar")

  log_message("Reading, reprojecting, and intersecting IUCN GET ecosystem layers using parallel processing...")

  pus_bbox_file <- tempfile(fileext = ".gpkg")
  boundary_layer_file <- tempfile(fileext = ".gpkg")
  pus_crs <- terra::crs(pus)

  terra::writeVector(pus_bbox_wgs, pus_bbox_file, overwrite = TRUE)
  sf::st_write(boundary_layer, boundary_layer_file, delete_dsn = TRUE, quiet = TRUE)

  iucn_list <- progressr::with_progress({
    p <- progressr::progressor(along = all_files)

    future.apply::future_lapply(all_files, function(file) {
      p()

      pus_bbox <- terra::vect(pus_bbox_file)
      boundary <- terra::vect(boundary_layer_file)

      v <- tryCatch({
        terra::vect(file)
      }, error = function(e) {
        message(sprintf("Failed to read %s: %s", basename(file), e$message))
        return(NULL)
      })

      if (is.null(v) || terra::nrow(v) == 0)
        return(NULL)

      v <- terra::crop(v, pus_bbox)

      if (terra::nrow(v) == 0)
        return(NULL)

      layer_id <- gsub("_", ".", gsub("_v.*$", "", tools::file_path_sans_ext(basename(file))))
      v$get_id <- layer_id

      v <- tryCatch({
        terra::intersect(v, terra::project(boundary, terra::crs(v)))
      }, error = function(e) {
        message(sprintf("Failed to intersect %s: %s", basename(file), e$message))
        return(NULL)
      })

      # Validate that v is still valid after intersect
      if (is.null(v) || !inherits(v, "SpatVector") || terra::nrow(v) == 0 || is.null(terra::geom(v))) {
        message(sprintf("Intersect result invalid or empty for %s", basename(file)))
        return(NULL)
      }

      if (terra::crs(v) != pus_crs) {
        # Check again for null geom before projecting
        if (is.null(terra::geom(v)))
          return(NULL)

        v <- terra::project(v, pus_crs)
      }

      if (terra::nrow(v) == 0 || is.null(terra::geom(v)))
        return(NULL)

      return(sf::st_as_sf(v))
    })
  })

  unlink(pus_bbox_file)
  unlink(boundary_layer_file)
  rm(pus_crs)

  log_message(glue::glue("Finished intersecting all IUCN GET features in {iso3}. Checking for any NULL features..."))

  iucn_list <- Filter(NROW, iucn_list)

  if (length(iucn_list) == 0) {
    log_message("No intersecting features found in any file.")
    return(NULL)
  }

  # Correct for issues in GET attribute spelling of occurrence (vs. occurence)
  iucn_list <- lapply(iucn_list, function(x) {
    colnames(x) <- gsub("^occurence$", "occurrence", colnames(x))
    x
  })

  # Find common columns
  common_cols <- Reduce(intersect, lapply(iucn_list, names))

  # Subset each sf to common columns
  iucn_list_common <- lapply(iucn_list, function(x) x[, common_cols, drop = FALSE])

  # Combine safely
  iucn_ecosystems <- do.call(rbind, iucn_list_common)

  iucn_ecosystems <- iucn_ecosystems %>%
    group_by(get_id, occurrence) %>%
    summarise()

  # Optionally filter out minor occurrence
  if (!include_minor_occurrence) {
    log_message("Removing minor occurence polygons...")
    iucn_ecosystems <- dplyr::filter(iucn_ecosystems, occurrence != 1)
  }

  # Ensure valid geometry and select output fields
  log_message("Checking and repairing geometries...")
  iucn_ecosystems <- sf::st_as_sf(iucn_ecosystems) %>%
    sf::st_make_valid() %>%
    dplyr::select(get_id, occurrence)

  # Optionally write to file
  if (!is.null(output_path)) {
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    out_file <- file.path(output_path, glue::glue("iucn_ecosystems_{iso3}.gpkg"))
    log_message(glue::glue("Saving merged vector layer to: {out_file}"))
    sf::st_write(iucn_ecosystems, out_file, delete_dsn = TRUE, quiet = TRUE)
  }

  return(iucn_ecosystems)
}

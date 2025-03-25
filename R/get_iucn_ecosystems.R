#' Extract IUCN GET Ecosystems Vector Layers
#'
#' This function reads and merges `.gpkg` vector files representing IUCN GET ecosystems
#' from a specified directory. It optionally filters files based on filename prefixes
#' (e.g., "T", "TF", "FM"), restricts features to those intersecting a country boundary,
#' reprojects them to match a planning units raster (`pus`), and removes ecosystems
#' marked as `minor occurrence` if desired.
#'
#' The layer ID (e.g., "F1.1") is derived from the source filename and added to each feature
#' for tracking. If `output_path` is provided, the final merged and filtered layer is saved
#' as a GeoPackage.
#'
#' @param iucn_get_directory Character. Directory containing IUCN GET `.gpkg` files.
#' @param iso3 Character. ISO3 country code used for naming the output (e.g., "KEN").
#' @param boundary_layer sf object. Vector polygon used to spatially clip features (usually country boundary).
#' @param pus SpatRaster. Planning units raster used to define spatial extent and target CRS.
#' @param include_minor_occurrence Logical. If FALSE, excludes features with minor occurrence (default: TRUE).
#' @param iucn_get_prefixes Character vector of filename prefixes to include (e.g., c("T", "TF", "FM")) or NULL to include all `.gpkg` files.
#' @param output_path Character or NULL. If provided, writes the merged output to a GeoPackage.
#'
#' @return An `sf` object containing merged and filtered IUCN GET ecosystem features with valid geometry.
#' @export
#'
#' @examples
#' \dontrun{
#' pus <- terra::rast("data/pus_raster.tif")
#' boundary <- sf::st_read("data/country_boundary.gpkg")
#' ecosystems <- get_iucn_ecosystems(
#'   iucn_get_directory = "data/iucn_layers",
#'   iso3 = "KEN",
#'   boundary_layer = boundary,
#'   pus = pus,
#'   iucn_get_prefixes = c("T", "TF"),
#'   include_minor_occurrence = FALSE,
#'   output_path = "outputs"
#' )
#' }

get_iucn_ecosystems <- function(
    iucn_get_directory,
    iso3,
    boundary_layer,
    pus,
    include_minor_occurence = TRUE,
    iucn_get_prefixes = NULL,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory), dir.exists(iucn_get_directory))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(inherits(boundary_layer, "sf"))

  allowed_prefixes <- c("F", "FM", "M", "MFT", "MT", "S", "SF", "SM", "T", "TF")

  if (!is.null(iucn_get_prefixes)) {
    invalid <- setdiff(iucn_get_prefixes, allowed_prefixes)
    assertthat::assert_that(
      length(invalid) == 0,
      msg = glue::glue(
        "Invalid prefixes: {paste(invalid, collapse = ', ')}.\n",
        "Allowed prefixes: {paste(allowed_prefixes, collapse = ', ')}."
      )
    )
    pattern <- paste0("^(", paste(iucn_get_prefixes, collapse = "|"), ").*\\.gpkg$")
  } else {
    pattern <- "\\.gpkg$"
  }

  all_files <- list.files(iucn_get_directory, pattern = pattern, full.names = TRUE)

  assertthat::assert_that(
    length(all_files) > 0,
    msg = paste("No matching '.gpkg' files found in", iucn_get_directory)
  )

  # Get bounding box for fast filtering (WGS84)
  pus_bbox <- terra::as.polygons(terra::ext(pus)) %>%
    sf::st_as_sf()
  sf::st_crs(pus_bbox) <- terra::crs(pus)
  pus_bbox_wgs <- sf::st_transform(pus_bbox, crs = "EPSG:4326") %>%
    terra::vect()

  cat("Reading, reprojecting, and intersecting IUCN GET ecosystem layers...\n")
  iucn_list <- lapply(all_files, function(file) {
    v <- terra::vect(file, extent = pus_bbox_wgs)
    if (NROW(v) == 0) return(NULL)

    # Intersect and reproject
    v <- terra::intersect(v, terra::project(terra::vect(boundary_layer), terra::crs(v)))
    if (terra::crs(v) != terra::crs(pus)) {
      v <- terra::project(v, terra::crs(pus))
    }

    # Safely derive layer ID from filename
    layer_id <- gsub("_", ".", gsub("_v.*$", "", tools::file_path_sans_ext(basename(file))))
    v$id <- layer_id

    return(v)
  })

  iucn_list <- Filter(NROW, iucn_list)

  if (length(iucn_list) == 0) {
    cat("No intersecting features found in any file.\n")
    return(NULL)
  }

  iucn_ecosystems <- do.call(rbind, iucn_list)

  # Correct for issues in GET attribute spelling of occurrence (vs. occurence)
  col_names <- names(iucn_ecosystems)
  if ("occurence" %in% col_names && !"occurrence" %in% col_names) {
    iucn_ecosystems <- dplyr::rename(iucn_ecosystems, occurrence = occurence)
  } else if ("occurence" %in% col_names && "occurrence" %in% col_names) {
    warning(
      "Both 'occurrence' and 'occurence' columns found - merging with preference to 'occurrence'."
    )
    iucn_ecosystems$occurrence <- dplyr::coalesce(iucn_ecosystems$occurrence, iucn_ecosystems$occurence)
    iucn_ecosystems <- dplyr::select(iucn_ecosystems, -occurence)
  }

  # Optionally filter out minor occurrence
  if (!include_minor_occurrence) {
    iucn_ecosystems <- dplyr::filter(iucn_ecosystems, occurrence != 1)
  }

  # Ensure valid geometry and select output fields
  iucn_ecosystems <- sf::st_as_sf(iucn_ecosystems) %>%
    sf::st_make_valid() %>%
    dplyr::select(id, occurrence)

  # Optionally write to file
  if (!is.null(output_path)) {
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    out_file <- file.path(output_path, glue::glue("iucn_ecosystems_{iso3}.gpkg"))
    cat(glue::glue("Saving merged vector layer to: {out_file}\n"))
    sf::st_write(iucn_ecosystems, out_file, delete_dsn = TRUE, quiet = TRUE)
  }

  return(iucn_ecosystems)
}

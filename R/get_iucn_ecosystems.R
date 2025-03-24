#' Extract IUCN GET Ecosystems Vector Layers
#'
#' This function reads and merges `.gpkg` vector files representing IUCN GET ecosystems
#' from a specified directory. Optionally, it filters files by a set of filename prefixes
#' (e.g., "T", "TF", "FM", etc.), restricts features to those intersecting the extent
#' of a planning units raster, and filters out minor occurrence ecosystems. If `output_path`
#' is specified, the result is written to a GeoPackage.
#'
#' @param iucn_get_directory Character. Path to the directory containing `.gpkg` files.
#' @param iso3 Character. ISO3 country code (e.g., "KEN"), used in optional output naming.
#' @param pus SpatRaster. Planning units raster used to define spatial extent and CRS.
#' @param include_minor_occurence Logical. If FALSE, features marked as "minor" occurrence will be excluded (default = TRUE).
#' @param prefixes Character vector of filename prefixes to include (e.g., `c("T", "TF", "FM")`).
#' Must match one or more of: `"F"`, `"FM"`, `"M"`, `"MFT"`, `"MT"`, `"S"`, `"SF"`, `"SM"`, `"T"`, `"TF"`.
#' If `NULL` (default), all `.gpkg` files in the directory will be included.
#' @param output_path Optional character. If provided, writes the merged ecosystem layer as a GeoPackage.
#'
#' @return An `sf` object containing all filtered IUCN GET ecosystem polygons intersecting the PUs extent.
#' @export
#'
#' @import sf
#' @import terra
#' @import dplyr
#' @import assertthat
#' @import glue
#'
#' @examples
#' \dontrun{
#' pus <- terra::rast("data/pus_raster.tif")
#' ecosystems <- get_iucn_ecosystems(
#'   iucn_get_directory = "data/iucn_layers",
#'   iso3 = "KEN",
#'   pus = pus,
#'   prefixes = c("T", "TF"),
#'   include_minor_occurence = FALSE,
#'   output_path = "outputs"
#' )
#' }

get_iucn_ecosystems <- function(
    iucn_get_directory,
    iso3,
    pus,
    include_minor_occurence = TRUE,
    iucn_get_prefixes = NULL,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(assertthat::is.string(iucn_get_directory), dir.exists(iucn_get_directory))
  assertthat::assert_that(assertthat::is.string(iso3), msg = "'iso3' must be a valid country ISO3 code string (e.g., 'KEN').")
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "'pus' must be a SpatRaster object.")

  # Allowed filename prefixes from IUCN GET
  allowed_prefixes <- c("F", "FM", "M", "MFT", "MT", "S", "SF", "SM", "T", "TF")

  # Validate prefixes if provided
  if (!is.null(iucn_get_prefixes)) {
    invalid <- setdiff(iucn_get_prefixes, allowed_prefixes)
    assertthat::assert_that(length(invalid) == 0,
                            msg = glue::glue(
                              "Invalid prefixes detected: {paste(invalid, collapse = ', ')}.\n",
                              "Allowed prefixes are: {paste(allowed_prefixes, collapse = ', ')}."
                            )
    )
    pattern <- paste0("^(", paste(iucn_get_prefixes, collapse = "|"), ").*\\.gpkg$")
  } else {
    pattern <- "\\.gpkg$"
  }

  # List matching files
  all_files <- list.files(
    iucn_get_directory,
    pattern = pattern,
    full.names = TRUE
  )

  # Ensure at least one file found
  assertthat::assert_that(length(all_files) > 0,
                          msg = paste("No matching '.gpkg' files found in", iucn_get_directory)
  )

  # Convert PUs extent to WGS84 sf polygon for filtering
  pus_bbox <- terra::as.polygons(terra::ext(pus)) |>
    sf::st_as_sf()
  sf::st_crs(pus_bbox) <- terra::crs(pus)
  pus_bbox <- sf::st_transform(pus_bbox, crs = "EPSG:4326")

  # Read and spatially filter files
  cat("Reading and filtering IUCN GET ecosystem layers...\n")
  iucn_list <- lapply(all_files, function(file) {
    terra::vect(file, extent = pus_bbox)
  })

  # Remove empty results
  iucn_list <- Filter(NROW, iucn_list)

  if (length(iucn_list) == 0) {
    cat("No intersecting features found in any file.\n")
    return(NULL)
  }

  # Merge and convert to sf
  iucn_ecosystems <- do.call(rbind, iucn_list) |>
    sf::st_as_sf()

  # Reproject to match PUs
  if (sf::st_crs(iucn_ecosystems) != terra::crs(pus)) {
    iucn_ecosystems <- sf::st_transform(iucn_ecosystems, terra::crs(pus))
  }

  # Optionally exclude minor occurrences
  if (!include_minor_occurence) {
    # Merge 'occurence' and 'occurrence' into a single column
    col_names <- names(iucn_ecosystems)

    if ("occurence" %in% col_names && !"occurrence" %in% col_names) {
      # Rename occurence → occurrence
      iucn_ecosystems <- dplyr::rename(iucn_ecosystems, occurrence = occurence)
    } else if ("occurence" %in% col_names && "occurrence" %in% col_names) {
      warning("Both 'occurrence' and 'occurence' columns found — merging into 'occurrence' with preference to the correctly spelled column.")
      # Use 'occurrence' where not NA, fallback to 'occurence' otherwise
      iucn_ecosystems$occurrence <- dplyr::coalesce(
        iucn_ecosystems$occurrence,
        iucn_ecosystems$occurence
      )
      iucn_ecosystems <- dplyr::select(iucn_ecosystems, -occurence)
    } else if (!"occurrence" %in% col_names) {
      warning("No 'occurrence' column found — skipping minor occurrence filtering.")
    }

    # If we have a valid 'occurrence' column, filter on it
    if ("occurrence" %in% names(iucn_ecosystems)) {
      iucn_ecosystems <- dplyr::filter(iucn_ecosystems, occurrence != 1)
    }
  }

  # Write to disk if requested
  if (!is.null(output_path)) {
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    out_file <- file.path(output_path, glue::glue("iucn_ecosystems_{iso3}.gpkg"))
    cat(glue::glue("Saving merged vector layer to: {out_file}\n"))
    sf::st_write(iucn_ecosystems, out_file, delete_dsn = TRUE, quiet = TRUE)
  }

  return(iucn_ecosystems)
}

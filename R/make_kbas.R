#' Create a Standardised Raster of Key Biodiversity Areas (KBAs)
#'
#' This function processes a Key Biodiversity Areas (KBA) vector dataset and converts it into
#' a normalised raster aligned with the input planning units. It allows for filtering to
#' include or exclude Alliance for Zero Extinction (AZE) sites, as well as optionally excluding
#' KBAs marked as "Regional". By default, it includes all KBAs except regional-only sites.
#' It can also be used to return only AZE sites if `aze_only = TRUE`.
#'
#' @param kba_in An `sf` object containing KBA vector features, including columns like `iso3`, `azestatus`, and `kbaclass`.
#' @param pus A `SpatRaster` object representing planning units (reference extent and resolution).
#' @param iso3 A character string representing the 3-letter ISO country code (e.g., "KEN").
#' @param include_aze_sites Logical. If `TRUE`, includes KBAs that are also AZE sites (default is `FALSE`).
#' @param aze_only Logical. If `TRUE`, returns only confirmed AZE sites (default is `FALSE`).
#' @param include_regional_kba Logical. If `FALSE`, filters out KBAs marked as "Regional" or "Global/ Regional to be determined".
#' @param buffer_points Logical. If `TRUE`, circular buffers are generated around point geometries using the `area_column` attribute (default is `TRUE`).
#' @param area_column A string indicating the name of the column containing the reported area for point geometries (in km²), used when buffering points. The default `"repareakm2"` is the KBA dataset's column for points with reported areas. Note: `sitareakm2` contains site area for polygon features but is typically NA for points.
#' @param nQuadSegs An integer specifying the number of segments used to approximate circular buffers (default: 50).
#' @param output_path Optional character. Directory path to save the output raster. If `NULL`, output is not written to file.
#'
#' @return A `SpatRaster` object with normalised values representing KBA (or AZE) coverage across planning units.
#' If `output_path` is provided, the raster is also written to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' kba_raster <- make_kbas(
#'   kba_in = kba_sf,
#'   pus = planning_units,
#'   iso3 = "KEN",
#'   aze_only = TRUE,
#'   output_path = "outputs"
#' )
#' }
make_kbas <- function(
    kba_in,
    pus,
    iso3,
    include_aze_sites = FALSE,
    aze_only = FALSE,
    include_regional_kba = FALSE,
    buffer_points = TRUE,
    area_column = "repareakm2",
    nQuadSegs = 50,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(inherits(kba_in, "sf"), msg = "kba_in must be an sf object.")
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "pus must be a SpatRaster object.")
  assertthat::assert_that(is.character(iso3) && nchar(iso3) == 3, msg = "iso3 must be a 3-character country code.")
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # To keep consistent with use of iso3 elsewhere, and to avoid issues around filtering
  # on the attribute of the same name in the KBA dataset, we use iso3_filter.
  iso3_filter <- iso3

  kba <- kba_in %>%
    dplyr::filter(iso3 == iso3_filter)

  if (nrow(kba) > 0) {
    if (aze_only) {
      log_message("Returning only AZE sites.")
      kba <- dplyr::filter(kba, azestatus == "confirmed")

      if (nrow(kba) == 0) {
        log_message("No matching AZE sites found in the study region: returning empty raster.")
        kba <- terra::ifel(pus == 1, 0, NA)
      }
    } else {
      if (!include_aze_sites) {
        log_message("Excluding AZE sites from KBA.")
        kba <- dplyr::filter(kba, is.na(azestatus) | azestatus != "confirmed")

        if (nrow(kba) == 0) {
          log_message("After excluding AZEs, there are no KBA sites found: returning empty raster.")
          kba <- terra::ifel(pus == 1, 0, NA)
          return(kba)
        }
      } else {
        log_message("Including AZE sites in KBAs.")
      }

      if (!include_regional_kba) {
        log_message("Excluding Regional KBAs or those with undetermined Global status.")
        kba <- dplyr::filter(kba, !kbaclass %in% c("Regional", "Global/ Regional to be determined"))
      }

      if (nrow(kba) == 0) {
        log_message("No sites founds after removing Regional sites and those with undetermined Global status: returning empty raster.")
        kba <- terra::ifel(pus == 1, 0, NA)
      }
    }

    if (inherits(kba, "SpatRaster")) {
      return(kba)
    } else {
      # Separate polygons and points - exactextractr only works with polygons
      geom_types <- sf::st_geometry_type(kba)
      has_points <- any(geom_types %in% c("POINT", "MULTIPOINT"))
      has_polygons <- any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))

      # Start with polygons (keep all of them regardless of area attribute)
      polygons <- kba %>%
        dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON")))

      # Handle points if present
      if (has_points && buffer_points) {
        points <- kba %>%
          dplyr::filter(sf::st_is(., c("POINT", "MULTIPOINT")))

        # Only buffer points with valid area values
        points_with_area <- points %>%
          dplyr::filter(!is.na(.data[[area_column]]) & .data[[area_column]] > 0)

        if (nrow(points_with_area) > 0) {
          log_message("Buffering {nrow(points_with_area)} point features with area attributes...")
          points_buffered <- convert_points_polygon(
            sf_layer = points_with_area,
            area_crs = sf::st_crs(pus),
            area_attr = area_column,
            nQuadSegs = nQuadSegs,
            area_multiplier = 1e4,
            append_original_polygons = FALSE
          )
          # Combine with polygons
          if (nrow(polygons) > 0) {
            polygons <- dplyr::bind_rows(polygons, points_buffered)
          } else {
            polygons <- points_buffered
          }
        } else {
          log_message("No point features have valid area attributes for buffering; using polygons only.")
        }
      } else if (has_points && !buffer_points) {
        log_message("Points present but buffer_points=FALSE; using polygons only.")
      }

      # Check if we have any polygons to rasterize
      if (nrow(polygons) == 0) {
        log_message("No polygon features available for rasterization. Returning empty raster.")
        kba <- terra::ifel(pus == 1, 0, NA)
      } else {
        log_message("Rasterising {nrow(polygons)} polygon features...")
        kba <- polygons %>%
          sf::st_transform(sf::st_crs(pus)) %>%
          dplyr::summarise() %>%
          sf::st_make_valid()

        kba <- exactextractr::coverage_fraction(pus, kba)[[1]] %>%
          elsar::make_normalised_raster(pus = pus, iso3 = iso3)
      }
    }
  } else {
    log_message("No matching KBA features found in the study region: returning empty raster.")
    kba <- terra::ifel(pus == 1, 0, NA)
  }

  if (!is.null(output_path)) {
    # Auto-generate name_out
    if (aze_only) {
      name_out <- "aze_sites"
    } else if (!include_aze_sites) {
      name_out <- "kba_aze_sites"
    } else {
      name_out <- "kba_sites"
    }

    elsar::save_raster(
      raster = kba,
      filename = glue::glue("{output_path}/{name_out}_{iso3}.tif"),
      datatype = "FLT4S"
    )
  }

  return(kba)
}

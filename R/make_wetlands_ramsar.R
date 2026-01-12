#' Create a Standardised Raster for Wetlands and Ramsar Sites
#'
#' This function processes and combines Ramsar and global wetlands data to produce a standardised
#' raster layer aligned to planning units. It supports handling of Ramsar point geometries by buffering
#' them into polygons if required, and allows filtering and normalisation based on user-defined
#' thresholds. Either Ramsar or Wetlands data can be provided, or both.
#'
#' @param ramsar_in Ramsar site data. Can be one of:
#'   - An `sf` object containing Ramsar site geometries (points and/or polygons)
#'   - A character path to a GeoPackage file. If the gpkg contains separate "centroids" and
#'     "boundaries" layers, both will be read and combined automatically.
#' @param wetlands_in A `SpatRaster` object representing global wetlands data (optional).
#' @param pus A `SpatRaster` object containing the planning units used for resolution and extent.
#' @param iso3 A character string with the ISO3 country code (e.g., "KEN").
#' @param buffer_points Logical. Only relevant when "POINT" or "MULTIPOINT" geometries exist.
#'   If `TRUE`, circular buffers are generated from the area attribute (default: TRUE).
#' @param area_column A string indicating the name of the column containing site area (in hectares).
#'   Used when buffering point geometries (default: "area_ha").
#' @param nQuadSegs An integer specifying the number of segments used to approximate circular buffer (default: 50).
#' @param wetland_threshold Numeric. Threshold for binarising wetlands raster (default: 0.25).
#' @param return_all Logical. If `TRUE`, returns a raster stack containing the combined wetlands+ramsar raster,
#'   as well as the individual normalised input layers.
#'
#' @return A `SpatRaster` object. If `return_all = TRUE`, returns a raster stack with three layers:
#'   combined, ramsar only, and wetlands only.
#' @export
make_wetlands_ramsar <- function(
    ramsar_in = NULL,
    wetlands_in = NULL,
    pus,
    iso3,
    buffer_points = TRUE,
    area_column = "area_ha",
    nQuadSegs = 50,
    wetland_threshold = 0.25,
    return_all = FALSE) {

  # To keep consistent with use of iso3 elsewhere, and to avoid issues around filtering
  # on the attribute of the same name in the Ramsar dataset, we use iso3_filter.
  iso3_filter <- iso3

  if (!is.null(ramsar_in)) {
    # Handle gpkg file path input - read and combine centroids and boundaries layers
    if (is.character(ramsar_in) && file.exists(ramsar_in)) {
      log_message("Reading Ramsar data from GeoPackage: {basename(ramsar_in)}")
      available_layers <- sf::st_layers(ramsar_in)$name

      ramsar_combined <- NULL

      # Read boundaries (polygons) if available
      if ("boundaries" %in% available_layers) {
        boundaries <- tryCatch({
          # Try reading with sf::st_read - may fail if curve geometries present
          b <- sf::st_read(ramsar_in, layer = "boundaries", quiet = TRUE)
          b <- sf::st_make_valid(b)
          # Cast to MULTIPOLYGON to ensure compatible geometry type
          b <- sf::st_cast(b, "MULTIPOLYGON")
          b
        }, error = function(e) {
          # If sf can't parse curves, try with GDAL's curve approximation via ogr2ogr workaround
          tryCatch({
            log_message("Curve geometries detected - using GDAL linearization...")
            # Read using a lower-level approach that can handle curves
            # Set the GDAL config to convert non-linear geometries
            old_val <- Sys.getenv("OGR_STROKE_CURVE")
            on.exit(Sys.setenv(OGR_STROKE_CURVE = old_val), add = TRUE)
            Sys.setenv(OGR_STROKE_CURVE = "TRUE")

            b <- sf::st_read(ramsar_in, layer = "boundaries", quiet = TRUE)
            b <- sf::st_make_valid(b)
            # Force to MULTIPOLYGON
            b <- sf::st_cast(b, "MULTIPOLYGON")
            b
          }, error = function(e2) {
            # Final fallback: read as WKB and try to linearize
            tryCatch({
              log_message("Attempting geometry linearization fallback...")
              # Use terra/GDAL as intermediary which may handle curves better
              temp_vect <- terra::vect(ramsar_in, layer = "boundaries")
              b <- sf::st_as_sf(temp_vect)
              b <- sf::st_make_valid(b)
              b <- sf::st_cast(b, "MULTIPOLYGON")
              b
            }, error = function(e3) {
              log_message("Warning: Could not read boundaries layer: {e3$message}")
              NULL
            })
          })
        })
        if (!is.null(boundaries) && nrow(boundaries) > 0) {
          log_message("Read {nrow(boundaries)} boundary features from Ramsar gpkg")
          ramsar_combined <- boundaries
        }
      }

      # Read centroids (points) if available
      if ("centroids" %in% available_layers) {
        centroids <- tryCatch({
          sf::st_read(ramsar_in, layer = "centroids", quiet = TRUE)
        }, error = function(e) {
          log_message("Warning: Could not read centroids layer: {e$message}")
          NULL
        })
        if (!is.null(centroids) && nrow(centroids) > 0) {
          log_message("Read {nrow(centroids)} centroid features from Ramsar gpkg")
          if (is.null(ramsar_combined)) {
            ramsar_combined <- centroids
          } else {
            # Combine - keep only common columns
            common_cols <- intersect(names(ramsar_combined), names(centroids))
            ramsar_combined <- dplyr::bind_rows(
              ramsar_combined[, common_cols],
              centroids[, common_cols]
            )
          }
        }
      }

      if (is.null(ramsar_combined)) {
        stop("GeoPackage does not contain 'centroids' or 'boundaries' layers.", call. = FALSE)
      }

      ramsar_in <- ramsar_combined
    }

    log_message("Filtering Ramsar sites data for iso3 code: {iso3}...")
    ramsar <- ramsar_in %>%
      dplyr::filter(iso3 == iso3_filter) %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_make_valid()

    if (nrow(ramsar) == 0) {
      log_message("No Ramsar sites in the planning region - returning an empty raster.")
      ramsar <- terra::ifel(pus == 1, 0, NA)
    } else {
      # exactextractr only works with polygon information; need to deal with points
      if (("MULTIPOINT" %in% sf::st_geometry_type(ramsar)) ||
          ("POINT" %in% sf::st_geometry_type(ramsar))) {
        if (buffer_points) {
          # buffer around "POINTS" and make them into polygons
          ramsar <- convert_points_polygon(
            sf_layer = ramsar,
            area_crs = sf::st_crs(pus),
            area_attr = area_column,
            nQuadSegs = nQuadSegs,
            area_multiplier = 1e4
          ) %>%
            sf::st_transform(sf::st_crs(pus)) %>%
            dplyr::summarise() %>%
            sf::st_make_valid()
        } else {
          # only keep polygon and multipolygon information

          if (nrow(ramsar %>% dplyr::filter(sf::st_is(., c(
            "POLYGON", "MULTIPOLYGON"
          )))) > 0) {
            ramsar <- ramsar %>%
              sf::st_transform(sf::st_crs(pus)) %>%
              dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
              dplyr::summarise() %>%
              sf::st_make_valid()

            log_message("Rasterising and normalising Ramsar sites...")
            ramsar <- exactextractr::coverage_fraction(pus, ramsar)[[1]] %>% # problem when point
              elsar::make_normalised_raster(pus = pus, iso3 = iso3)

          } else {
            log_message(
              "Only 'POINT' or 'MULTIPOINT' geometry type Ramsar sites found in the planning region and 'buffer_points' is set to false."
            )
            log_message("Returning an empty raster.")

            ramsar <- terra::ifel(pus == 1, 0, NA)
          }
        }
      }
    }
  }

  if (!is.null(wetlands_in)) {
    wetlands <- elsar::make_normalised_raster(
      raster_in = wetlands_in,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(is.na(x), 0, terra::ifel(x > 1, 1, 0)),
      conditional_expression = function(x) terra::ifel(x > wetland_threshold, 1, 0)
    )
  } else {
    wetlands <- NULL
  }

  # Check if ramsar data has valid values (not just an empty raster)
  has_ramsar <- exists("ramsar") && inherits(ramsar, "SpatRaster") &&
    !is.na(suppressWarnings(terra::minmax(ramsar)[2])) &&
    terra::minmax(ramsar)[2] > 0

  # Check if wetlands data has valid values
  has_wetlands <- !is.null(wetlands) &&
    !is.na(suppressWarnings(terra::minmax(wetlands)[2])) &&
    terra::minmax(wetlands)[2] > 0

  if (has_wetlands && has_ramsar) {
    log_message("Wetlands and Ramsar raster built using Wetlands AND Ramsar data.")
    ramsar_wetlands <- 0.5 * wetlands + ramsar
  } else if (has_ramsar && !has_wetlands) {
    log_message("Wetlands and Ramsar raster built using only Ramsar data.")
    ramsar_wetlands <- ramsar
  } else if (has_wetlands && !has_ramsar) {
    log_message("Wetlands and Ramsar raster built using only Wetlands data.")
    ramsar_wetlands <- wetlands
  } else {
    stop("Either Wetlands or Ramsar data must be provided with valid values.", call. = FALSE)
  }

  ramsar_wetlands <- ramsar_wetlands %>%
    elsar::make_normalised_raster(
      pus = pus,
      iso3 = iso3
    )

  if (return_all && has_wetlands && has_ramsar) {
    ramsar_wetlands <- c(
      ramsar_wetlands,
      ramsar %>% terra::subst(NA, 0),
      wetlands %>% terra::subst(NA, 0)
    )
    names(ramsar_wetlands) <- c("combined", "ramsar", "wetlands")
  }

  return(ramsar_wetlands)
}

#' Load spatial or raster data from local files or Postgres
#'
#' Loads spatial (`sf`) or raster (`SpatRaster`) data from various sources:
#' - Local files: Shapefiles, GeoPackages, GeoJSON, TIFFs, GRDs, NetCDF, etc.
#' - PostGIS databases via direct query
#'
#' Vector data can be automatically filtered by an ISO3 country code and/or a WKT geometry.
#' For shapefiles and multi-layer files (e.g. GeoPackage or file geodatabases), this function merges
#' multiple layers or files and ensures consistent column alignment.
#'
#' @param file_name Character or vector. File name(s) with extension, or `NULL` to load all files of the type (e.g., all `.shp` files in folder).
#' @param file_path Character. Path to the local folder or file. Required for local sources.
#' @param file_lyr Character. Optional. Specific layer name within a multi-layer file (e.g., a layer in a GPKG or GDB).
#' @param file_type Character. One of: `"postgres"`, `"shp"`, `"gpkg"`, `"geojson"`, `"gdb"`, `"tif"`, `"tiff"`, `"grd"`, `"gri"`, `"nc"`, `"hdf"`.
#' @param wkt_filter Character. Optional WKT geometry used to spatially filter the data (e.g., a bounding box).
#' @param db_info Named list. Required for Postgres if `pg_connection` is not supplied. Must contain: `host`, `dbname`, `port`, `user`, `password`.
#' @param drop3d Logical. Whether to drop Z or M dimensions (3D/4D) to keep only XY (2D). Default is `TRUE`.
#' @param pg_connection Named list. Alternative to `db_info`, passed directly to `RPostgres::dbConnect`.
#' @param iso3_column Character. Column name to filter by ISO3 country code. Default is `"iso3"`.
#' @param iso3 Character. ISO3 code (e.g., `"NPL"`) to filter the vector data.
#'
#' @return An `sf` object (for vector formats) or a `terra::SpatRaster` (for raster formats).
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a single GeoTIFF
#' load_raster <- elsar_load_data(
#'   "layer.tif",
#'   "/data",
#'   file_type = "tif"
#'   )
#'
#' # Load and merge all shapefiles in a folder
#' merged_shps <- elsar_load_data(
#'   file_name = NULL,
#'   file_path = "/data/shapes",
#'   file_type = "shp"
#'   )
#'
#' # Load and filter a PostGIS table
#' load_pg <- elsar_load_data(
#'   file_name = "boundaries",
#'   file_type = "postgres",
#'   pg_connection = list(
#'     host = "localhost", dbname = "gis", port = 5432, user = "user", password = "pass"
#'   ),
#'   iso3_column = "iso3cd",
#'   iso3 = "NPL"
#' )
#' }
elsar_load_data <- function(
    file_name = NULL,
    file_path = NULL,
    file_lyr = NULL,
    file_type,
    wkt_filter = NULL,
    db_info = NULL,
    drop3d = TRUE,
    pg_connection = NULL,
    iso3_column = "iso3",
    iso3) {

  # Helper function to filter by iso3 code and spatial extent when reading data
  filter_sf <- function(file_path, iso3, iso3_column, layer_name = NULL, drop3d = TRUE, wkt_filter = NULL) {
    if (is.null(layer_name)) {
      layer_info <- sf::st_layers(file_path)
      layer_name <- layer_info$name[1]  # Default to first layer
    }

    query <- glue::glue("SELECT * FROM \"{layer_name}\" WHERE \"{iso3_column}\" = '{iso3}'")

    dat <- tryCatch({
      if (!is.null(wkt_filter)) {
        sf::st_read(file_path, query = query, wkt_filter = wkt_filter, quiet = TRUE)
      } else {
        sf::st_read(file_path, query = query, quiet = TRUE)
      }
    }, error = function(e) {
      message("Failed to read layer: ", layer_name, " in file: ", file_path, "\nError: ", e$message)
      return(NULL)
    })

    if (!is.null(dat) && drop3d) {
      dat <- sf::st_zm(dat, drop = TRUE, what = "ZM")
    }

    if (!is.null(dat)) {
      dat <- sf::st_make_valid(dat)
    }

    return(dat)
  }

  # Prepare spatial filter (WKT) if needed
  if (!is.null(wkt_filter)) {
    spatial_filter <- terra::ext(wkt_filter) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      sf::st_set_crs("EPSG:4326") %>%
      sf::st_transform(4326) %>%
      sf::st_geometry() %>%
      sf::st_as_text()
  }

  # Connect to Postgres and run filtered query
  if (is.null(file_path) & file_type == "postgres") {
    # Connect using provided connection info
    if (is.null(pg_connection) && is.null(db_info)) {
      stop("Error: Both 'db_info' and 'pg_connection' are NULL. Please provide at least one.")
    }

    # Create database connection
    con <- if (is.null(pg_connection)) {
      RPostgres::dbConnect(
        RPostgres::Postgres(),
        host = db_info["host"][[1]],
        dbname = db_info["dbname"][[1]],
        port = db_info["port"][[1]],
        user = db_info["user"][[1]],
        password = db_info["password"][[1]]
      )
    } else {
      RPostgres::dbConnect(
        RPostgres::Postgres(),
        host = pg_connection[["host"]],
        dbname = pg_connection[["dbname"]],
        port = pg_connection[["port"]],
        user = pg_connection[["user"]],
        password = pg_connection[["password"]]
      )
    }

    # Load data using SQL filtered by iso3
    loaded_data <- sf::st_read(
      dsn = con,
      query = glue::glue("SELECT * FROM {file_name} WHERE {iso3_column} = '{iso3}'")
    )

  }

  # Load local vector or raster data
  else if (!is.null(file_path) & file_type != "postgres") {
    to_load <- file.path(file_path, file_name)

    if (file_type %in% c("shp", "gpkg", "geojson", "gdb")) {
      # Handle multiple ESRI Shapefiles if file_type is 'shp'
      if (file_type == "shp" &&(is.null(file_name) || length(file_name) > 1)) {
        shapefiles <- if (is.null(file_name)) {
          list.files(file_path, pattern = "\\.shp$", full.names = TRUE)
        } else {
          file.path(file_path, file_name)
        }

        # Apply filter_sf to each shapefile
        all_data <- lapply(shapefiles, function(f) {
          tryCatch({
            filter_sf(
              file_path = f,
              iso3 = iso3,
              iso3_column = iso3_column,
              drop3d = drop3d,
              wkt_filter = wkt_filter
            )
          }, error = function(e) {
            message("Failed to read shapefile: ",
                    f,
                    "\nError: ",
                    e$message)
            NULL
          })
        })
        all_data <- Filter(Negate(is.null), all_data)
        all_data <- lapply(all_data, sf::st_make_valid)

        # Merge and align fields
        all_cols <- unique(unlist(lapply(all_data, names)))
        all_data <- lapply(all_data, function(x) {
          missing <- setdiff(all_cols, names(x))
          for (col in missing)
            x[[col]] <- NA
          x[, all_cols]
        })

        loaded_data <- dplyr::bind_rows(all_data)

        return(loaded_data)
      }

      # Load and merge all layers from a multi-layer vector source
      if (file_type %in% c("gpkg", "gdb") && is.null(file_lyr)) {
        all_layers <- sf::st_layers(to_load)$name

        all_data <- lapply(all_layers, function(lyr) {
          tryCatch({
            filter_sf(
              file_path = to_load,
              iso3 = iso3,
              iso3_column = iso3_column,
              layer_name = lyr,
              drop3d = drop3d,
              wkt_filter = wkt_filter
            )
          }, error = function(e) {
            message("Failed to read layer: ", lyr, " in file: ", to_load, "\nError: ", e$message)
            NULL
          })
        })

        all_data <- Filter(Negate(is.null), all_data)
        all_data <- lapply(all_data, sf::st_make_valid)

        # Unify columns across layers
        all_cols <- unique(unlist(lapply(all_data, names)))
        all_data <- lapply(all_data, function(x) {
          missing_cols <- setdiff(all_cols, names(x))
          for (col in missing_cols)
            x[[col]] <- NA
          x[, all_cols]
        })

        loaded_data <- dplyr::bind_rows(all_data)

      } else {
        # Load a specific vector layer
        if (!is.null(file_lyr)) {
          loaded_data <- filter_sf(
            file_path = to_load,
            layer_name = file_lyr,
            iso3 = iso3,
            iso3_column = iso3_column,
            drop3d = drop3d,
            wkt_filter = wkt_filter
          )
        }
      }
    }

    # Load raster files using terra
    else if (file_type %in% c("tif", "tiff", "grd", "gri", "nc", "hdf")) {
      loaded_data <- if (!is.null(file_lyr)) {
        terra::rast(to_load, lyrs = file_lyr)
      } else {
        terra::rast(to_load)
      }

    } else {
      log_msg(
        "Selected file_type might not be provided yet. Please add an issue on https://github.com/ELSA-UNDP/elsar/issues."
      )
    }
  }

  # Handle case where file_path is missing for local files
  else {
    log_msg(
      "Please provide a file path for your local data. Remote accessing is currently only supported through postgres."
    )
  }

  return(loaded_data)
}

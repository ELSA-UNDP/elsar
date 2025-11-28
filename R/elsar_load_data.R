#' Load spatial or raster data from local files or Postgres
#'
#' Automatically detects file type and loads spatial (`sf`) or raster (`SpatRaster`) data.
#' Supports local files (e.g., shapefiles, GeoPackages, TIFFs) and PostGIS tables.
#'
#' @param file_name Character or NULL. File name with extension. Use NULL to load all
#'   files of a type in a folder. Set to `"postgres"` to load from a PostGIS database.
#' @param file_path Character. Folder or file path. Not required when loading from PostgreSQL.
#' @param file_lyr Character. Optional. Layer name for multi-layer files (e.g., GeoPackage
#'   or GDB). When loading from PostgreSQL, this specifies the table name.
#' @param wkt_filter `sf`, `SpatRaster`, or `SpatVector`, from which WKT geometry can be
#'   derived to spatially filter vector data when reading in.
#' @param db_info Named list with PostgreSQL connection parameters. Required elements:
#'   \describe{
#'     \item{host}{Database host (e.g., "localhost" or an IP address)}
#'     \item{dbname}{Name of the PostgreSQL database}
#'     \item{port}{Port number (typically 5432)}
#'     \item{user}{Database username}
#'     \item{password}{Database password}
#'   }
#' @param pg_connection Named list. Alternative parameter name for `db_info` (same structure).
#' @param drop3d Logical. Drop Z/M dimensions if TRUE. Default is TRUE.
#' @param iso3_column Character. Column name to filter by ISO3 code.
#' @param iso3 Character. ISO3 code to filter vector data.
#'
#' @return An sf object or SpatRaster depending on the file type.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a single raster
#' r <- elsar_load_data(
#'   file_name = "lulc.tif",
#'   file_path = "data"
#'   )
#'
#' # Load shapefile with ISO3 filter
#' shp <- elsar_load_data(
#'   file_name = "admin.shp",
#'   file_path = "data",
#'   iso3 = "KEN",
#'   iso3_column = "ISO3"
#'   )
#'
#' # Load all shapefiles in a folder
#' shp_all <- elsar_load_data(
#'   file_name = NULL,
#'   file_path = "data/shapes"
#'   )
#'
#' # Load PostGIS table
#' pg <- elsar_load_data(
#'   file_name = "postgres",
#'   file_lyr = "admin",
#'   iso3 = "KEN",
#'   iso3_column = "iso3",
#'   pg_connection = list(
#'       host = "localhost",
#'       dbname = "gis",
#'       port = 5432,
#'       user = "me",
#'       password = "pass"
#'       )
#'    )
#' }
elsar_load_data <- function(file_name = NULL,
                            file_path = NULL,
                            file_lyr = NULL,
                            wkt_filter = NULL,
                            db_info = NULL,
                            pg_connection = NULL,
                            drop3d = TRUE,
                            iso3_column = NULL,
                            iso3 = NULL) {

  # Prepare spatial filter (WKT) if needed
  wkt_str <- NULL
  if (!is.null(wkt_filter) && inherits(wkt_filter, c("sf", "SpatRaster", "SpatVector"))) {
    wkt_str <- terra::ext(wkt_filter) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      sf::st_set_crs(terra::crs(wkt_filter)) %>%
      sf::st_transform("EPSG:4326") %>%
      sf::st_geometry() %>%
      sf::st_as_text()
  }

  # Handle PostGIS
  if (is.null(file_path) &&
      !is.null(file_name) && file_name == "postgres") {
    if (is.null(pg_connection) && is.null(db_info)) {
      stop("Postgres connection info required.")
    }
    con <- if (is.null(pg_connection)) {
      RPostgres::dbConnect(RPostgres::Postgres(), !!!db_info)
    } else {
      RPostgres::dbConnect(RPostgres::Postgres(), !!!pg_connection)
    }
    return(sf::st_read(
      dsn = con,
      query = glue::glue("SELECT * FROM {file_lyr} WHERE {iso3_column} = '{iso3}'")
    ))
  }

  # Local file handling
  input_path <- if (!is.null(file_name)) {
    file.path(file_path, file_name)
  } else {
    file_path
  }

  file_type <- get_file_type(input_path)


  if (file_type %in% c("tif", "tiff", "grd", "gri", "nc", "hdf")) {
    return(if (!is.null(file_lyr))
      terra::rast(input_path, lyrs = file_lyr)
      else
        terra::rast(input_path))
  }

  # Vector formats
  if (file_type == "shp" && (is.null(file_name) || length(file_name) > 1)) {
    shapefiles <- if (is.null(file_name)) {
      list.files(file_path, pattern = "\\.shp$", full.names = TRUE)
    } else {
      file.path(file_path, file_name)
    }
    all_data <- lapply(shapefiles, function(f)
      filter_sf(f, iso3, iso3_column, drop3d = drop3d, wkt_filter = wkt_str, file_type = file_type))
    all_data <- Filter(Negate(is.null), all_data)
    all_cols <- unique(unlist(lapply(all_data, names)))
    all_data <- lapply(all_data, function(x) {
      missing <- setdiff(all_cols, names(x))
      for (col in missing)
        x[[col]] <- NA
      x[, all_cols]
    })
    return(dplyr::bind_rows(all_data))
  }

  if (file_type %in% c("gpkg", "gdb") && is.null(file_lyr)) {
    all_layers <- sf::st_layers(input_path)$name
    all_data <- lapply(all_layers, function(lyr)
      filter_sf(input_path, iso3, iso3_column, lyr, drop3d, wkt_str, file_type))
    all_data <- Filter(Negate(is.null), all_data)
    all_cols <- unique(unlist(lapply(all_data, names)))
    all_data <- lapply(all_data, function(x) {
      missing <- setdiff(all_cols, names(x))
      for (col in missing)
        x[[col]] <- NA
      x[, all_cols]
    })
    return(dplyr::bind_rows(all_data))
  }

  # Single vector layer
  return(filter_sf(input_path, iso3, iso3_column, layer_name = file_lyr, drop3d = drop3d, wkt_filter = wkt_str, file_type = file_type))
  }

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

 # Input validation
  assertthat::assert_that(
    is.null(file_name) || is.character(file_name),
    msg = "'file_name' must be NULL or a character string."
  )

  assertthat::assert_that(
    is.null(file_path) || is.character(file_path),
    msg = "'file_path' must be NULL or a character string."
  )

  assertthat::assert_that(
    is.logical(drop3d),
    msg = "'drop3d' must be TRUE or FALSE."
  )

  # Prepare spatial filter as a geometry (keep its CRS). filter_sf reprojects
  # it into each layer's CRS before applying, since GDAL applies a spatial
  # filter in the layer's own CRS - a filter in another CRS would match nothing.
  filter_geom <- NULL
  if (!is.null(wkt_filter)) {
    assertthat::assert_that(
      inherits(wkt_filter, c("sf", "SpatRaster", "SpatVector")),
      msg = "'wkt_filter' must be an sf, SpatRaster, or SpatVector object."
    )
    log_message("Preparing spatial filter from input extent...")
    filter_geom <- terra::ext(wkt_filter) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      sf::st_set_crs(terra::crs(wkt_filter)) %>%
      sf::st_transform("EPSG:4326") %>%
      sf::st_geometry()
  }

  # Handle PostGIS
  if (is.null(file_path) &&
      !is.null(file_name) && file_name == "postgres") {
    if (is.null(pg_connection) && is.null(db_info)) {
      stop("Postgres connection info required. Provide 'db_info' or 'pg_connection'.")
    }
    assertthat::assert_that(
      !is.null(file_lyr),
      msg = "'file_lyr' (table name) is required when loading from PostgreSQL."
    )
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
      stop("Package 'RPostgres' is required to load from PostgreSQL. ",
           "Install it with install.packages(\"RPostgres\").", call. = FALSE)
    }
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("Package 'DBI' is required to load from PostgreSQL. ",
           "Install it with install.packages(\"DBI\").", call. = FALSE)
    }

    log_message("Connecting to PostgreSQL database...")
    # Use do.call to splice the named connection list into dbConnect(). The
    # rlang splice operator `!!!` does not work here: dbConnect() is a plain S4
    # generic, not a quoting function, so `!!!x` would be parsed as `!(!(!x))`.
    params <- if (!is.null(pg_connection)) pg_connection else db_info
    con <- do.call(RPostgres::dbConnect, c(list(RPostgres::Postgres()), params))
    # Always close the connection, even if the read errors.
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Build the query with properly quoted identifiers and literals to avoid
    # SQL injection and to handle names that need quoting.
    tbl <- DBI::dbQuoteIdentifier(con, file_lyr)
    query <- if (!is.null(iso3) && !is.null(iso3_column)) {
      col <- DBI::dbQuoteIdentifier(con, iso3_column)
      val <- DBI::dbQuoteLiteral(con, iso3)
      glue::glue("SELECT * FROM {tbl} WHERE {col} = {val}")
    } else {
      glue::glue("SELECT * FROM {tbl}")
    }
    log_message("Loading table '{file_lyr}' from PostgreSQL...")
    return(sf::st_read(dsn = con, query = query))
  }

  # Local file handling
  input_path <- if (!is.null(file_name)) {
    file.path(file_path, file_name)
  } else {
    file_path
  }

  file_type <- get_file_type(input_path)
  log_message("Detected file type: {file_type}")

  # Raster formats
  if (file_type %in% c("tif", "tiff", "grd", "gri", "nc", "hdf")) {
    log_message("Loading raster data from '{input_path}'...")
    result <- if (!is.null(file_lyr)) {
      terra::rast(input_path, lyrs = file_lyr)
    } else {
      terra::rast(input_path)
    }
    assertthat::assert_that(
      inherits(result, "SpatRaster"),
      msg = "Failed to load raster data."
    )
    log_message("Loaded raster with {terra::nlyr(result)} layer(s).")
    return(result)
  }

  # Vector formats - multiple shapefiles
  if (file_type == "shp" && (is.null(file_name) || length(file_name) > 1)) {
    shapefiles <- if (is.null(file_name)) {
      list.files(file_path, pattern = "\\.shp$", full.names = TRUE)
    } else {
      file.path(file_path, file_name)
    }
    log_message("Loading {length(shapefiles)} shapefile(s)...")
    all_data <- lapply(shapefiles, function(f)
      filter_sf(f, iso3, iso3_column, drop3d = drop3d, wkt_filter = filter_geom, file_type = file_type))
    return(combine_layers(all_data, shapefiles, noun = "file"))
  }

  # Vector formats - multi-layer files without specified layer
 if (file_type %in% c("gpkg", "gdb") && is.null(file_lyr)) {
    all_layers <- sf::st_layers(input_path)$name
    log_message("Loading all {length(all_layers)} layers from '{file_type}' file...")
    all_data <- lapply(all_layers, function(lyr)
      filter_sf(input_path, iso3, iso3_column, lyr, drop3d, filter_geom, file_type))
    return(combine_layers(all_data, all_layers, noun = "layer"))
  }

  # Single vector layer
  log_message("Loading vector data from '{input_path}'...")
  result <- filter_sf(input_path, iso3, iso3_column, layer_name = file_lyr, drop3d = drop3d, wkt_filter = filter_geom, file_type = file_type)
  # filter_sf() returns NULL only when the read itself failed (a successful read
  # with no matching rows returns a 0-row sf). So NULL here is a genuine error,
  # not an empty result - surface it instead of silently returning a non-sf NULL.
  if (is.null(result)) {
    stop(glue::glue("Failed to read '{input_path}'. See the message above for the error."),
         call. = FALSE)
  }
  log_message("Loaded {nrow(result)} features.")
  return(result)
}

#' Combine per-layer read results into a single sf, surfacing read failures
#'
#' Aggregates the results of several [filter_sf()] calls (one per file or
#' layer). `NULL` entries are failed reads; successful reads are `sf` objects
#' (possibly with zero rows). Unlike a bare `dplyr::bind_rows()`, this:
#'
#' * errors if *every* read failed (rather than returning a `0 x 0` tibble that
#'   is not an `sf` object), and
#' * warns, but continues, if *some* reads failed, naming which.
#'
#' @param results List returned from [filter_sf()] calls; `NULL` = failed read.
#' @param sources Character vector of the file paths / layer names read, in the
#'   same order as `results`, used in messages.
#' @param noun Character. `"file"` or `"layer"`, for messages.
#' @return An `sf` object combining all successful reads (possibly zero rows).
#' @keywords internal
combine_layers <- function(results, sources, noun = "layer") {
  n_total <- length(results)
  failed <- vapply(results, is.null, logical(1))
  n_failed <- sum(failed)

  if (n_total == 0) {
    stop(glue::glue("No {noun}s found to load."), call. = FALSE)
  }
  if (n_failed == n_total) {
    stop(
      glue::glue("Failed to read all {n_total} {noun}(s). ",
                 "See the messages above for the per-{noun} errors."),
      call. = FALSE
    )
  }
  if (n_failed > 0) {
    warning(
      glue::glue("{n_failed} of {n_total} {noun}(s) failed to read and were ",
                 "skipped: {paste(sources[failed], collapse = ', ')}"),
      call. = FALSE
    )
  }

  ok <- results[!failed]
  all_cols <- unique(unlist(lapply(ok, names)))
  ok <- lapply(ok, function(x) {
    missing <- setdiff(all_cols, names(x))
    for (col in missing) {
      x[[col]] <- NA
    }
    x[, all_cols]
  })
  result <- dplyr::bind_rows(ok)
  log_message("Loaded {nrow(result)} features from {length(ok)} {noun}(s).")
  result
}

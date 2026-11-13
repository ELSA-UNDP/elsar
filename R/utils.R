#' Validate an ISO3-style region code
#'
#' Accepts a standard 3-letter ISO3 country code (e.g. `"ECU"`) or a regional
#' variant with a suffix used for sub-national runs (e.g. `"ECU_REG"`,
#' `"ECU-GEF8"`). The code must begin with three letters; an optional suffix of
#' letters, digits, underscores or hyphens may follow.
#'
#' Used by functions that take an `iso3` for filtering or output naming, so that
#' a custom regional code (relying on a supplied boundary rather than ISO lookup)
#' is not rejected.
#'
#' @param iso3 Character. The code to validate.
#'
#' @return `TRUE`/`FALSE`.
#' @keywords internal
is_valid_iso3 <- function(iso3) {
  is.character(iso3) && length(iso3) == 1 && !is.na(iso3) &&
    grepl("^[A-Za-z]{3}([_-][A-Za-z0-9]+)*$", iso3)
}

#' National part of an ISO3-style region code
#'
#' Global datasets (KBAs, Ramsar, WDPCA, GHSL urban centres) carry a national
#' ISO3 attribute, so a sub-national code such as `"USA_CA"` must be matched
#' as `"USA"`. A plain 3-letter code is returned unchanged.
#'
#' @param iso3 Character. An ISO3-style code accepted by [is_valid_iso3()].
#'
#' @return The first three characters of `iso3`, in upper case.
#' @keywords internal
iso3_base <- function(iso3) {
  # Upper case: global datasets (KBA, Ramsar, WDPCA, GHSL) use upper-case
  # codes, and a lower-case code ("npl") used to match nothing, silently
  toupper(substr(iso3, 1, 3))
}

#' Rescale Raster to 0-1 Range
#'
#' This function rescales values in a raster to a range between 0 and 1.
#'
#' @param raster_in Input `SpatRaster` to be rescaled.
#' @param raster_in_min Optional numeric. Minimum value of input raster. If NULL, will be calculated.
#' @param raster_in_max Optional numeric. Maximum value of input raster. If NULL, will be calculated.
#' @param new_min Numeric. New minimum of rescaled raster (default = 0).
#' @param new_max Numeric. New maximum of rescaled raster (default = 1).
#'
#' @return A `SpatRaster` with values rescaled to the specified range. A
#'   constant layer returns `new_max` where its value is positive and `new_min`
#'   otherwise (with a message), instead of `NaN`. Note that with
#'   `invert = TRUE` in [make_normalised_raster()], a constant positive layer
#'   is inverted first and so becomes `new_min` everywhere.
#' @export
rescale_raster <- function(
    raster_in,
    raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min,
    raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max,
    new_min = 0,
    new_max = 1) {
  if (is.null(raster_in_min)) raster_in_min <- terra::global(raster_in, min, na.rm = TRUE)$min
  if (is.null(raster_in_max)) raster_in_max <- terra::global(raster_in, max, na.rm = TRUE)$max
  if (isTRUE(raster_in_max - raster_in_min <= 0)) {
    # A constant layer has no range: dividing by zero would give NaN everywhere
    # (e.g. a region fully covered by protected areas). Keep the information
    # that it is uniformly present (new_max) or absent (new_min).
    value <- if (isTRUE(raster_in_max > 0)) new_max else new_min
    # Common and harmless for empty layers (e.g. no KBAs in the country), so
    # report it without a warning
    log_message("rescale_raster: the layer is constant ({signif(raster_in_max, 4)}); returning {value} wherever it has data.")
    return(terra::ifel(is.na(raster_in), NA, value))
  }
  new_min + (raster_in - raster_in_min) * ((new_max - new_min) / (raster_in_max - raster_in_min))
}

#' Convert Points to Buffered Polygons Based on Area
#'
#' This function converts POINT or MULTIPOINT geometries into circular polygons using the specified area attribute
#' to calculate a radius (assuming the area is in hectares or another square unit). The buffer is created in a projected
#' CRS (default: Mollweide, EPSG:54009) and then transformed back to the input CRS. Optionally, original polygon features
#' can be retained and combined with the buffered features.
#'
#' @param sf_layer An `sf` object containing geometries, including points or multipoints and an area column.
#' @param area_attr Character. Name of the attribute column that contains site area (default: `"REP_AREA"`).
#' @param area_crs Character. CRS used for buffering operation (default: `"ESRI:54009"` = World Mollweide).
#' @param nQuadSegs Integer. Number of segments per circle quadrant for buffering (default: `50`).
#' @param append_original_polygons Logical. If `TRUE`, appends original polygons to buffered features (default: `TRUE`).
#' @param area_multiplier Numeric. Multiplier applied to the area attribute to convert units (e.g., `1e4` for hectares to m2).
#'
#' @return An `sf` object containing polygon features (either buffered points, original polygons, or both). The `sf` object can be empty.
#' If no valid features are found, returns `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' buffered <- convert_points_polygon(
#'   sf_layer = my_kba_layer,
#'   area_attr = "gisarea",
#'   area_crs = "ESRI:54009",
#'   append_original_polygons = TRUE
#' )
#' }
convert_points_polygon <- function(
    sf_layer,
    area_attr = "REP_AREA",
    area_crs = "ESRI:54009",
    nQuadSegs = 50,
    append_original_polygons = TRUE,
    area_multiplier = 1e6) {

  # Basic input checks
  if (!inherits(sf_layer, "sf")) {
    stop("sf_layer must be an sf object.")
  }
  if (!is.numeric(nQuadSegs) || nQuadSegs <= 0) {
    stop("nQuadSegs must be a positive integer.")
  }

  is_point <- sf::st_is(sf_layer, c("POINT", "MULTIPOINT"))
  # Polygons are kept as they are: their area attribute is not needed (WDPCA
  # polygons often report 0 or NA). Before 0.4.2 they were dropped too.
  polygons <- sf_layer[!is_point, ]
  points <- sf_layer[is_point, ]

  points_buffered <- NULL
  had_points <- nrow(points) > 0
  if (had_points) {
    if (!area_attr %in% names(sf_layer)) {
      stop(paste("The area attribute", area_attr, "does not exist in the input sf_layer."))
    }
    area <- suppressWarnings(as.numeric(sf::st_drop_geometry(points)[[area_attr]]))
    points <- points[!is.na(area) & area > 0, ]
    area <- area[!is.na(area) & area > 0]
  }

  if (nrow(points) == 0) {
    if (had_points) warning("No point geometries with area attributes found.")
  } else {
    # A multipoint's reported area is shared between its points (before 0.4.2
    # each point got a circle of the full area)
    points$.feature <- seq_len(nrow(points))
    points$.area <- area
    # Cast to MULTIPOINT first: casting a mixed POINT/MULTIPOINT column straight
    # to POINT keeps only the first point of each multipoint
    single <- suppressWarnings(sf::st_cast(sf::st_cast(points, "MULTIPOINT"), "POINT"))
    n_parts <- tabulate(single$.feature, nbins = nrow(points))
    part_area <- single$.area / n_parts[single$.feature]

    points_buffered <- sf::st_buffer(
      sf::st_transform(single, crs = area_crs),
      dist = sqrt(part_area * area_multiplier / pi),
      nQuadSegs = nQuadSegs
    )
    points_buffered <- sf::st_transform(points_buffered, crs = sf::st_crs(sf_layer))
    points_buffered$.feature <- NULL
    points_buffered$.area <- NULL
  }

  out <- if (append_original_polygons) {
    if (is.null(points_buffered)) polygons else dplyr::bind_rows(polygons, points_buffered)
  } else {
    points_buffered
  }
  if (is.null(out) || nrow(out) == 0) {
    warning("No valid point or polygon features found. Returning an empty sf object.")
    return(sf_layer[0, ])
  }
  out
}


#' Extract Filename and Filetype from Directory
#'
#' Extracts the base filename and file extension for a given `data_name` in a specified folder.
#'
#' @param data_name Character. Name pattern of the file to search.
#' @param file_path Character. Path to the folder.
#'
#' @return A named list with elements: `filename` and `filetype`.
#' @export
extract_filename_filetype <- function(data_name, file_path) {
  my_files <- list.files(path = file_path)
  input_string <- my_files[grep(data_name, my_files)]

  last_dot_position <- utils::tail(gregexpr("\\.", input_string)[[1]], 1)
  filetype <- substring(input_string, last_dot_position + 1)
  filename <- substring(input_string, 1, last_dot_position)

  if ("shp" %in% filetype) {
    index <- which(filetype == "shp")
    filetype <- "shp"
    filename <- filename[index]
  }

  return(list(filetype = filetype, filename = filename))
}

#' Calculate Areal Coverage of a Zone Relative to Planning Units
#'
#' Calculates percentage of a zone layer relative to the total area of planning units.
#'
#' @param zone_layer A binary `SpatRaster` representing a target zone.
#' @param pu_layer A binary `SpatRaster` representing planning units.
#'
#' @return Numeric. Percentage of zone coverage across PUs.
#' @export
get_coverage <- function(zone_layer, pu_layer) {
  # Count the zone only within the planning units (coded 1; NA or 0 outside),
  # so coverage cannot exceed 100% (before 0.4.2 cells outside counted too)
  zone_in_pus <- terra::mask(zone_layer, pu_layer, maskvalues = c(NA, 0))
  terra::global(zone_in_pus, sum, na.rm = TRUE)$sum /
    terra::global(pu_layer, sum, na.rm = TRUE)$sum * 100
}


#' Rasterise Feature Attributes onto Planning Units, Weighted by Coverage
#'
#' Rasterises an attribute of vector features onto the planning-unit grid,
#' using the exact share of each planning unit that each feature covers.
#'
#' How features are combined in a planning unit depends on `fun`:
#' * `mean` (default): the coverage-weighted mean of the features present,
#'   `sum(value * coverage) / sum(coverage)`. A planning unit only partly
#'   covered by one feature gets that feature's value.
#' * `sum`: `sum(value * coverage)`, so uncovered parts count as 0 (use for
#'   area-weighted totals or shares).
#' * `max`, `min`: the largest or smallest value among the features present.
#'
#' Planning units with no feature are 0. Since 0.4.2 one
#' [exactextractr::exact_extract()] pass handles any number of features with
#' the same rule; before, `mean` averaged over *all* features (including ones
#' not touching the planning unit) and the method changed above 1,000 features.
#'
#' @param features An `sf` or `SpatVector` object containing the vector features to rasterise.
#' @param attribute Character. The column name in `features` holding the value to rasterise.
#' @param iso3 Character. ISO3 country code, passed to [make_normalised_raster()].
#' @param pus A `SpatRaster` defining the planning-unit grid.
#' @param invert Logical. If `TRUE`, inverts the resulting values during normalisation (default: `FALSE`).
#' @param rescaled Logical. If `TRUE`, rescales the output to 0-1 using [make_normalised_raster()] (default: `TRUE`).
#' @param fun How overlapping features are combined: `mean` (default), `sum`,
#'   `max` or `min`, as a function or a string.
#' @param cores,max_exact_features Ignored since 0.4.2 (kept so existing calls
#'   still work).
#'
#' @return A `SpatRaster` on the planning-unit grid.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Rasterise polygons using a 'score' attribute
#' result <- exact_rasterise(features = my_polygons, attribute = "score", pus = my_raster, fun = sum)
#' }
exact_rasterise <- function(
    features,
    attribute,
    iso3,
    pus,
    invert = FALSE,
    rescaled = TRUE,
    fun = mean,
    cores = 4,
    max_exact_features = 1000
) {
  fun_name <- .rasterise_fun_name(fun, deparse(substitute(fun)))

  # Validate inputs
  assertthat::assert_that(
    inherits(features, "sf") || inherits(features, "SpatVector"),
    msg = "'features' must be an 'sf' or 'SpatVector' object."
  )
  if (inherits(features, "SpatVector")) features <- sf::st_as_sf(features)
  assertthat::assert_that(
    attribute %in% names(features),
    msg = glue::glue("Attribute '{attribute}' not found in 'features'.")
  )
  assertthat::assert_that(
    inherits(pus, "SpatRaster"),
    msg = "'pus' must be a 'SpatRaster' object."
  )
  assertthat::assert_that(
    is_valid_iso3(iso3),
    msg = "'iso3' must be a 3-letter ISO3 code, optionally with a regional suffix (e.g. ECU_REG)."
  )

  features <- features[!sf::st_is_empty(features), ]

  if (nrow(features) == 0) {
    log_message("exact_rasterise: no features; returning an empty layer.")
    r <- terra::ifel(pus == 1, 0, NA)
  } else {
    features <- sf::st_make_valid(sf::st_transform(features, sf::st_crs(pus)))
    features <- .polygons_only(features)
    raw <- sf::st_drop_geometry(features)[[attribute]]
    # A factor's level codes are not its values (factor("10") is code 1)
    if (is.factor(raw)) raw <- as.character(raw)
    values <- suppressWarnings(as.numeric(raw))
    assertthat::assert_that(
      sum(is.na(values)) == sum(is.na(raw)),
      msg = glue::glue("Attribute '{attribute}' must be numeric.")
    )
    log_message("exact_rasterise: {nrow(features)} feature(s), fun = {fun_name}.")

    cells <- exactextractr::exact_extract(
      pus, features, include_cell = TRUE, progress = FALSE
    )
    cells <- dplyr::bind_rows(lapply(seq_along(cells), function(i) {
      d <- cells[[i]]
      d <- d[d$coverage_fraction > 0, c("cell", "coverage_fraction"), drop = FALSE]
      if (nrow(d) > 0) d$value <- values[i]
      d
    }))
    cells <- cells[!is.na(cells$value), , drop = FALSE]

    r <- terra::ifel(pus == 1, 0, NA)
    if (nrow(cells) > 0) {
      # Base rowsum()/ordering: far faster than grouped dplyr on millions of rows
      if (fun_name %in% c("mean", "sum")) {
        totals <- rowsum(cbind(cells$value * cells$coverage_fraction, cells$coverage_fraction),
                         cells$cell, reorder = FALSE)
        cell_ids <- as.numeric(rownames(totals))
        v <- if (fun_name == "mean") totals[, 1] / totals[, 2] else totals[, 1]
      } else {
        ord <- order(cells$cell, if (fun_name == "max") -cells$value else cells$value)
        first <- ord[!duplicated(cells$cell[ord])]
        cell_ids <- cells$cell[first]
        v <- cells$value[first]
      }
      r[cell_ids] <- v
      r <- terra::mask(r, pus)
    }
  }

  # Normalise the final result
  log_message("Normalising output...")
  make_normalised_raster(
    raster_in = r,
    pus = pus,
    iso3 = iso3,
    invert = invert,
    rescaled = rescaled
  )
}

# The name of an aggregation for exact_rasterise(): "mean", "sum", "max" or
# "min", given a function (base or terra versions) or a string
.rasterise_fun_name <- function(fun, expr = "") {
  allowed <- c("mean", "sum", "max", "min")
  if (is.character(fun)) {
    name <- fun
  } else {
    name <- NA_character_
    # terra's mean, sum, ... are S4 generics: use their generic name
    if (isS4(fun) && "generic" %in% methods::slotNames(fun)) name <- as.character(fun@generic)
    for (nm in allowed) {
      if (identical(fun, match.fun(nm)) || identical(fun, get(nm, envir = baseenv()))) {
        name <- nm
      }
    }
    if (is.na(name)) name <- sub("^.*::", "", expr) # e.g. a call written as terra::mean
  }
  assertthat::assert_that(
    name %in% allowed,
    msg = "'fun' must be mean, sum, max or min."
  )
  name
}

#' Crop a Global Raster to the Extent of Planning Units
#'
#' This function crops a large global raster to the spatial extent of a planning units raster (`pus`).
#' It reprojects the `pus` extent to the coordinate reference system of the input global raster
#' to ensure accurate cropping. This is useful for pre-processing global inputs before using
#' functions like `make_normalised_raster()` to reduce processing time.
#'
#' @param raster_in SpatRaster. A large input raster (e.g., global dataset).
#' @param pus SpatRaster. Planning units raster used to define the target extent.
#' @param threads Ignored since 0.4.2 (kept so existing calls still work).
#'
#' @return A cropped SpatRaster with the same CRS as `raster_in` and extent matching the reprojected `pus`.
#' @export
#'
#' @examples
#' \dontrun{
#' cropped <- crop_global_raster(global_raster, pus) |>
#'   elsar::make_normalised_raster(pus = pus, iso3 = "KEN")
#' }
crop_global_raster <- function(raster_in, pus, threads = TRUE) {
  assertthat::assert_that(inherits(raster_in, "SpatRaster"),
                          msg = "'raster_in' must be a SpatRaster.")
  assertthat::assert_that(inherits(pus, "SpatRaster"),
                          msg = "'pus' must be a SpatRaster.")

  # Crop to the planning-unit footprint, projected into the input's CRS. Only
  # "no overlap" gives an empty layer; any other error (memory, a bad file) is
  # raised. Before 0.4.2 every error became an all-0 layer without notice.
  log_message("Cropping raster to PU layer...")
  cropped <- .crop_to(raster_in, pus)
  if (is.null(cropped)) {
    log_message("The input raster does not overlap the planning units; returning an empty raster.")
    return(terra::ifel(pus == 1, 0, NA))
  }
  cropped
}

#' Log a message with timestamp
#'
#' Prints a timestamped message to the console. Supports glue-style interpolation
#' of variables from the calling environment. Optionally prints and returns a variable
#' value, useful for debugging pipelines.
#'
#' @param msg Character. Message to log, supports glue syntax for variable interpolation.
#' @param variable Optional. If provided, appends the value to the message and returns it invisibly.
#'
#' @return Invisibly returns NULL, or the variable if provided.
#' @export
#'
#' @examples
#' # Simple message
#' log_message("Starting the process...")
#'
#' # With variable interpolation
#' iso3 <- "KEN"
#' log_message("Processing {iso3}...")
#'
#' # With variable return (useful in pipelines)
#' result <- log_message("Computed value", 42)
log_message <- function(msg, variable = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  glued_msg <- glue::glue(msg, .envir = parent.frame())

  if (!is.null(variable)) {
    message(glue::glue("[{timestamp}] {glued_msg}: {toString(variable)}"))
    return(invisible(variable))
  } else {
    message(glue::glue("[{timestamp}] {glued_msg}"))
    return(invisible(NULL))
  }
}

#' @rdname log_message
#' @export
log_msg <- function(msg, variable = NULL) {
 .Deprecated("log_message")
  log_message(msg, variable)
}

#' Infer file type from file path or directory
#'
#' This utility function takes a path (to a file or folder) and returns the detected geospatial file type,
#' such as `"shp"`, `"gpkg"`, `"tif"`, etc. It checks extensions for files and scans directory contents
#' for recognizable formats (e.g., `.shp`, `.gdb`).
#'
#' @param path Character. A file path or directory path to inspect.
#'
#' @return A character string indicating the file type (e.g., `"shp"`, `"gpkg"`, `"tif"`).
#' @export
#'
#' @examples
#' get_file_type("data/boundaries.shp")     # returns "shp"
#' get_file_type("data/vector_layers.gpkg") # returns "gpkg"
#' \dontrun{
#' get_file_type("data/rasters")            # scans folder for known formats
#' }
get_file_type <- function(path) {
  if (dir.exists(path)) {
    # Check if path itself is a .gdb folder
    if (grepl("\\.gdb$", path)) return("gdb")

    # If path is a folder, scan inside it
    files <- list.files(path, full.names = TRUE)
    if (any(grepl("\\.shp$", files))) return("shp")

    stop("Cannot determine file type from folder contents.")
  }

  # Otherwise, infer from file extension
  ext <- tolower(tools::file_ext(path))
  known_types <- c("shp", "gpkg", "geojson", "gdb", "tif", "tiff", "grd", "gri", "nc", "hdf")
  if (ext %in% known_types) return(ext)

  stop(glue::glue("Unsupported file extension: {ext}"))
}

#' Load and optionally filter a single vector layer
#'
#' This utility function wraps `sf::st_read()` to load a vector dataset (e.g., from GPKG, GDB, or SHP).
#' It supports optional filtering by ISO3 country code and/or a spatial filter in WKT format.
#' It automatically selects the first available layer if `layer_name` is not provided and the file type supports layers.
#'
#' @param file_path Character. Full file path to the vector dataset.
#' @param iso3 Character or NULL. ISO3 country code used for attribute filtering.
#' @param iso3_column Character or NULL. Column in the dataset to match `iso3`.
#' @param layer_name Character or NULL. Optional. Specific layer name for multi-layer formats.
#' @param drop3d Logical. If TRUE, drops Z and M dimensions from geometries.
#' @param wkt_filter Character or NULL. Optional WKT string used for spatial filtering.
#' @param file_type Character or NULL. File type used to determine if layer name inference is required.
#'
#' @param warn_empty Logical. Warn when an ISO3 filter matches no features
#'   (default `TRUE`). Loaders that read several layers and combine them set it
#'   to `FALSE` and check the combined result instead.
#' @return An `sf` object with valid geometry, or `NULL` on failure.
#' @export
#'
#' @examples
#' \dontrun{
#' filter_sf("data/layers.gpkg", iso3 = "KEN", iso3_column = "ISO3")
#' filter_sf("data/boundary.shp", wkt_filter = "POLYGON((...))")
#' }
filter_sf <- function(file_path,
                      iso3 = NULL,
                      iso3_column = NULL,
                      layer_name = NULL,
                      drop3d = TRUE,
                      wkt_filter = NULL,
                      file_type = NULL,
                      warn_empty = TRUE) {

  # If no layer name provided and the format supports layers, pick the first
  # one. Guard the read: st_layers() throws on an unreadable/corrupt file, and
  # this call is outside the st_read() tryCatch below, so without this guard a
  # bad file would error out of filter_sf() instead of returning NULL (its
  # documented failure contract, which callers rely on to skip bad layers).
  if (is.null(layer_name)) {
    layer_info <- tryCatch(sf::st_layers(file_path), error = function(e) {
      message("Failed to read layer info from file: ", file_path,
              "\nError: ", e$message)
      NULL
    })
    if (is.null(layer_info)) {
      return(NULL)
    }
    layer_name <- layer_info$name[1]
  }

  # Reproject a spatial-filter geometry into the layer's CRS before use. GDAL
  # applies a spatial filter in the layer's own CRS, so a filter supplied in a
  # different CRS (e.g. a pre-projected input) would otherwise match nothing.
  # (A filter passed as a ready WKT string is used as-is.)
  if (!is.null(wkt_filter) && !is.character(wkt_filter)) {
    layer_crs <- tryCatch({
      li <- sf::st_layers(file_path)
      idx <- match(layer_name, li$name)
      if (!is.na(idx)) li$crs[[idx]] else NA
    }, error = function(e) NA)
    fg <- sf::st_geometry(wkt_filter)
    if (!is.na(sf::st_crs(layer_crs)) && !is.na(sf::st_crs(fg)) &&
        sf::st_crs(fg) != sf::st_crs(layer_crs)) {
      fg <- sf::st_transform(fg, layer_crs)
    }
    wkt_filter <- sf::st_as_text(fg)
  }

  # Build SQL query for attribute filtering if needed
  query <- if (!is.null(iso3) && !is.null(iso3_column) && !is.null(layer_name)) {
    log_message("Building SQL query to filter layer {layer_name} by ISO3 code...")
    glue::glue("SELECT * FROM \"{layer_name}\" WHERE \"{iso3_column}\" = '{iso3}'")
  } else NULL

  if(!is.null(wkt_filter) && is.null(query)){
    log_message("Applying a spatial filter to the input data...")
  }

  # Dynamically build st_read() call
  dat <- tryCatch({
    args <- list(dsn = file_path, quiet = TRUE)
    # (sf ignores, and warns about, a layer when a query is given)
    if (!is.null(layer_name) && is.null(query)) args$layer <- layer_name
    if (!is.null(query)) args$query <- query
    if (!is.null(wkt_filter)) args$wkt_filter <- wkt_filter
    do.call(sf::st_read, args)
  }, error = function(e) {
    message("Failed to read layer: ", layer_name, " in file: ", file_path, "\nError: ", e$message)
    return(NULL)
  })

  # An ISO3 filter that matches nothing usually means a wrong iso3_column or
  # code; say so instead of silently returning no features
  if (warn_empty && !is.null(dat) && !is.null(query) && nrow(dat) == 0) {
    warning("No features in layer '", layer_name, "' where ", iso3_column, " = '", iso3,
            "'. Check 'iso3' and 'iso3_column'.", call. = FALSE)
  }

  # Drop Z/M geometry dimensions if requested
  if (!is.null(dat) && drop3d) {
    log_message("Dropping 3D geometries (if present)...")
    dat <- sf::st_zm(dat, drop = TRUE, what = "ZM")
  }

  # Ensure geometry is valid
  if (!is.null(dat)) {
    log_message("Repairing any geometry errors...")
    dat <- sf::st_make_valid(dat)
  }

  return(dat)
}

#' Split a Bounding Box into a Regular Grid of Polygon Tiles
#'
#' Given an input `sf` object, this function extracts its bounding box and
#' divides it into a regular grid of rectangular polygon tiles using a specified
#' number of columns and rows.
#'
#' This is useful for spatially chunking large geometries to speed up
#' operations like intersection or cropping, especially when used with spatial
#' indexing.
#'
#' @param bbox_sf An `sf` object. Only the bounding box is used; geometries inside are ignored.
#' @param ncols Integer. Number of columns to split the bounding box into. Default is 2.
#' @param nrows Integer. Number of rows to split the bounding box into. Default is 2.
#'
#' @return An `sf` object consisting of rectangular polygons covering the bounding box of the input.
#'         Each polygon represents a tile in the grid. All geometries are valid.
#' @export
#'
#' @examples
#' \dontrun{
#'   world <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   tiles <- split_bbox_into_tiles(world, ncols = 3, nrows = 3)
#'   plot(st_geometry(world))
#'   plot(st_geometry(tiles), add = TRUE, border = "red")
#' }
#'
split_bbox_into_tiles <- function(bbox_sf, ncols = 2, nrows = 2) {
  # Get bounding box coordinates
  bbox <- sf::st_bbox(bbox_sf)

  # Define breaks along x (longitude) and y (latitude) axes
  x_breaks <- seq(bbox["xmin"], bbox["xmax"], length.out = ncols + 1)
  y_breaks <- seq(bbox["ymin"], bbox["ymax"], length.out = nrows + 1)

  tiles <- list()

  # Loop over grid positions to build each rectangle
  for (i in seq_len(ncols)) {
    for (j in seq_len(nrows)) {
      coords <- rbind(
        c(x_breaks[i],     y_breaks[j]),
        c(x_breaks[i+1],   y_breaks[j]),
        c(x_breaks[i+1],   y_breaks[j+1]),
        c(x_breaks[i],     y_breaks[j+1]),
        c(x_breaks[i],     y_breaks[j])  # Close polygon
      )
      tile <- sf::st_polygon(list(coords))
      tiles <- append(tiles, list(tile))
    }
  }

  # Combine tiles into an sf object with matching CRS, ensuring validity
  sf::st_sf(geometry = sf::st_sfc(tiles, crs = sf::st_crs(bbox_sf))) %>%
    sf::st_make_valid()
}

#' Conditionally Subdivide Bounding Box into Grid Tiles Based on Geographic Extent
#'
#' This function checks whether the geographic extent of an `sf` object's bounding box
#' exceeds a specified threshold in degrees (longitude or latitude). If so, it splits
#' the bounding box into a regular grid of rectangular tiles of approximately `tile_size_deg`
#' degrees in size. If not, it simply returns the original input.
#'
#' This is useful to improve performance when running spatial operations (e.g., `intersect`)
#' on large countries or regions by breaking the work into spatial chunks that benefit
#' from spatial indexing.
#'
#' @param bbox_sf An `sf` object. Only the bounding box is used for subdivision logic.
#' @param degree_threshold Numeric. Threshold in degrees of lat/lon span at which to trigger subdivision. Default is 10.
#' @param tile_size_deg Numeric. Approximate tile width/height in degrees. Default is 5.
#'
#' @return An `sf` object. Either:
#'   - A grid of tiles (rectangular polygons) if subdivision is triggered, or
#'   - The original input `bbox_sf` unchanged.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   world <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   tiles <- conditionally_subdivide_bbox(world)
#'   plot(st_geometry(world))
#'   plot(st_geometry(tiles), add = TRUE, border = "red")
#' }
conditionally_subdivide_bbox <- function(bbox_sf,
                                         degree_threshold = 10,
                                         tile_size_deg = 5) {
  bbox <- sf::st_bbox(bbox_sf)

  # Calculate longitude and latitude spans
  lon_span <- bbox["xmax"] - bbox["xmin"]
  lat_span <- bbox["ymax"] - bbox["ymin"]

  # Trigger subdivision only if extent exceeds the threshold
  if (lon_span > degree_threshold || lat_span > degree_threshold) {
    ncols <- ceiling(lon_span / tile_size_deg)
    nrows <- ceiling(lat_span / tile_size_deg)

    # Optional message for logging context (you can remove iso3 if undefined)
    log_message("Input spans a large area ({round(lon_span, 1)} degrees longitude by {round(lat_span, 1)} degrees latitude) - subdividing into ~{tile_size_deg} degree tiles (grid: {ncols} x {nrows})")

    # Create grid tiles
    tiles <- split_bbox_into_tiles(bbox_sf, ncols = ncols, nrows = nrows)
    return(tiles)
  } else {
    # Return original as a single tile
    return(bbox_sf)
  }
}

#' Save a Raster as a Cloud Optimized GeoTIFF (COG)
#'
#' This helper function saves a `SpatRaster` to disk as a Cloud Optimized GeoTIFF (COG) with ZSTD compression.
#' It automatically sets the appropriate GDAL predictor based on the data type to optimize file size and performance.
#' For floating point rasters (`FLT4S`), Predictor 3 is used, and for integer rasters, Predictor 2 is used, which improves compressibility.
#' A nodata value is also automatically defined and applied: 255 for integer rasters (e.g. `"INT1U"`) and `NaN` for floating point rasters.
#'
#' This function should be used across processing functions to ensure consistent raster output formats.
#'
#' @param raster A `SpatRaster` object to be saved.
#' @param filename Character. Full file path (including `.tif` extension) where the raster will be saved.
#' @param datatype Character. GDAL data type to use for saving the raster (e.g. `"FLT4S"` for float, `"INT1U"` for unsigned byte). Default is `"FLT4S"`.
#' @param resampling Character or NULL. Overview resampling method (e.g. `"average"`, `"nearest"`, `"mode"`). If `NULL` (default), it is chosen from `datatype`: `"average"` for float, `"nearest"` for integer. Use `"mode"` for multi-class categorical layers such as land cover so overviews keep the dominant class.
#'
#' @return None. The function is called for its side effect of saving the raster to disk.
#'
#'@export
#'
#' @examples
#' \dontrun{
#' save_raster(my_raster, "output/my_raster.tif", datatype = "INT1U")
#' save_raster(my_raster, "output/my_raster_float.tif", datatype = "FLT4S")
#' }
#'
save_raster <- function(raster, filename, datatype = "FLT4S", resampling = NULL) {
  # NoData and compression predictor per data type. Before 0.4.2 every type
  # other than FLT4S got NoData 255 and predictor 2: FLT8S failed, and in
  # INT2U/INT4U data the valid value 255 became NoData.
  types <- list(
    FLT4S = list(nodata = NaN, predictor = "3"),
    FLT8S = list(nodata = NaN, predictor = "3"),
    INT1U = list(nodata = 255, predictor = "2"),
    INT1S = list(nodata = -128, predictor = "2"),
    INT2U = list(nodata = 65535, predictor = "2"),
    INT2S = list(nodata = -32768, predictor = "2"),
    INT4U = list(nodata = 4294967295, predictor = "2"),
    INT4S = list(nodata = -2147483648, predictor = "2")
  )
  assertthat::assert_that(
    datatype %in% names(types),
    msg = paste0("'datatype' must be one of: ", paste(names(types), collapse = ", "), ".")
  )
  type <- types[[datatype]]

  # Overview resampling. Caller may override; otherwise default by datatype:
  #   - float    -> AVERAGE  (smooth downsampling of continuous values)
  #   - integer  -> NEAREST  (preserves binary/presence values)
  # For multi-class categorical layers (e.g. land cover) pass resampling = "mode"
  # so overviews keep the dominant class instead of an arbitrary pixel.
  resampling_value <- if (!is.null(resampling)) {
    toupper(resampling)
  } else if (startsWith(datatype, "FLT")) {
    "AVERAGE"
  } else {
    "NEAREST"
  }

  # Write raster to disk as Cloud Optimized GeoTIFF (COG). Overviews are built
  # (OVERVIEWS=AUTO) so the output is a fully valid COG - required by strict COG
  # readers/servers such as TiTiler/rio-cogeo, which reject COGs without them.
  .muffle_cog_warnings(
    terra::writeRaster(
      raster,
      filename = filename,
      filetype = "COG",
      datatype = datatype,
      NAflag = type$nodata,
      gdal = c(
        "COMPRESS=ZSTD",
        glue::glue("PREDICTOR={type$predictor}"),
        "NUM_THREADS=ALL_CPUS",
        "OVERVIEWS=AUTO",
        glue::glue("OVERVIEW_RESAMPLING={resampling_value}")
      ),
      overwrite = TRUE
    )
  )

  log_message("Saved: {filename}.")
  invisible(filename)
}

# Instruction appended to median_from_rast() errors. The side-car must sit next
# to the file terra actually opened (terra::sources() resolves symlinks).
sidecar_fix_hint <- function(tif) {
  size_gb <- suppressWarnings(file.size(tif) / 1024^3)
  cost <- if (!is.na(size_gb) && size_gb >= 1) {
    paste0(" (reads the whole ", round(size_gb), " GB file once; ~",
           max(1, round(size_gb / 8)), " min)")
  } else {
    ""
  }
  paste0("This raster needs a precomputed histogram. Create it with:\n",
         "  gdalinfo -hist '", tif, "'", cost, "\n",
         "or from R: sf::gdal_utils('info', '", tif, "', options = '-hist')")
}

#' Compute median from a SpatRaster using its GDAL PAM side-car histogram
#'
#' Extracts the histogram stored in the GDAL PAM side-car XML file
#' (e.g. `raster.tif.aux.xml`) and computes the exact median value
#' by finding the bucket midpoint where the cumulative count reaches 50%.
#'
#' @param r A `SpatRaster` (from **terra**) whose underlying file has
#'   a GDAL PAM side-car XML (e.g. `raster.tif.aux.xml`).
#'
#' @details
#' Before running this function, you must generate the histogram side‑car:
#' in a terminal, run:
#' ```bash
#' gdalinfo -hist /path/to/your.tif
#' ```
#' This computes and stores the histogram in `/path/to/your.tif.aux.xml`.
#' Once the side-car exists, calling this function reads the stored
#' `<Histogram>` data without re-scanning pixel values and returns
#' the exact median bucket midpoint.
#'
#' @return A single numeric value: the median of the raster (bucket midpoint).
#'
#' @examples
#' \dontrun{
#' library(terra)
#' r <- rast("eii_2023.tif")
#' # First, in the shell:
#' #   gdalinfo -hist eii_2023.tif
#' med <- median_from_rast(r)
#' }
#'
#' @export
median_from_rast <- function(r) {
  stopifnot(inherits(r, "SpatRaster"))
  tif  <- terra::sources(r)[1]
  xmlf <- paste0(tif, ".aux.xml")
  if (!file.exists(xmlf)) {
    alt <- sub("\\.tif$", ".xml", tif, ignore.case = TRUE)
    if (file.exists(alt)) {
      xmlf <- alt
    } else {
      stop("Side-car XML not found for ", tif, "\n",
           sidecar_fix_hint(tif), call. = FALSE)
    }
  }

  doc      <- xml2::read_xml(xmlf)
  histItem <- xml2::xml_find_first(doc, ".//HistItem")
  if (is.na(histItem)) {
    stop("No <HistItem> in side-car XML: ", xmlf, "\n",
         sidecar_fix_hint(tif), call. = FALSE)
  }

  minv        <- as.numeric(xml2::xml_text(xml2::xml_find_first(histItem, "./HistMin")))
  maxv        <- as.numeric(xml2::xml_text(xml2::xml_find_first(histItem, "./HistMax")))
  bucketCount <- as.integer(xml2::xml_text(xml2::xml_find_first(histItem, "./BucketCount")))

  counts_str  <- xml2::xml_text(xml2::xml_find_first(histItem, "./HistCounts"))
  counts      <- as.numeric(strsplit(counts_str, "\\|")[[1]])
  if (length(counts) != bucketCount) {
    warning("Declared BucketCount (", bucketCount,
            ") != number of counts (", length(counts), ").")
  }

  edges <- seq(minv, maxv, length.out = length(counts) + 1L)
  mids  <- (head(edges, -1L) + tail(edges, -1L)) / 2

  half <- sum(counts) / 2
  idx  <- which(cumsum(counts) >= half)[1]

  mids[idx]
}

#' Extract binary layers within a raster stack
#'
#' This function extracts all layers in a raster stack that have either c(0,1) or only 1 or only 0.
#' This is needed to split the raster stack saved from the pipeline into two raster stacks, one int and one float.
#'
#' @param raster_stack A `terra` `SpatRaster` containing several raster layers, some of which can be binary.
#'
#' @return Two `terra` `SpatRaster` raster stacks, one containing only the binary layers, and one containing the float layers
#' @export
#'
#' @examples
#' \dontrun{
#' raster_out <- get_binary_layers(raster_stack = stack)
#'
#' int_stack <- raster_out[[1]]
#' float_stack <- raster_out[[2]]
#' }
get_binary_layers <- function(raster_stack) {
  assertthat::assert_that(
    inherits(raster_stack, "SpatRaster"),
    msg = "'raster_stack' must be a SpatRaster object."
  )

  if (terra::nlyr(raster_stack) == 0) {
    log_message("Raster stack has 0 layers - returning empty result.")
    return(list(NULL, NULL))
  }

  log_message("Analyzing {terra::nlyr(raster_stack)} layer(s) for binary values...")

  # A layer is binary when every value is exactly 0 or 1, tested layer by layer
  # (before 0.4.2: values were rounded to 4 digits first, so near-0 or near-1
  # floats counted as binary and were then written as integers, and an all-NA
  # layer shifted the labels of the others). All-NA layers are kept as float.
  # One pass over the whole stack (a per-layer loop was ~7x slower on disk)
  other <- terra::global(raster_stack != 0 & raster_stack != 1, "sum", na.rm = TRUE)[[1]]
  has_data <- terra::global(raster_stack, "notNA")[[1]] > 0
  binary_mask <- other == 0 & has_data

  int_layers <- if (any(binary_mask)) raster_stack[[which(binary_mask)]] else NULL
  float_layers <- if (any(!binary_mask)) raster_stack[[which(!binary_mask)]] else NULL
  log_message("Found {sum(binary_mask)} binary layer(s) and {sum(!binary_mask)} float layer(s).")

  list(int_layers, float_layers)
}



# Keep only polygon geometry: extract polygons from geometry collections (which
# st_make_valid() can produce) and drop points and lines, which coverage
# fractions cannot use
.polygons_only <- function(x) {
  if (any(sf::st_is(x, "GEOMETRYCOLLECTION"))) {
    x <- sf::st_collection_extract(x, "POLYGON")
  }
  x[sf::st_is(x, c("POLYGON", "MULTIPOLYGON")), ]
}

# Evaluate a terra::writeRaster(filetype = "COG") call without GDAL's
# creation-option warnings. GDAL builds a COG through an in-memory copy and
# warns that the MEM driver ignores the creation options. When terra writes to
# disk in chunks (large rasters, or terraOptions(todisk = TRUE) as the pipeline
# sets), it writes a plain GeoTIFF first, which warns that GTiff has no
# OVERVIEWS options; the COG step then applies them. Both are harmless. Other
# warnings pass through.
.muffle_cog_warnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    msg <- conditionMessage(w)
    if (grepl("driver (MEM|GTiff) does not support creation option", msg)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Warn that a layer carries no information for prioritisation
#'
#' Raised when every ecosystem gets the same score, so the layer cannot rank
#' planning units (e.g. Denmark: both WWF ecoregions are 100% below the
#' global EII median). The warning has class `elsar_uninformative_layer`, so
#' callers such as the pipeline can catch it and leave the layer out.
#'
#' @param layer Character. Layer name, for the message.
#' @param reason Character. Why the layer is uniform.
#' @return Invisibly NULL; called for the warning.
#' @keywords internal
warn_uninformative <- function(layer, reason) {
  msg <- paste0(layer, " carries no information for prioritisation: ", reason)
  warning(structure(
    class = c("elsar_uninformative_layer", "warning", "condition"),
    list(message = msg, call = NULL, layer = layer)
  ))
  invisible(NULL)
}

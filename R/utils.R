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
#' @return A `SpatRaster` with values rescaled to the specified range.
#' @export
rescale_raster <- function(
    raster_in,
    raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min,
    raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max,
    new_min = 0,
    new_max = 1) {
  if (is.null(raster_in_min)) raster_in_min <- terra::global(raster_in, min, na.rm = TRUE)$min
  if (is.null(raster_in_max)) raster_in_max <- terra::global(raster_in, max, na.rm = TRUE)$max
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

  if (!area_attr %in% names(sf_layer)) {
    stop(paste("The area attribute", area_attr, "does not exist in the input sf_layer."))
  }

  if (!is.numeric(nQuadSegs) || nQuadSegs <= 0) {
    stop("nQuadSegs must be a positive integer.")
  }

  # Filter for points with area values
  points_with_area <- sf_layer %>%
    dplyr::filter(!!rlang::sym(area_attr) > 0 &
                    sf::st_geometry_type(.) %in% c("POINT", "MULTIPOINT"))

  # Create circular buffers for valid point features
  if (nrow(points_with_area) == 0) {
    warning("No point geometries with area attributes found.")
  } else {
    points_transformed <- sf::st_transform(points_with_area, crs = area_crs)

    points_buffered <- sf::st_buffer(
      points_transformed,
      dist = sqrt((as.numeric(sf::st_drop_geometry(points_with_area)[[area_attr]]) * area_multiplier) / pi),
      nQuadSegs = nQuadSegs
    )

    # Reproject back to original CRS
    points_buffered <- sf::st_transform(points_buffered, crs = sf::st_crs(sf_layer))
  }

  # Append original polygons if requested
  if (append_original_polygons) {
    polygons_with_area <- sf_layer %>%
      dplyr::filter(!!rlang::sym(area_attr) > 0 &
                      !(sf::st_geometry_type(.) %in% c("POINT", "MULTIPOINT")))

    if (nrow(polygons_with_area) == 0 && exists("points_buffered", inherits = FALSE)) {
      return(points_buffered)
    } else if (nrow(polygons_with_area) > 0 && exists("points_buffered", inherits = FALSE)) {
      return(rbind(polygons_with_area, points_buffered))
    } else if (nrow(polygons_with_area) > 0 && !exists("points_buffered", inherits = FALSE)) {
      return(polygons_with_area)
    } else {
      warning("No valid point or polygon features found with area attributes. Returning an empty sf object.")
      return(sf_layer[0, ])
    }
  } else {
    if (exists("points_buffered", inherits = FALSE)) {
      return(points_buffered)
    } else {
      warning("No point features matched the criteria. Returning an empty sf object.")
      return(sf_layer[0, ])
    }
  }
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
  terra::global(zone_layer, sum, na.rm = TRUE)$sum /
    terra::global(pu_layer, sum, na.rm = TRUE)$sum * 100
}

#' Efficient Attribute-Weighted Rasterization Using Coverage Fraction
#'
#' Rasterizes vector features to a raster grid defined by a planning unit (`pus`) layer,
#' assigning values based on a given attribute and the actual coverage fraction of
#' each feature over each raster cell. This method is optimized for speed and precision
#' and is especially useful when features overlap or vary in size and shape.
#'
#' If multiple features are provided, each is rasterized independently and aggregated
#' across the stack using the specified function (`fun`), typically `sum`, `mean`, or `max`.
#'
#' @param features An `sf` or `SpatVector` object containing the vector features to rasterize.
#' @param attribute Character. The column name in `features` to use as a weight for raster values.
#' @param iso3 Character. ISO3 country code, passed to `make_normalised_raster()`.
#' @param pus A `SpatRaster` object defining the resolution, extent, and CRS of the output raster.
#' @param invert Logical. If `TRUE`, inverts the resulting values during normalization (default: `FALSE`).
#' @param rescaled Logical. If `TRUE`, rescales the output to 0-1 using `make_normalised_raster()` (default: `TRUE`).
#' @param fun Function. Aggregation function applied across overlapping rasterized features (default: `mean`).
#' @param cores Integer. Number of CPU cores to use for multi-core processing (default: 4).
#'
#' @return A `SpatRaster` object representing the attribute-weighted rasterization of the input features.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Rasterize polygons using a 'score' attribute
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
    cores = 4
) {

  # Validate inputs
  assertthat::assert_that(
    inherits(features, "sf") || inherits(features, "SpatVector"),
    msg = "'features' must be an 'sf' or 'SpatVector' object."
  )

  assertthat::assert_that(
    attribute %in% colnames(features),
    msg = glue::glue("Attribute '{attribute}' not found in 'features'.")
  )

  assertthat::assert_that(
    inherits(pus, "SpatRaster"),
    msg = "'pus' must be a 'SpatRaster' object."
  )

  assertthat::assert_that(
    is.character(iso3) && nchar(iso3) == 3,
    msg = "'iso3' must be a 3-letter country code string."
  )

  # Initialize an empty raster stack
  r_stack <- terra::rast()

  # Handle multiple features by rasterizing each individually and stacking
  if (nrow(features) > 1) {
    for (i in 1:nrow(features)) {
      f <- dplyr::slice(features, i)
      attr_val <- dplyr::pull(f, attribute)        # Get attribute value directly

      # Rasterize the coverage fraction of this feature
      f_r <- exactextractr::coverage_fraction(pus, f)[[1]]

      # Multiply by the attribute value (to weight by that attribute)
      f_r <- f_r * attr_val

      r_stack <- c(r_stack, f_r)

      log_msg(glue::glue("{round(100 * i / nrow(features), 1)}% complete"))
    }

    # Aggregate across all rasterized layers using the specified function
    log_msg(glue::glue("Aggregating layers..."))
    r_stack <- terra::app(
      r_stack,
      cores = cores,
      fun = fun,
      na.rm = TRUE
    )

  } else {
    # Single feature: get coverage fraction directly.
    log_msg(glue::glue("Calculating weighted coverage fraction using a single feature..."))
    r_stack <- exactextractr::coverage_fraction(pus, features)[[1]]
    r_stack <- r_stack * dplyr::pull(features, attribute)
  }

  # Normalise the final result
  log_msg(glue::glue("Normalising output..."))
  result <- elsar::make_normalised_raster(
    raster_in = r_stack,
    pus = pus,
    iso3 = iso3,
    invert = invert,
    rescaled = rescaled
  )

  return(result)
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
#' @param threads Optional method to use multi-core processing - to speed on some `terra` functions (default: `TRUE`).
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

  # Attempt to project and crop
  tryCatch({
    log_msg("Projecting PUs to match global input raster...")
    pus_extent <- pus %>%
      terra::project(
        terra::crs(raster_in),
        threads = threads
        ) %>%
      terra::ext()

    log_msg("Cropping raster to PU layer...")
    cropped <- terra::crop(
      raster_in,
      pus_extent,
      extend = TRUE
    )

    return(cropped)
  }, error = function(e) {
    log_msg(glue::glue("Warning: crop failed: {e$message}; returning an empty raster."))
    return(terra::ifel(pus == 1, 0, NA))  # Or NULL if you want to explicitly handle failure
  })
}

#' Log a timestamped message to the console
#'
#' Utility function to print a timestamped message using `message()`, which
#' works well with progress bars and other console output.
#'
#' @param msg A character string to log.
#' @export
#'
#' @examples
#' log_msg("Starting the process...")
log_msg <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M")
  message(glue::glue("[{timestamp}] {msg}"), appendLF = TRUE)
  invisible(NULL)
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
#' get_file_type("data/rasters")            # scans folder for known formats
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
                      file_type = NULL) {

  # If no layer name provided and the format supports layers, pick the first one
  if (is.null(layer_name)) {
    layer_info <- sf::st_layers(file_path)
    layer_name <- layer_info$name[1]
  }

  # Build SQL query for attribute filtering if needed
  query <- if (!is.null(iso3) && !is.null(iso3_column) && !is.null(layer_name)) {
    log_msg(glue::glue("Building SQL query to filter layer {layer_name} by ISO3 code..."))
    glue::glue("SELECT * FROM \"{layer_name}\" WHERE \"{iso3_column}\" = '{iso3}'")
  } else NULL

  if(!is.null(wkt_filter) && is.null(query)){
  log_msg(glue::glue("Applying a spatial filter to the input data..."))
  }

  # Dynamically build st_read() call
  dat <- tryCatch({
    args <- list(dsn = file_path, quiet = TRUE)
    if (!is.null(layer_name)) args$layer <- layer_name
    if (!is.null(query)) args$query <- query
    if (!is.null(wkt_filter)) args$wkt_filter <- wkt_filter
    do.call(sf::st_read, args)
  }, error = function(e) {
    message("Failed to read layer: ", layer_name, " in file: ", file_path, "\nError: ", e$message)
    return(NULL)
  })

  # Drop Z/M geometry dimensions if requested
  if (!is.null(dat) && drop3d) {
    log_msg("Dropping 3D geometries (if present)...")
    dat <- sf::st_zm(dat, drop = TRUE, what = "ZM")
  }

  # Ensure geometry is valid
  if (!is.null(dat)) {
    log_msg("Repairing any geometry errors...")
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
    log_msg(glue::glue("Input spans a large area ({round(lon_span, 1)} degrees longitude by {round(lat_span, 1)} degrees latitude) - subdividing into ~{tile_size_deg} degree tiles (grid: {ncols} x {nrows})"))

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
save_raster <- function(raster, filename, datatype = "FLT4S") {
  # Determine GDAL predictor: 3 for float (better for continuous), 2 for integer (better for categorical)
  predictor_value <- ifelse(datatype == "FLT4S", "3", "2")

  # Define nodata value: NaN for float, 255 for integer (common convention)
  nodata_value <- if (datatype == "FLT4S") NaN else 255

  # Write raster to disk as Cloud Optimized GeoTIFF (COG)
  terra::writeRaster(
    raster,
    filename = filename,
    filetype = "COG",
    datatype = datatype,
    NAflag = nodata_value,
    gdal = c(
      "COMPRESS=ZSTD",
      glue::glue("PREDICTOR={predictor_value}"),
      "NUM_THREADS=ALL_CPUS",
      "OVERVIEWS=NONE"
    ),
    overwrite = TRUE
  )

  # Log saved file path
  log_msg(glue::glue("Saved: {filename}."))
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
      stop("Side-car XML not found for ", tif)
    }
  }

  doc      <- xml2::read_xml(xmlf)
  histItem <- xml2::xml_find_first(doc, ".//HistItem")
  if (is.na(histItem)) stop("No <HistItem> in side-car XML: ", xmlf)

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







#' Rescale Raster to 0–1 Range
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
#' Converts point geometries to circular polygon buffers based on an area attribute.
#'
#' @param wdpa_layer An `sf` object with point or multipoint geometries and an area field.
#' @param area_attr Name of the area attribute column (default = "REP_AREA").
#' @param area_crs CRS to use for buffering (default = 'ESRI:54009' = World Mollweide).
#' @param nQuadSegs Number of segments per circle quadrant used in buffering (default = 50).
#' @param append_sf Logical. If TRUE, append buffered geometries to existing polygons (default = TRUE).
#' @param area_multiplier Numeric. Scaling multiplier for area units (e.g., 1e6 for km^2).
#'
#' @return An `sf` object with polygons for both original and buffered geometries.
#' @export
convert_points_polygon <- function(
    wdpa_layer,
    area_attr = "REP_AREA",
    area_crs = "ESRI:54009",
    nQuadSegs = 50,
    append_sf = TRUE,
    area_multiplier = 1e6) {

  if (!inherits(wdpa_layer, "sf")) {
    stop("wdpa_layer must be an sf object.")
  }

  if (!area_attr %in% names(wdpa_layer)) {
    stop(paste("The area attribute", area_attr, "does not exist in wdpa_layer."))
  }

  if (!is.numeric(nQuadSegs) || nQuadSegs <= 0) {
    stop("nQuadSegs must be a positive integer.")
  }

  points_with_area <- wdpa_layer %>%
    dplyr::filter(!!rlang::sym(area_attr) > 0 &
                    sf::st_geometry_type(.) %in% c("POINT", "MULTIPOINT"))

  if (nrow(points_with_area) == 0) {
    warning("No point geometries with area attributes found.")
    return(NULL)
  }

  points_transformed <- sf::st_transform(points_with_area, crs = area_crs)

  points_buffered <- sf::st_buffer(
    points_transformed,
    dist = sqrt((as.numeric(sf::st_drop_geometry(points_with_area)[[area_attr]]) * area_multiplier) / pi),
    nQuadSegs = nQuadSegs
  )

  points_buffered <- sf::st_transform(points_buffered, crs = sf::st_crs(wdpa_layer))

  if (append_sf) {
    polygons_with_area <- wdpa_layer %>%
      dplyr::filter(!!rlang::sym(area_attr) > 0 &
                      !(sf::st_geometry_type(.) %in% c("POINT", "MULTIPOINT")))

    polygon_sf <- rbind(polygons_with_area, points_buffered)
    return(polygon_sf)
  } else {
    return(points_buffered)
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
#' @param rescaled Logical. If `TRUE`, rescales the output to 0–1 using `make_normalised_raster()` (default: `TRUE`).
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

      cat(glue::glue("Feature {i} processed."), "\n")
    }

    # Aggregate across all rasterized layers using the specified function
    cat(glue::glue("Aggregating layers..."), "\n")
    r_stack <- terra::app(
      r_stack,
      cores = cores,
      fun = fun,
      na.rm = TRUE
    )

  } else {
    # Single feature: get coverage fraction directly.
    cat(glue::glue("Calculating weighted coverage fraction using a single feature..."), "\n")
    r_stack <- exactextractr::coverage_fraction(pus, features)[[1]]
    r_stack <- r_stack * attr_val
  }

  # Normalise the final result
  cat(glue::glue("Normalising output..."), "\n")
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
#'
#' @return A cropped SpatRaster with the same CRS as `raster_in` and extent matching the reprojected `pus`.
#' @export
#'
#' @examples
#' \dontrun{
#' cropped <- crop_global_raster(global_raster, pus) |>
#'   elsar::make_normalised_raster(pus = pus, iso3 = "KEN")
#' }
crop_global_raster <- function(raster_in, pus) {
  assertthat::assert_that(inherits(raster_in, "SpatRaster"),
                          msg = "'raster_in' must be a SpatRaster.")
  assertthat::assert_that(inherits(pus, "SpatRaster"),
                          msg = "'pus' must be a SpatRaster.")

  # Project extent of PUs to match raster CRS
  pus_extent <- pus %>%
    terra::project(terra::crs(raster_in)) %>%
    terra::ext()

  # Crop raster to extent
  cropped <- terra::crop(
    raster_in,
    pus_extent,
    extend = TRUE)

  return(cropped)
}



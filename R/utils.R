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

#' Calculate Underrepresented Ecosystems Across a Set
#'
#' Applies a weighted averaging function across a set of ecosystem coverage rasters.
#'
#' @param x An `sf` or `data.frame` with one row per ecosystem polygon and a `target` column.
#' @param pus A `SpatRaster` representing the planning units grid.
#' @param iso3 ISO3 country code used for naming.
#' @param invert Logical. If TRUE, invert values before normalization (default = FALSE).
#' @param rescaled Logical. If TRUE, normalize result to [0, 1] (default = TRUE).
#' @param fun Function. Aggregation function to apply across rasters (default = mean).
#'
#' @return A normalized `SpatRaster` showing weighted average underrepresentation.
#' @export
get_underrepresented_ecosystems = function(
    x,
    pus,
    iso3 = iso3,
    invert = FALSE,
    rescaled = TRUE,
    fun = mean) {
  r_stack  = terra::rast()

  for (i in 1:nrow(x)) {
    f <- dplyr::slice(x, i)
    f_r <- exactextractr::coverage_fraction(pus, f)[[1]]
    f_r <- f_r * f$target
    r_stack <- c(r_stack, f_r)
  }

  r_out = terra::app(
    r_stack,
    fun = function(x, ...)
      fun(x, na.rm = TRUE)
  ) %>%
    elsar::make_normalised_raster(
      raster_in = .,
      pus = pus,
      iso3 = iso3,
      invert = invert,
      rescaled = rescaled
    )

  return(r_out)
}

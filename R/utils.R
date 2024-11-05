#' Rescale rasters to between 0 and 1
#'
#' @param raster_in Input raster
#' @param raster_in_min Min value of raster
#' @param raster_in_max Max value of raster
#' @param new_min 0
#' @param new_max 1
#'
#' @return `raster` file that is normalised from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' raster_rescaled <- rescale_raster(dat_aligned)
#' }
rescale_raster <- function(raster_in,
                           raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min,
                           raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max,
                           new_min = 0,
                           new_max = 1){
  if(is.null(raster_in_min)) raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min
  if(is.null(raster_in_max)) raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max
  new_min + (raster_in - raster_in_min) * ((new_max - new_min) / (raster_in_max - raster_in_min))
}


#' Create Buffered Polygons from Point Geometries
#'
#' This function creates polygon buffers around point geometries within a given spatial layer.
#' It uses the specified area attribute to determine the size of each buffer and transforms
#' geometries into a specified coordinate reference system for accurate area calculations.
#'
#' @param wdpa_layer An `sf` object containing point geometries and the area attribute.
#' @param area_attr A character string specifying the name of the area attribute. Default is 'REP_AREA'.
#' @param area_crs A character string specifying the coordinate reference system for area calculations. Default is 'ESRI:54009' (World Mollweide projection).
#' @param nQuadSegs An integer specifying the number of segments to use for buffering. Default is 50.
#' @param append_sf logical. If `TRUE`, returns the initial wdpa_layer with polygons instead of points. If `FALSE`, will only returned the points with buffers.
#'
#' @return An `sf` object with buffered polygon geometries.
#' @export
#'
#' @examples
#' \dontrun{
#'  pa <- wdpar::wdpa_fetch("NPL",
#'   wait = TRUE,
#'   download_dir = here::here()
#' )
#' buffered_polygons <- convert_points_polygon(wdpa_layer = pa,
#'       area_crs = sf::st_crs(pa))
#'       }
convert_points_polygon <- function(wdpa_layer,
                                   area_attr = "REP_AREA",
                                   area_crs = "ESRI:54009", # Calculates areas by default using the World Mollweide projection, per Protected Planet
                                   nQuadSegs = 50,
                                   append_sf = TRUE) {
  # Error checking for input types
  if (!inherits(wdpa_layer, "sf")) {
    stop("wdpa_layer must be an sf object.")
  }

  if (!area_attr %in% names(wdpa_layer)) {
    stop(paste("The area attribute", area_attr, "does not exist in wdpa_layer."))
  }

  if (!is.numeric(nQuadSegs) || nQuadSegs <= 0) {
    stop("nQuadSegs must be a positive integer.")
  }

  # Filter out points with area greater than 0 and geometry type of POINT or MULTIPOINT
  points_with_area <- wdpa_layer %>%
    dplyr::filter(!!rlang::sym(area_attr) > 0 &
                    sf::st_geometry_type(.) %in% c("POINT", "MULTIPOINT"))

  # Check if there are any points to process
  if (nrow(points_with_area) == 0) {
    warning("No point geometries with area attributes found.")
    return(NULL)
  }

  # Project to an appropriate CRS for buffering
  points_transformed <- sf::st_transform(points_with_area, crs = area_crs)

  # Calculate the buffer distance
  points_buffered <- sf::st_buffer(points_transformed,
                                   dist = sqrt((sf::st_drop_geometry(points_with_area)[[area_attr]] * 1e6) / pi),
                                   nQuadSegs = nQuadSegs
  )

  # Transform back to the original CRS
  points_buffered <- sf::st_transform(points_buffered, crs = sf::st_crs(wdpa_layer))

  if (append_sf) {
    polygons_with_area <- wdpa_layer %>%
      dplyr::filter(!!rlang::sym(area_attr) > 0 &
                      !(sf::st_geometry_type(.) %in% c("POINT", "MULTIPOINT")))

    polygon_sf <- polygons_with_area %>%
      rbind(points_buffered)

    return(polygon_sf)
  } else {
    return(points_buffered)
  }
}

#' Extract the file name and file type of data in the local path
#'
#' This is a helper function used in the wrapper function to load local data (can be extended later to work with postgres).
#'
#' @param data_name The name of the data of interest (needs to be in the way provided here in the file name)
#' @param file_path The local path where the data is saved.
#'
#' @return A `list` in the form of a dictionary that contains "filename" and "filetype". Can be used as inputs for [elsar_load_data()].
#' @export
#'
extract_filename_filetype <- function(data_name, file_path) {
  my_files <- list.files(path = file_path)
  input_string <- my_files[grep(data_name, my_files)]

  # Use gregexpr to find all occurrences of "." in the string (works also if there's another "." in the name)
  last_dot_position <- utils::tail(gregexpr("\\.", input_string)[[1]], 1)

  # Extract substring starting just after the last "."
  filetype <- substring(input_string, last_dot_position + 1)

  # Extract substring from the start of the string
  filename <- substring(input_string, 1, last_dot_position)

  if ("shp" %in% filetype) {
    index <- which(filetype == "shp")
    filetype <- "shp"
    filename <- filename[index]
  }

  return(list(filetype = filetype, filename = filename))
}

#' Calculate country areal coverage (as a percentage) of an input binary layer, likely representing a zone.
#'
#' @param zone_layer A binary `spatRast` representing a zone or protected area type feature.
#' @param pu_layer A binary `spatRast`` representing the planning units.
#' @return A vector of areal targets of length 1.
#'
#' @seealso [terra::global()] which this function wraps.
#' @export
#' @examples
#' \dontrun{
#'    get_coverage(PA, pu)
#'    get_coverage(zone_protect, pu)
#'    get_coverage(zone_manage, pu)
#' }
get_coverage <- function(zone_layer, pu_layer) {
  terra::global(zone_layer, sum, na.rm = TRUE)$sum / terra::global(pu_layer, sum, na.rm = TRUE)$sum * 100
}

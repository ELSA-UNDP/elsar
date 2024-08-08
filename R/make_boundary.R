#' Create a boundary of the planning region
#'
#' @param boundary_in A file containing the boundary information. Can be `sf` or `SpatRaster`
#' @param input_type A string that is either "sf" or "SpatRaster" (default is "sf").
#' @param limit_to_mainland Logical. Limits the extent of the data to mainland.
#' @param col_name A string of the column containing the actual extent of the planning region (not outside area). Can be `NULL`.
#' @param filter_out A value representing the outside area in the data (e.g. `0`)
#' @param do_project Logical. `TRUE`if custom projection for planning region is wanted.
#' @param iso3_column A string of the name of where iso3 information can be found in a dataset. Only needed when do_project = TRUE.
#' @param iso3 The iso3 country code (character) of the country of interest. Only needed when do_project = TRUE.
#' @param output_path An optional output path for the created file. Only needed when do_project = TRUE.
#'
#' @return `sf` object of the boundary of the planning region
#' @export
#'
#' @examples
#' \dontrun{
#' boundary <- make_boundary(
#'   boundary_src = "pu_nepal_450m.tif",
#'   data_path = path_to_data,
#'   input_type = "raster", col_name = "pu_nepal_450m"
#' )
#' }
make_boundary <- function(boundary_in,
                          input_type = "sf", # sf, raster
                          limit_to_mainland = FALSE,
                          col_name = NULL,
                          filter_out = 0,
                          do_project = FALSE,
                          iso3_column = "iso_sov1",
                          iso3,
                          output_path) {
  #load the file with load_file() outside of this function and then use it as input here
  if (input_type == "sf") {
    nb <- boundary_in
  } else if (input_type == "SpatRaster") {
    nb <- boundary_in %>%
      terra::as.polygons() %>%
      sf::st_as_sf()
  }

  if (limit_to_mainland == TRUE) { # exclude any islands etc.
    nb <- nb %>%
      sf::st_cast("POLYGON") %>%
      dplyr::slice(which.max(as.numeric(sf::st_area(.data)))) %>% # get largest polygon that represents mainland
      sf::st_transform(nb, crs = sf::st_crs(4326))
  } else {
    if (!is.null(col_name)) {
      nb <- nb %>%
        dplyr::filter(!!rlang::sym(col_name) != filter_out) # filter out anything that's not data (e.g. 0s, NAs)
    }
    nb <- nb %>%
      sf::st_transform(nb, crs = sf::st_crs(4326))
  }

  if (do_project) {
    wkt <- make_custom_projection(boundary = nb, output_path = output_path, iso3 = iso3)
    nb <- sf::st_transform(nb, crs = sf::st_crs(wkt))
  }

  boundary <- nb
}

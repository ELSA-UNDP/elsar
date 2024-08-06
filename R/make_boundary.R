#' Create a boundary of the planning region
#'
#' @param boundary_src A string of the data name and file type
#' @param boundary_lyr The layer used within the data (character or numeric).
#' @param data_path A string of the path where the data is saved. This is also where the custom projection is saved.
#' @param input_type A string that is either "sf", "postgres" or "raster" (default is "sf").
#' @param col_name A string of the column containing the actual extent of the planning region (not outside area). Can be `NULL`.
#' @param filter_out A value representing the outside area in the data (e.g. `0`)
#' @param do_project Logical. `TRUE`if custom projection for planning region is wanted.
#' @param iso3_column Only relevant when `ìnput_type` "postgres" is selected. A string of the name of where iso3 information can be found in a dataset.
#' @param iso3 The iso3 country code (character) of the country of interest.
#' @param output_path An optional output path for the created file.
#'
#' @return `sf` object of the boundary of the planning region
#' @export
#'
#' @examples
#' \dontrun{
#' boundary <- make_boundary(boundary_src = "pu_nepal_450m.tif",
#' data_path = path_to_data,
#' input_type = "raster", col_name = "pu_nepal_450m")
#' }
make_boundary <- function(boundary_src, # string of data name and file type
                          boundary_lyr = NULL, # layer within file
                          data_path,
                          input_type = "sf", # sf, postgres, raster)
                          limit_to_mainland = FALSE,
                          col_name = NULL,
                          filter_out = 0,
                          do_project = FALSE,
                          iso3_column = "iso_sov1",
                          iso3,
                          output_path
                          ) {
  # create path to data (same path will be needed later to save projection)
  boundary_src <- file.path(data_path, boundary_src)

  if (input_type == "postgres") { # can't check this
    # PostgreSQL Connection only, if needed
    dn <- dbConnect("PostgreSQL", dbname = boundary_src)
    nb <- st_read(dsn = dn, query = glue::glue("SELECT * FROM {boudnary_lyr} WHERE type = 'Land' AND {iso3_column} = '{iso3}'"))
  } else if (input_type == "sf") {
    nb <- sf::read_sf(boundary_src, boundary_lyr)
  } else if (input_type == "raster") {
    nb <- terra::rast(boundary_src, lyrs = boundary_lyr) %>%
      terra::as.polygons() %>%
      sf::st_as_sf()
  }

  if (limit_to_mainland == TRUE) { # exclude any islands etc.
    nb <- nb %>%
      sf::st_cast("POLYGON") %>%
      dplyr::slice(which.max(as.numeric(sf::st_area(.)))) %>% # get largest polygon that represents mainland
      sf::st_transform(nb, crs = st_crs(4326))
  } else {
    if(!is.null(col_name)) {
      nb <- nb %>%
        dplyr::filter(!!rlang::sym(col_name) != filter_out)# filter out anything that's not data (e.g. 0s, NAs)
    }
    nb <- nb %>%
      sf::st_transform(nb, crs = st_crs(4326))
  }

  if (do_project) {
    wkt <- make_custom_projection(boundary = nb, output_path = output_path, iso3 = iso3)
    nb <- sf::st_transform(nb, crs = sf::st_crs(wkt))
  }

  boundary <- nb
}

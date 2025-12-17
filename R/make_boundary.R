#' Create a boundary of the planning region
#'
#' @param boundary_in A file containing the boundary information. Can be `sf` or `SpatRaster`
#' @param input_type A string that is either "sf" or "SpatRaster" (default is "sf").
#' @param limit_to_mainland Logical. Limits the extent of the data to mainland.
#' @param col_name A string of the column containing the actual extent of the planning region (not outside area). Can be `NULL`.
#' @param filter_out A value representing the outside area in the data (e.g. `0`)
#' @param custom_projection Logical. `TRUE`if custom projection for planning region is wanted.
#' @param iso3 The iso3 country code (character) of the country of interest.
#' @param iso3_column Only relevant when `iso3` != NULL. A string of the name of where iso3 information can be found in a dataset.
#' @param output_path An optional output path for the created file. Only needed when custom_projection = TRUE.
#'
#' @return `sf` object of the boundary of the planning region
#' @export
#'
#' @examples
#' boundary <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
make_boundary <- function(boundary_in,
                          input_type = "sf", # sf, raster
                          limit_to_mainland = FALSE,
                          col_name = NULL,
                          filter_out = 0,
                          custom_projection = TRUE,
                          iso3 = NULL,
                          iso3_column = NULL,
                          output_path = NULL) {
  # Input validation
  assertthat::assert_that(
    input_type %in% c("sf", "SpatRaster"),
    msg = "'input_type' must be either 'sf' or 'SpatRaster'."
  )
  if (input_type == "sf") {
    assertthat::assert_that(inherits(boundary_in, "sf"),
                            msg = "'boundary_in' must be an sf object when input_type = 'sf'.")
  } else {
    assertthat::assert_that(inherits(boundary_in, "SpatRaster"),
                            msg = "'boundary_in' must be a SpatRaster when input_type = 'SpatRaster'.")
  }
  if (!is.null(iso3)) {
    assertthat::assert_that(assertthat::is.string(iso3),
                            msg = "'iso3' must be a character string.")
    assertthat::assert_that(!is.null(iso3_column),
                            msg = "'iso3_column' must be provided when 'iso3' is specified.")
  }
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  log_message("Creating boundary for planning region...")

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
      dplyr::slice(which.max(as.numeric(sf::st_area(.data)))) # get largest polygon that represents mainland
  } else {
    if (!is.null(col_name)) {
      nb <- nb %>%
        dplyr::filter(!!rlang::sym(col_name) != filter_out) # filter out anything that's not data (e.g. 0s, NAs)
    }
    nb <- nb %>%
      sf::st_transform(nb, crs = sf::st_crs(4326))
  }

  if (!is.null(iso3)) { #filter for right country if needed
    nb <- nb %>%
      dplyr::filter(!!rlang::sym(iso3_column) == iso3)
  }

  if (custom_projection) {
    wkt <- make_custom_projection(
      boundary = nb,
      output_path = output_path,
      iso3_column = iso3_column,
      iso3 = iso3
    )
    nb <- sf::st_transform(nb, crs = sf::st_crs(wkt))
  }

  return(nb)
}

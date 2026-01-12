#' Create a custom projection based on the planning region
#'
#' @param boundary `sf` object of the boundary of the planning region. Should match iso3 country code.
#' @param output_path An optional output path for the created file.
#' @param iso3_column A string of the name of where iso3 information can be found in a dataset.
#' @param iso3 The iso3 country code (character) of the country of interest.
#'
#' @return A `wkt` file centred on the planning region
#' @export
#'
#' @examples
#' boundary <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' wkt <- make_custom_projection(boundary = boundary, iso3 = "NPL")
make_custom_projection <- function(boundary,
                                   output_path = NULL,
                                   iso3_column = "iso3cd",
                                   iso3 = NULL) {
  # Input validation
  assertthat::assert_that(inherits(boundary, "sf"),
                          msg = "'boundary' must be an sf object.")
  if (!is.null(iso3)) {
    assertthat::assert_that(assertthat::is.string(iso3),
                            msg = "'iso3' must be a character string.")
  }
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  log_message("Creating custom Mollweide projection centred on planning region...")

  xmid <- mean(c(sf::st_bbox(boundary)$xmin, sf::st_bbox(boundary)$xmax))
  ymid <- mean(c(sf::st_bbox(boundary)$ymin, sf::st_bbox(boundary)$ymax))

  # Create WKT projection string
  wkt <- glue::glue('PROJCS["Mollweide_{iso3}",GEOGCS["GCS_unknown",
                  DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,
                  AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],
                  PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],
                  PROJECTION["Mollweide"],PARAMETER["central_meridian",{xmid}],
                  PARAMETER["false_easting",0],PARAMETER["false_northing",
                  {ymid}],UNIT["metre",1,AUTHORITY["EPSG","9001"]],
                  AXIS["Easting",EAST],AXIS["Northing",NORTH]]')
  # note: for marine it's not this straight forward if EEZ goes across date line

  if (!is.null(output_path)) {
    writeLines(wkt, glue::glue("{output_path}/Mollweide_{toupper(iso3)}.wkt")) # save wkt
  }

  return(wkt)
}

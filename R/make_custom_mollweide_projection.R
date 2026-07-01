#' Create a custom Mollweide projection centred on the planning region
#'
#' Builds an equal-area \strong{Mollweide} projection whose central meridian is
#' the longitude of the planning region's centre, and returns it as a WKT CRS
#' string. The boundary is transformed to WGS84 internally to compute the centre,
#' so any input CRS is handled correctly.
#'
#' @param boundary `sf` object of the boundary of the planning region. Should match iso3 country code.
#' @param output_path An optional output path for the created file.
#' @param iso3_column A string of the name of where iso3 information can be found in a dataset.
#' @param iso3 The iso3 country code (character) of the country of interest. Used only to name the projection / output file; the projection maths uses the boundary geometry.
#'
#' @return A `wkt` CRS string for a Mollweide projection centred on the planning region
#' @export
#'
#' @examples
#' boundary <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' wkt <- make_custom_mollweide_projection(boundary = boundary, iso3 = "NPL")
make_custom_mollweide_projection <- function(boundary,
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

  # Label for the projection name / output file. iso3 is optional (a custom
  # boundary may not have one); fall back to a generic label so glue() does not
  # collapse the WKT string to length zero when iso3 is NULL.
  iso3_label <- if (is.null(iso3)) "region" else iso3

  # The Mollweide central meridian is a longitude in DEGREES, so the centre must
  # be computed from geographic coordinates. If the boundary is in a projected
  # CRS (e.g. UTM, metres), transform to WGS84 first - otherwise xmid/ymid would
  # be eastings/northings in metres and the projection would be nonsensical
  # (e.g. central_meridian = 674875 instead of -79).
  if (!is.na(sf::st_crs(boundary)) && !sf::st_is_longlat(boundary)) {
    boundary <- sf::st_transform(boundary, 4326)
  } else if (is.na(sf::st_crs(boundary))) {
    log_message("Warning: boundary has no CRS; assuming geographic (WGS84) coordinates.")
  }

  xmid <- mean(c(sf::st_bbox(boundary)$xmin, sf::st_bbox(boundary)$xmax))
  ymid <- mean(c(sf::st_bbox(boundary)$ymin, sf::st_bbox(boundary)$ymax))

  # Create WKT projection string
  wkt <- glue::glue('PROJCS["Mollweide_{iso3_label}",GEOGCS["GCS_unknown",
                  DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,
                  AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],
                  PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],
                  PROJECTION["Mollweide"],PARAMETER["central_meridian",{xmid}],
                  PARAMETER["false_easting",0],PARAMETER["false_northing",
                  {ymid}],UNIT["metre",1,AUTHORITY["EPSG","9001"]],
                  AXIS["Easting",EAST],AXIS["Northing",NORTH]]')
  # note: for marine it's not this straight forward if EEZ goes across date line

  if (!is.null(output_path)) {
    writeLines(wkt, glue::glue("{output_path}/Mollweide_{toupper(iso3_label)}.wkt")) # save wkt
  }

  return(wkt)
}

#' @rdname make_custom_mollweide_projection
#' @description
#' `make_custom_projection()` is a deprecated alias for
#' `make_custom_mollweide_projection()` - the projection has always been
#' Mollweide; the clearer name should be preferred.
#' @export
make_custom_projection <- function(boundary,
                                   output_path = NULL,
                                   iso3_column = "iso3cd",
                                   iso3 = NULL) {
  .Deprecated("make_custom_mollweide_projection")
  make_custom_mollweide_projection(
    boundary = boundary,
    output_path = output_path,
    iso3_column = iso3_column,
    iso3 = iso3
  )
}

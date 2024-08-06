#' Create a custom projection based on the planning region
#'
#' @param boundary`sf` object of the boundary of the planning region. Should match iso3 country code.
#' @param output_path An optional output path for the created file.
#' @param iso3_column Only relevant when `ìnput_type` "postgres" is selected. A string of the name of where iso3 information can be found in a dataset.
#' @param iso3 The iso3 country code (character) of the country of interest.
#'
#' @return A `wkt` file centred on the planning region
#' @export
#'
#' @examples
#' \dontrun{
#' boundary <- make_boundary(boundary_src = "pu_nepal_450m.tif",
#' data_path = path_to_data,
#' input_type = "raster", col_name = "pu_nepal_450m")
#' wkt <- make_custom_projection(boundary = boundary, output_path = outputPath, iso3 = "NPL")
#' boundary_proj <- sf::st_transform(boundary, crs = sf::st_crs(wkt))
#' }
make_custom_projection <- function(boundary,
                                   output_path,
                                   iso3_column = "iso_sov1",
                                   iso3) {
  xmid <- mean(c(sf::st_bbox(boundary)$xmin, sf::st_bbox(boundary)$xmax))
  ymid <- mean(c(sf::st_bbox(boundary)$ymin, sf::st_bbox(boundary)$ymax))

  # Create WKT projection string
  wkt <- glue::glue('PROJCS["Mollweide_Custom_{iso3}",GEOGCS["GCS_unknown",
                  DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,
                  AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],
                  PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],
                  PROJECTION["Mollweide"],PARAMETER["central_meridian",{xmid}],
                  PARAMETER["false_easting",0],PARAMETER["false_northing",
                  {ymid}],UNIT["metre",1,AUTHORITY["EPSG","9001"]],
                  AXIS["Easting",EAST],AXIS["Northing",NORTH]]')
  # note: for marine it's not this straight forward if EEZ goes across date line
  writeLines(wkt, glue::glue("{output_path}/{tolower(iso3)}_proj.wkt")) # save wkt

  cCRS <- wkt
}

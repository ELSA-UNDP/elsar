make_planning_unit_layer <- function(nat_boundary_src,
                                     nat_boudnary_lyr = NULL,
                                     is_postgres = FALSE,
                                     pu_size = FALSE,
                                     is_projected = FALSE, # Is data already in non WGS84?
                                     limit_to_mainland = FALSE,
                                     iso3_column = "iso_sov1",
                                     iso3,
                                     output_dir = NULL) {
  require(terra)
  require(sf)

  # Calculate central Lat and Long

  if (is_postgres == TRUE) {
    require(RPostgreSQL)

    # PostgreSQL Connection only, if needed
    dn <- dbConnect("PostgreSQL", dbname = nat_boundary_src)
    nb <- st_read(dsn = dn, query = glue::glue("SELECT * FROM {nat_boudnary_lyr} WHERE type = 'Land' AND {iso3_column} = '{iso3}'"))
  } else {
    nb <- sf::read_sf(nat_boundary_src, nat_boudnary_lyr)
  }

  if (limit_to_mainland == TRUE) {
    nb <- nb %>%
      st_cast("POLYGON") %>%
      arrange(desc(st_area(.))) %>%
      head(1)

    if (is_projected == TRUE) {
      nb <- sf::st_transform(nb, crs = st_crs(4326))
    }
  } else {
    if (is_projected == TRUE) {
      nb <- sf::st_transform(nb, crs = st_crs(4326))
    }
  }

  bbox <- sf::st_bbox(nb)

  xmid <- mean(c(bbox[[1]], bbox[[3]]))

  ymid <- mean(c(bbox[[2]], bbox[[4]]))

  # Create WKT projection string
  wkt <- glue::glue('PROJCS["Mollweide_Custom_{iso3}",GEOGCS["GCS_unknown",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Mollweide"],PARAMETER["central_meridian",{xmid}],PARAMETER["false_easting",0],PARAMETER["false_northing",{ymid}],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]]')

  writeLines(wkt, glue::glue("custom_projections_wkt/{tolower(iso3)}_proj.wkt"))

  nb_proj <- sf::st_transform(nb, crs = sf::st_crs(wkt))

  if (pu_size == FALSE) {
    pu_sum <- 8.5e5

    pu_size <- units::drop_units(st_area(nb_proj)) / 1e6 / 8e5 * 1e3 # convert to edge length of PU

    pu_size <- case_when(
      pu_size < 100 ~ 100,
      between(pu_size, 100, 300) ~ plyr::round_any(pu_size, accuracy = 50, floor),
      pu_size > 300 ~ plyr::round_any(pu_size, accuracy = 100, floor)
    )

    while (pu_sum >= 8.5e5) {
      pu_size <- pu_size + case_when(
        between(pu_size, 100, 300) ~ 50,
        pu_size > 300 ~ 100
      )

      r1 <- terra::rast(
        resolution = pu_size,
        crs = wkt,
        extent = terra::ext(nb_proj)
      )


      r1 <- terra::rasterize(vect(nb_proj),
        r1,
        touches = TRUE,
        update = TRUE,
        background = 0
      )

      pu_sum <- global(r1, sum, na.rm = TRUE)
    }
  }

  terra::writeRaster(r1,
    glue::glue("{output_dir}/planning_units_{iso3}.tif"),
    gdal = c("COMPRESS=DEFLATE", "OVERVIEWS=NONE"),
    NAflag = 255,
    overwrite = TRUE,
    filetype = "COG",
    datatype = "INT1U"
  )

  base_v <<- nb_proj
}

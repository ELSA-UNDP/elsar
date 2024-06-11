exact_rasterize <- function(x,
                            r_ref,
                            attribute = NULL,
                            mask) {
  r_stack <- terra::rast()

  for (i in 1:nrow(x)) {
    f <- slice(x, i)
    f_r <- exactextractr::coverage_fraction(r_ref, f)[[1]]
    f_r <- f_r * pull(f, attribute) # attribute to map
    r_stack <- c(r_stack, f_r)

    gc()
  }

  rescale(terra::app(r_stack, mean, na.rm = TRUE) * mask)
}

make_threatened_ecosystems <- function(
    in_eco,
    in_intact,
    r_ref,
    mask,
    iso3 = iso3,
    output_dir = NULL) {
  # Find non-intact areas based on the median intactness value
  intact <- terra::rast(in_intact)

  intact_median <- terra::global(intact, median, na.rm = TRUE)$global

  m <- c(
    0, intact_median, 1,
    intact_median, 1, 0
  )

  rclmat <- matrix(m, ncol = 3, byrow = TRUE)

  intact <- terra::classify(intact, rclmat, include.lowest = TRUE)

  non_intact <- terra::as.polygons(intact, dissolve = TRUE) %>%
    st_as_sf() %>%
    filter(tmp == 1) %>%
    st_make_valid()

  # Ecosystem data
  eco_in <- rast(in_eco)

  eco_in <- crop(eco_in, ext(project(base, y = "epsg:4326")))

  activeCat(eco_in) <- 10 # Activate correct RAT attribute

  eco_in <- terra::as.polygons(eco_in, dissolve = TRUE, values = TRUE) %>%
    st_as_sf() %>%
    st_transform(crs = crs(base)) %>%
    st_make_valid()

  eco_threat <- st_intersection(eco_in, non_intact)

  eco_threat <- eco_threat %>%
    group_by(W_Ecosystm) %>%
    summarise() %>%
    mutate(area_intact = units::drop_units(st_area(.))) %>%
    st_set_geometry(NULL)

  eco_threat <- eco_in %>%
    group_by(W_Ecosystm) %>%
    summarise() %>%
    mutate(area = units::drop_units(st_area(.))) %>%
    left_join(eco_threat, by = "W_Ecosystm") %>%
    mutate(
      threat = area_intact / area,
      threat = ifelse(is.na(threat), 0, threat)
    )


  r <- exact_rasterize(eco_threat,
    r_ref = r_ref,
    attribute = "threat",
    mask = r_mask
  )

  terra::writeRaster(r,
    glue::glue("{output_dir}/threatened_ecosystems_{iso3}.tif"),
    gdal = c("COMPRESS=DEFLATE", "OVERVIEWS=NONE"),
    NAflag = -9999,
    overwrite = TRUE,
    filetype = "COG"
  )
}

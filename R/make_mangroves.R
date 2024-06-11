make_mangroves = function(v_in,
                          v_layer = NULL,
                          r_ref,
                          mask,
                          iso3,
                          output_dir = NULL){
  
  wkt = terra::ext(mask) %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(value = terra::crs(mask)) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_geometry() %>%
    sf::st_as_text()

  v_in = st_read(v_in,
                 layer = v_layer,
                 wkt_filter = wkt)
  
  if(nrow(v_in) > 0){
   
    v_in = v_in %>%
      st_transform(crs = st_crs(r_ref)) %>% 
      st_make_valid() %>% 
      summarise()
    
     r = rescale(exactextractr::coverage_fraction(r_ref, v_in)[[1]] * mask)
     
  } else {
    
    r = rast(r_ref)
    
  }
  
 writeRaster(r,
             glue::glue("{output_dir}/mangroves_{iso3}.tif"),
             gdal=c("COMPRESS=DEFLATE"),
             NAflag = -9999,
             overwrite=TRUE,
             filetype = "COG")
  
  gc()

}
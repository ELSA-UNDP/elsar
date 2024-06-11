make_landmark_areas = function(v_in,
                                v_layer = NULL,
                                r_ref,
                                mask,
                                iso3,
                                output_dir = NULL){
  
  v_in = st_read(v_in, layer=v_layer, query = glue::glue("SELECT * FROM {v_layer} WHERE ISO_CODE = '{iso3}'" ))
  
  if(nrow(v_in) > 0){
   
    v_in = v_in %>%
      st_transform(crs = st_crs(r_ref)) %>% 
      st_make_valid() %>% 
      summarise()
    
     r = rescale(exactextractr::coverage_fraction(r_ref, v_in)[[1]] * r_mask)
     
  } else {
    
    r = rast(r_ref)
    
  }
  
 writeRaster(r,
              glue::glue("{output_dir}/landmark_area_{iso3}.tif"),
              gdal=c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite=TRUE,
              filetype = "COG") 

}
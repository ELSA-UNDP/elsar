make_protected_areas = function(from_protected_planet = TRUE,
                                pa_def = 1,
                                include_proposed = FALSE,
                                v_in,
                                v_layer = NULL,
                                r_ref,
                                mask,
                                iso3,
                                output_dir = NULL){
  
  if(from_protected_planet == TRUE){
    
    pa = wdpa_fetch(iso3,
                    wait = TRUE, 
                    download_dir = here::here("data_orig/protected_planet_wdpa/"))
  
  } else {
    
    pa = st_read(v_in_pa, v_in_pa_layer)
  
  }
  
  if(pa_def == 1){
    
    pa = pa %>%
      filter(PA_DEF == 1)
  
  } 
  
  if(include_proposed == FALSE){
    
    pa = pa %>%
      filter(STATUS %in% c('Established','Inscribed','Designated'))
    
  }
  
  pa = pa %>% 
    filter(!stringr::str_detect(DESIG,"MAB")) # Exclude UNESCO MAB areas
  
  pa = pa %>% 
    st_transform(st_crs(base)) %>% 
    summarise() %>% 
    st_make_valid()
  
  r = exactextractr::coverage_fraction(r_ref, pa)[[1]] * mask
  
  writeRaster(r,
              glue::glue("{output_dir}/protected_areas_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
              NAflag = -9999,
              overwrite = T,
              filetype = "COG") 

}
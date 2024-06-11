
underepresented_ecosystems_extract = function(x,
                                              r_ref,
                                              mask){
  
  r_stack  = rast()
  
  for(i in 1:nrow(x)){
    f = slice(x, i)
    f_r = exactextractr::coverage_fraction(r_ref, f)[[1]]
    f_r = f_r * pull(f, target)
    r_stack = c(r_stack, f_r)
    
    gc()
  }
  
  rescale(app(r_stack, sum, na.rm = TRUE) * mask)
  
}

make_underepresented_ecosystems = function(from_protected_planet = TRUE,
                                           pa_def = 1,
                                           include_proposed = FALSE,
                                           in_eco,
                                           v_in_pa = NULL,
                                           v_in_pa_layer = NULL,
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
    st_transform(st_crs(base)) %>% 
    summarise() %>% 
    st_make_valid()
  
  # Crop and convert World Ecosytems to polygons
  eco_in = rast(in_eco)
    
  eco_in = crop(eco_in, ext(project(base, y = "epsg:4326")))
  
  activeCat(eco_in) = 10 # Activate correct RAT attribute
  
  eco_in = terra::as.polygons(eco_in, dissolve = TRUE, values = TRUE) %>% 
    st_as_sf() %>% 
    st_make_valid()
    
  eco_pa = st_intersection(eco_in, pa)
  
  eco_pa_area = eco_pa %>%
    group_by(gridcode) %>%
    summarise() %>%
    mutate(area_pa=units::drop_units(st_area(.))) %>%
    st_set_geometry(NULL) %>%
    select(gridcode, area_pa)

  eco_total = eco_in %>%
    group_by(gridcode) %>%
    summarise() %>%
    mutate(area = units::drop_units(st_area(.))) %>% 
    left_join(eco_pa_area, by = 'gridcode') %>% 
    mutate(pct_pa = area_pa/area,
           target = ifelse(pct_pa < 0.3, 0.3 - pct_pa, 0))

  r = underepresented_ecosystems_extract(eco_total)
  
  writeRaster(r,
              glue::glue("{output_dir}/underepresented_ecosystems_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG") 
  
  gc()

}
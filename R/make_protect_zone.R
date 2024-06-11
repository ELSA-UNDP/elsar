make_protect_zone = function(r_in,
                             r_ref = base,
                             from_protected_planet = TRUE,
                             pa_def = 1,
                             include_proposed = FALSE,
                             v_in = NULL,
                             v_layer = NULL,
                             mask = r_mask,
                             iso3,
                             output_dir = NULL){
  
  r_in = terra::rast(hfp)
  
  r_in = terra::crop(r_in, y = terra::ext(terra::project(mask, crs(hfp_in))))
  
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
    geos::as_geos_geometry() %>% 
    geos::geos_make_collection() %>%
    geos::geos_unary_union() %>% 
    sf::st_as_sf() %>% 
    st_make_valid()
  
  #HFP inside PAs excluding 5% highest 
  breaks = extract(r_in, vect(pa), fun = quantile, probs=0.95, na.rm=T)[,2]
  
  r = ifel(r_in < breaks, 1, 0) %>% 
    project(x = ., y = mask, method = "near") * mask
  
  crops = rast(glue::glue("{output_dir}/crops_{iso3}.tif"))
  
  built = rast(glue::glue("{output_dir}/built_areas_{iso3}.tif"))
  
  r[crops > 0 | built > 0 ] = NA
   
  # Buffer out by 2 cells
  dist =terra::distance(r)
    
  r[dist <= res(r)[1]*2 & dist > 0] = 1
  
  r[is.na(r)] = 0 
  
  r = r * mask
  
  terra::writeRaster(r,
            "tmp.tif",
            gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
            NAflag = 255,
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG")
  
  # Requires gdal sieve python command to run on system for now
  system(glue::glue("gdal_sieve.py -st 20 tmp.tif tmp2.tif"))
         
  r = rast("tmp2.tif")
  
  terra::writeRaster(r,
            "{output_dir}/protect_zone_{iso3}.tif",
            gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
            NAflag = 255,
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG") 

  file.remove(c("tmp.tif","tmp2.tif"))
  
}

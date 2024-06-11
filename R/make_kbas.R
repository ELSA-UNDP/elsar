make_kbas = function(kba_src,
                     is_postgres = FALSE,
                     kba_lyr = NULL,
                     kba_iso3_column = 'iso3',
                     incl_regional_kba = FALSE,
                     aze_only = FALSE,
                     r_ref,
                     mask,
                     iso3,
                     output_dir = NULL){
  
  if(is_postgres == TRUE) {
    require(RPostgreSQL)
    
    # PostgreSQL Connection only, if needed
    dn = dbConnect("PostgreSQL", dbname = kba_src)
    v_in = st_read(dsn = dn, query = glue::glue("SELECT * FROM {kba_lyr} WHERE {kba_iso3_column} = '{iso3}'"))
  
    } else {
      
    v_in = sf::read_sf(kba_src, kba_lyr)
  
    }
  
  if(aze_only == TRUE){
    
    v_in = v_in %>% 
      filter(azestatus == 'confirmed')
  }
  
  if(incl_regional_kba == FALSE){
    
    v_in = v_in %>%
      filter(kba_qual %ni% c('Regional','Global/ Regional to be determined'))
    
  }
  
  v_in = v_in %>% 
    st_transform(crs = crs(r_ref)) %>% 
    summarise() %>% 
    st_make_valid()
  
  r = exactextractr::coverage_fraction(r_ref, v_in)[[1]] * mask
  
  writeRaster(r,
              glue::glue("{output_dir}/kbas_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG") 
  
  gc()

}
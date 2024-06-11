make_wetlands = function(r_in,
                         r_ref,
                         mask){

  gdalUtils::align_rasters(unaligned = normalizePath(r_in),
                           reference = normalizePath(r_ref),
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  
  r = rast("tmp.tif")
  #r[is.na(r)] = 0
  #r = rescale(r * r_mask)

  #file.remove("tmp.tif")
  #gc()

}

make_ramsar_sites = function(v_in,
                             v_lyr = NULL,
                             r_ref,
                             mask){
  
  v_in = st_read(v_in, layer=v_lyr) %>%
    filter(iso3 == iso3) %>% 
    summarise()
  
  if(nrow(v_in == 0)){
    
    r = base
    r[r==1] = 0
    r = r * mask
    
    
  } else {
  
  r = exactextractr::coverage_fraction(rast(r_ref), v_in)[[1]] * mask
  
  }
  
  gc()

}

#Fix below
make_wetlands_ramsar = function(r_in,
                                v_in,
                                v_lyr,
                                mask,
                                iso3 = iso3,
                                output_dir = NULL){
  
  r1 = make_wetlands(r_in = r_in,
                     r_ref = r_ref,
                     mask = mask)
  
  r2 = make_ramsar_sites(v_in = v_in,
                         v_lyr = v_lyr,
                         r_ref = r_ref,
                         mask = mask)
  
  r = terra::rast(raster::raster(r1) | raster::raster(r2)) * mask #=calc((0.5*wetland+ramsar_cr_r),function(x) ifelse(x>1, 1 , x))*mask
  
  writeRaster(r,
              glue::glue("{output_dir}/wetlands_ramsar_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG")  

}
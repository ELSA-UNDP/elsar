make_urban_greening_zones = function(urban_in,
                                     r_ref = base,
                                     mask = r_mask,
                                     iso3,
                                     output_dir = NULL){
  
  r_in = terra::rast(urban_in)
  r_in[r_in > 0] = 1
  r_in[r_in == 0] = NA
  
  # Buffer out by 1 cell
  dist =terra::distance(r_in)
    
  r_in[dist <= terra::res(r_in)[1]*1 & dist > 0] = 1
  
  r_in[is.na(r_in)] = 0 
  
  r = r_in * mask
  
  terra::writeRaster(r,
            "tmp.tif",
            gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
            NAflag = 255,
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG")
  
  # Requires gdal sieve python command to run on system for now
  system(glue::glue("gdal_sieve.py -st 20 tmp.tif tmp2.tif"))
         
  r = terra::rast("tmp2.tif")
  
  terra::writeRaster(r,
            glue::glue("{output_dir}/urban_green_zone_{iso3}.tif"),
            gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
            NAflag = 255,
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG") 
  
 
}

make_restore_zones = function(hfp_in,
                              r_ref = base,
                              mask = r_mask,
                              iso3,
                              output_dir = NULL){
  
  hfp_in = terra::rast(hfp_in)
  
  hfp_in = terra::crop(hfp_in, y = terra::ext(terra::project(mask, terra::crs(hfp_in))))
  # Middle 80% of HFP values
  breaks = terra::global(hfp_in, quantile, probs=c(0.2,0.8), na.rm=T)
  
  r = terra::ifel(hfp_in > breaks[,1] & hfp_in < breaks[,2], 1, 0) %>%
    terra::project(x = ., y = terra::crs(mask), method = "bilinear") %>% 
    exactextractr::exact_resample(base, 'mean')
  r[r > 0] = 1
  r[r == 0] = NA
  
  forests = rast(glue::glue("{output_dir}/managed_forests_{iso3}.tif"))
  forests[forests > 0] = 1
  forests[forests == 0] = NA
  
  crops = rast(glue::glue("{output_dir}/crops_{iso3}.tif"))
  
  built = rast(glue::glue("{output_dir}/built_areas_{iso3}.tif"))
  
  r = rast(raster::raster(r) | raster::raster(forests))
  
  r[built > 0 | crops > 0] = NA 
   
  # Buffer out by 2 cells
  dist =terra::distance(r)
    
  r[dist <= terra::res(r)[1]*2 & dist > 0] = 1
  
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
         
  r = terra::rast("tmp2.tif")
  
  terra::writeRaster(r,
            glue::glue("{output_dir}/restore_zone_{iso3}.tif"),
            gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
            NAflag = 255,
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG") 
  
  # V2 - Managed forest areas only
  forests = rast(glue::glue("{output_dir}/managed_forests_{iso3}.tif"))
  forests[forests > 0] = 1
  
  terra::writeRaster(forests,
            glue::glue("{output_dir}/restore_zone_forest_{iso3}.tif"),
            gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
            NAflag = 255,
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG")

  file.remove(c("tmp.tif","tmp2.tif"))
  
}

make_managed_forests = function(forest_in,
                                r_ref = base,
                                mask = r_mask,
                                iso3,
                                output_dir = NULL){
  
  r = rast(forest_in)
  
  r = terra::crop(r, project(base, crs(r)))

  r = ifel(r %in% c(20,31,32,40,53), 1 , 0)
  
  r = project(r, crs(base))
  
  r = exactextractr::exact_resample(r, base, 'mean')
  
  r = rescale(r * mask)
  
  terra::writeRaster(r,
                     glue::glue("{output_dir}/managed_forests_{iso3}.tif"),
                     gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
                     NAflag = -9999,
                     overwrite = TRUE,
                     filetype = "COG")
  
}
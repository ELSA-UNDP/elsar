make_productive_managed_forests = function(npp_in,
                                           forest_in,
                                           r_ref = base,
                                           mask = r_mask,
                                           iso3,
                                           output_dir = NULL){
  
  npp = rast(npp_in)
  
  npp = terra::crop(npp, project(base, crs(npp)))

  forest = rast(forest_in)
  
  forest = terra::crop(forest, project(base, crs(forest)))

  forest = ifel(forest %in% c(20,31,32,40,53), 1 , 0)
  
  npp = resample(npp, forest, method = 'near')
  
  h = forest * npp
  
  h = project(h, base)
  
  h = rescale(h * mask)
  
  terra::writeRaster(h,
                     glue::glue("{output_dir}/productive_managed_forests_{iso3}.tif"),
                     gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
                     NAflag = -9999,
                     overwrite = TRUE,
                     filetype = "COG")
  
}
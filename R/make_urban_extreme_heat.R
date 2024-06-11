make_urban_extreme_heat = function(r_in,
                                   r_ref,
                                   r_urban,
                                   r_mask,
                                   iso3,
                                   output_dir = NULL){

  gdalUtils::align_rasters(unaligned = r_in,
                           reference = r_ref,
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  r_in = rast("tmp.tif")
  r_in[is.na(r_in)] = 0
  r_in = rescale(r_in * r_mask)
  
  gdalUtils::align_rasters(unaligned = r_urban,
                           reference = r_ref,
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  r_urban = rast("tmp.tif")
  r_urban[is.na(r_urban)] = 0
  r_urban = rescale(r_urban * r_mask)
  
  r_in[r_urban == 0] = 0

  writeRaster(r_in,
              glue::glue("urban_extreme_heat_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
              NAflag = -9999,
              overwrite = T,
              filetype = "COG")  
  
  file.remove("tmp.tif")
  gc()

}
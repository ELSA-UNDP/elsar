make_crop_suitability_difference = function(r_in,
                                            r_ref,
                                            mask,
                                            iso3,
                                            output_dir = NULL){
  
  gdalUtils::align_rasters(unaligned = r_in,
                           reference = r_ref,
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  
  r = rast("tmp.tif")
  r[r > 0] = 0
  r = -r
  r = rescale(r * mask)

  writeRaster(r,
              glue::glue("{output_dir}/crop_suitability_difference_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG") 
  
  file.remove("tmp.tif")
  gc()
  
}
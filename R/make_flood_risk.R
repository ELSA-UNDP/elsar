make_flood_risk = function(r_in,
                           r_ref,
                           mask = r_mask,
                           iso3 = iso3,
                           output_dir = NULL){
  
  gdalUtils::align_rasters(unaligned = r_in,
                           reference = r_ref,
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  r_in = rast("tmp.tif")
  r_in = rescale(r_in * r_mask)

  writeRaster(r_in,
              glue::glue("{output_dir}/flood_risk_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG") 
  
  file.remove("tmp.tif")
  gc()
  
}
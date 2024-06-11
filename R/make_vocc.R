make_vocc = function(r_in, r_ref, r_mask, iso3){
  
  gdalUtils::align_rasters(unaligned = r_in,
                           reference = r_ref,
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  r_in = rast("tmp.tif")
  r_in = rescale(r_in * r_mask)

  writeRaster(r_in,
              glue::glue("vocc_{iso3}.tif"),
              options = c("compress=deflate","tiled=yes"),
              NAflag = -9999,
              overwrite = T)  
  
  file.remove("tmp.tif")
  gc()
  
}
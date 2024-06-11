make_flii = function(r_in, r_ref, r_mask, iso3){
  
  gdalUtils::align_rasters(unaligned = r_in,
                           reference = r_ref,
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  r_in = rast("tmp.tif")
  r_in[r_in < 0] = 0
  r_in[is.na(r_in)] = 0
  r_in = rescale(r_in * r_mask)

  writeRaster(r_in,
              glue::glue("flii_{iso3}.tif"),
              options = c("compress=deflate","tiled=yes"),
              NAflag = -9999,
              overwrite = T)  
  
  file.remove("tmp.tif")
  gc()
  
}
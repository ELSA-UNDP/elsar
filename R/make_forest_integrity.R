make_forest_integrity = function(fsii,
                                 flii,
                                 r_ref,
                                 mask,
                                 iso3,
                                 output_dir = NULL){
  
  gdalUtils::align_rasters(unaligned = normalizePath(fsii),
                           reference = normalizePath(r_ref),
                           dstfile = "tmp_fsii.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)
  
  gdalUtils::align_rasters(unaligned = normalizePath(flii),
                           reference = normalizePath(r_ref),
                           dstfile = "tmp_flii.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)
  
  fsii = rast("tmp_fsii.tif")
  
  flii = rast("tmp_flii.tif")
  
  fi = terra::mean(fsii + flii)
  
  fi[is.na(fi)] = 0
  
  fi = rescale(fi*mask)

  terra::writeRaster(fi,
              glue::glue("{output_dir}/forest_integrity_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG") 
  
  file.remove("tmp_fsii.tif","tmp_flii.tif")
  gc()
  
}
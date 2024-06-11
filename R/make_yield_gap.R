make_yield_gap <- function(r_in,
                           r_ref,
                           mask,
                           iso3,
                           output_dir = NULL){

  gdalUtils::align_rasters(unaligned = normalizePath(r_in),
                           reference = normalizePath(r_ref),
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  
  r = rast("tmp.tif")
  r = 100 - r
  r[is.na(r)] = 0
  r = rescale(r * mask)

  writeRaster(r,
              glue::glue("{output_dir}/yield_gap_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG")  
  
  file.remove("tmp.tif")
  gc()

}
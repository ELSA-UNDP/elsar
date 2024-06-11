make_threatened_species_richness <- function(r_in,
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
  r_in = rast("tmp.tif")
  r_in[is.na(r_in)] = 0
  r_in = rescale(r_in * mask)

  writeRaster(r_in,
              glue::glue("{output_dir}/threatened_species_richness_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG")  
  
  file.remove("tmp.tif")
  gc()

  }
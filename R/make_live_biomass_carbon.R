make_live_biomass_carbon = function(r_in,
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
  
  r_in = rast("tmp.tif")
  r_in[r_in < 0] = 0
  r_in[is.na(r_in)] = 0
  r_in = rescale(r_in * mask)

  writeRaster(r_in,
              glue::glue("{output_dir}/live_biomass_carbon_{iso3}.tif"),
              gdal = c("COMPRESS=DEFLATE"),
              NAflag = -9999,
              overwrite = TRUE,
              filetype = "COG")  
  
  file.remove("tmp.tif")
  gc()
  
}
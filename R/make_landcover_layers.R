make_landcover_layers = function(mask,
                                 iso3,
                                 output_dir = NULL){
  
  r <- terra::rast("/media/scottca/Seagate Backup Plus Drive/work/undp_GIS/lc2021/lc2021.vrt")

  r_crop = terra::crop(r, terra::project(mask, terra::crs(r)))
  r_proj = r_crop %>%
    terra::project(crs(mask), res = 10, method = 'near')

  crops = terra::ifel(r_proj == 5, 1, 0)
  crops = exact_resample(crops, mask, 'mean') * mask

  built = terra::ifel(r_proj == 7, 1, 0)
  built = exact_resample(built, mask, 'mean') * mask
  
  terra::writeRaster(crops,
                     glue::glue("{output_dir}/crops_{iso3}.tif"),
                     gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
                     NAflag = -9999,
                     overwrite = T,
                     filetype = "COG")
  
  terra::writeRaster(built,
                     glue::glue("{output_dir}/built_areas_{iso3}.tif"),
                     gdal = c("COMPRESS=DEFLATE","OVERVIEWS=NONE"),
                     NAflag = -9999,
                     overwrite = T,
                     filetype = "COG")
  
}
# r_mtn_in = "data_orig/GlobalMountainsK3Binary/k3binary.tif"
# v_glc_in = "data_orig/glacier/rgi60_clean.gpkg"
# v_base = base_v
# iso3 = "CRI"


make_mountains_glaciers = function(r_mtn_in=NULL,
                                   v_glc_in=NULL,
                                   v_glc_layer=NULL,
                                   r_ref=planning_unit_layer,
                                   mask=r_mask,
                                   v_base=NULL,
                                   iso3=NULL){
  
  gdalUtils::align_rasters(unaligned = normalizePath(r_mtn_in),
                           reference = normalizePath(planning_unit_layer),
                           dstfile = "tmp.tif",
                           nThreads = 4,
                           r = 'average',
                           overwrite = TRUE)  
  r_mtn = rast("tmp.tif")
  r_mtn[r_mtn < 0] = 0
  r_mtn[is.na(r_mtn)] = 0
  r_mtn = rescale(r_mtn * mask) * 0.5

  if(!is.null(v_glc_in)){
    
    filter = v_base %>%
      st_transform(crs = st_crs(4326)) %>%
      st_as_sfc(st_bbox()) %>%
      st_as_text()
    
    v_in = read_sf(normalizePath(v_glc_in), wkt_filter = filter)
      
    if(nrow(v_in) > 0) {
      
      v_in = v_in %>%
        st_transform(crs=st_crs(mask)) %>% 
        summarise() %>% 
        st_make_valid()
      
      r_glc = exactextractr::coverage_fraction(r_ref, v_in)[[1]] * mask * 0.5
      
      r_glc = rescale(r_glc * mask)
    
      r_mtn_glc = r_mtn + r_glc * 0.5
  
      r_mtn_glc = rescale(r_mtn_glc * mask)
    
    } else {
      
      r_mnt_glc = rescale(r_mtn)
      
    }
  }
  
  terra::writeRaster(r_mnt_glc,
              glue::glue("mountains_glaciers_{iso3}.tif"),
              gdal=c("compress=deflate","tiled=yes"),
              NAflag = -9999,
              overwrite=T,
              datatype = 'FLT4S')

  file.remove("tmp.tif")
  
  gc()
  
  }
  
# Save a Raster as a Cloud Optimized GeoTIFF (COG)

This helper function saves a `SpatRaster` to disk as a Cloud Optimized
GeoTIFF (COG) with ZSTD compression. It automatically sets the
appropriate GDAL predictor based on the data type to optimize file size
and performance. For floating point rasters (`FLT4S`), Predictor 3 is
used, and for integer rasters, Predictor 2 is used, which improves
compressibility. A nodata value is also automatically defined and
applied: 255 for integer rasters (e.g. `"INT1U"`) and `NaN` for floating
point rasters.

## Usage

``` r
save_raster(raster, filename, datatype = "FLT4S")
```

## Arguments

- raster:

  A `SpatRaster` object to be saved.

- filename:

  Character. Full file path (including `.tif` extension) where the
  raster will be saved.

- datatype:

  Character. GDAL data type to use for saving the raster (e.g. `"FLT4S"`
  for float, `"INT1U"` for unsigned byte). Default is `"FLT4S"`.

## Value

None. The function is called for its side effect of saving the raster to
disk.

## Details

This function should be used across processing functions to ensure
consistent raster output formats.

## Examples

``` r
if (FALSE) { # \dontrun{
save_raster(my_raster, "output/my_raster.tif", datatype = "INT1U")
save_raster(my_raster, "output/my_raster_float.tif", datatype = "FLT4S")
} # }
```

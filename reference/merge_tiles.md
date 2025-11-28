# Merge downloaded GEE tiles into a single Cloud-Optimized GeoTIFF

Takes multiple downloaded GeoTIFF tiles and merges them into a single
Cloud-Optimized GeoTIFF (COG) with appropriate compression and
optimization.

## Usage

``` r
merge_tiles(local_path, output_file, datatype)
```

## Arguments

- local_path:

  Character. Path to directory containing downloaded tile files

- output_file:

  Character. Full path for the output merged COG file

- datatype:

  Character. GDAL datatype for output raster (e.g., "INT1U", "FLT4S")

## Value

A `SpatRaster` object of the merged and written COG

## Examples

``` r
if (FALSE) { # \dontrun{
merged_raster <- merge_tiles("/tmp/tiles", "/output/merged.tif", "INT1U")
} # }
```

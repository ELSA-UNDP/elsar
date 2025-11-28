# Infer file type from file path or directory

This utility function takes a path (to a file or folder) and returns the
detected geospatial file type, such as `"shp"`, `"gpkg"`, `"tif"`, etc.
It checks extensions for files and scans directory contents for
recognizable formats (e.g., `.shp`, `.gdb`).

## Usage

``` r
get_file_type(path)
```

## Arguments

- path:

  Character. A file path or directory path to inspect.

## Value

A character string indicating the file type (e.g., `"shp"`, `"gpkg"`,
`"tif"`).

## Examples

``` r
get_file_type("data/boundaries.shp")     # returns "shp"
#> [1] "shp"
get_file_type("data/vector_layers.gpkg") # returns "gpkg"
#> [1] "gpkg"
get_file_type("data/rasters")            # scans folder for known formats
#> Error in get_file_type("data/rasters"): Unsupported file extension: 
```

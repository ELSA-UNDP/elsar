# Extract binary layers within a raster stack

This function extracts all layers in a raster stack that have either
c(0,1) or only 1 or only 0. This is needed to split the raster stack
saved from the pipeline into two raster stacks, one int and one float.

## Usage

``` r
get_binary_layers(raster_stack)
```

## Arguments

- raster_stack:

  A `terra` `SpatRaster` containing several raster layers, some of which
  can be binary.

## Value

Two `terra` `SpatRaster` raster stacks, one containing only the binary
layers, and one containing the float layers

## Examples

``` r
if (FALSE) { # \dontrun{
raster_out <- get_binary_layers(raster_stack = stack)

int_stack <- raster_out[[1]]
float_stack <- raster_out[[2]]
} # }
```

# Rescale Raster to 0-1 Range

This function rescales values in a raster to a range between 0 and 1.

## Usage

``` r
rescale_raster(
  raster_in,
  raster_in_min = terra::global(raster_in, min, na.rm = TRUE)$min,
  raster_in_max = terra::global(raster_in, max, na.rm = TRUE)$max,
  new_min = 0,
  new_max = 1
)
```

## Arguments

- raster_in:

  Input `SpatRaster` to be rescaled.

- raster_in_min:

  Optional numeric. Minimum value of input raster. If NULL, will be
  calculated.

- raster_in_max:

  Optional numeric. Maximum value of input raster. If NULL, will be
  calculated.

- new_min:

  Numeric. New minimum of rescaled raster (default = 0).

- new_max:

  Numeric. New maximum of rescaled raster (default = 1).

## Value

A `SpatRaster` with values rescaled to the specified range.

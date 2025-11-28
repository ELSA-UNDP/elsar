# Crop a Global Raster to the Extent of Planning Units

This function crops a large global raster to the spatial extent of a
planning units raster (`pus`). It reprojects the `pus` extent to the
coordinate reference system of the input global raster to ensure
accurate cropping. This is useful for pre-processing global inputs
before using functions like
[`make_normalised_raster()`](https://elsa-undp.github.io/elsar/reference/make_normalised_raster.md)
to reduce processing time.

## Usage

``` r
crop_global_raster(raster_in, pus, threads = TRUE)
```

## Arguments

- raster_in:

  SpatRaster. A large input raster (e.g., global dataset).

- pus:

  SpatRaster. Planning units raster used to define the target extent.

- threads:

  Optional method to use multi-core processing - to speed on some
  `terra` functions (default: `TRUE`).

## Value

A cropped SpatRaster with the same CRS as `raster_in` and extent
matching the reprojected `pus`.

## Examples

``` r
if (FALSE) { # \dontrun{
cropped <- crop_global_raster(global_raster, pus) |>
  elsar::make_normalised_raster(pus = pus, iso3 = "KEN")
} # }
```

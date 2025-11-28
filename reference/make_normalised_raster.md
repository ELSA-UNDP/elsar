# Create a Normalised Raster Aligned with and masked by Planning Units

This function takes an input raster, aligns it with specified planning
units (PUs), optionally inverts, rescales, applies conditional
expressions, and saves the processed raster to a specified output path.

## Usage

``` r
make_normalised_raster(
  raster_in,
  pus,
  iso3,
  invert = FALSE,
  rescaled = TRUE,
  method_override = NULL,
  crop_global_input = TRUE,
  input_raster_conditional_expression = NULL,
  conditional_expression = NULL,
  fill_na = 0,
  name_out,
  output_path = NULL,
  threads = TRUE
)
```

## Arguments

- raster_in:

  `SpatRaster` The input raster to be processed.

- pus:

  `SpatVector` The planning units (PUs) to align the raster to.

- iso3:

  `character` ISO3 country code used for naming the output file.

- invert:

  `logical` If `TRUE`, inverts the raster values (default: `FALSE`).

- rescaled:

  `logical` If `TRUE`, rescales the raster using
  [`rescale_raster()`](https://elsa-undp.github.io/elsar/reference/rescale_raster.md)
  (default: `TRUE`).

- method_override:

  `character` Optional method for
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html),
  overriding the default (default: `NULL`).

- crop_global_input:

  `logical` If true the input (large global) raster is cropped to the PU
  extent before applying an `input_raster_conditional_expression`, to
  reduce the are of processing (default: `TRUE`).

- input_raster_conditional_expression:

  `function` Optional method to apply a function to the raster before
  resampling to the PU layer (default: `NULL`).

- conditional_expression:

  `function` Optional method to apply a function to the raster after
  resampling to the PU layer (default: `NULL`).

- fill_na:

  `numeric` or `NA` The fill value to use to fill in `NA` values before
  masking (default: 0).

- name_out:

  `character` The name of the output raster file (without the
  extension).

- output_path:

  `character` The directory path to save the output raster (default:
  `NULL`, i.e., not saved).

- threads:

  Optional method to use multi-core processing - to speed on some
  `terra` functions (default: `TRUE`).

## Value

Returns a `SpatRaster` object that has been reprojected and processed.
If `output_path` is specified, saves the raster as a COG (Cloud
Optimized GeoTIFF).

## Details

This function reprojects the input raster (`raster_in`) to match the CRS
and resolution of the planning units (`pus`). The method for
reprojection can be overridden using `method_override`. If
`input_raster_conditional_expression` is provided, it is applied before
any reprojection. Applying a `input_raster_conditional_expression` can
also make processing times significantly longer for high resolution
input rasters.The function can optionally rescale (0-1) and invert the
raster values.

## Examples

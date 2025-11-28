# Generate Protect Zone Raster for ELSA Analysis

This function creates a `SpatRaster` layer representing planning units
eligible for protect actions in the ELSA framework. Areas with high
human footprint, agriculture, or built-up land cover are excluded.
Optionally, the result can be inverted to serve as a locked-out
constraint for spatial prioritization tools such as `prioritizr`.

## Usage

``` r
make_protect_zone(
  iso3,
  pus,
  current_protected_areas,
  agricultural_areas_input = NULL,
  built_areas_input = NULL,
  lulc_raster = NULL,
  hii_input,
  hii_threshold = NULL,
  hii_quantile = 0.95,
  agriculture_lulc_value = 5,
  built_area_lulc_value = 7,
  agriculture_threshold = 0.1,
  built_areas_threshold = 0.1,
  filter_patch_size = TRUE,
  min_patch_size = 20,
  make_locked_out = FALSE,
  output_path = NULL
)
```

## Arguments

- iso3:

  ISO3 country code (e.g., "NPL").

- pus:

  A `SpatRaster` defining the planning units.

- current_protected_areas:

  a `sf` or `SpatVector` object of current protected areas. If NULL,
  [`elsar::make_protected_areas()`](https://elsa-undp.github.io/elsar/reference/make_protected_areas.md)
  will be used.

- agricultural_areas_input:

  A `SpatRaster` representing binary or probabilistic agricultural areas
  (optional).

- built_areas_input:

  A `SpatRaster` representing binary or probabilistic built-up areas
  (optional).

- lulc_raster:

  A `SpatRaster` LULC map used to extract agriculture/built-up areas if
  not already provided (default: NULL).

- hii_input:

  A `SpatRaster` of the Human Footprint Index.

- hii_threshold:

  A fixed numeric HII threshold. If NULL, `hii_quantile` is used to
  estimate it.

- hii_quantile:

  A quantile threshold (e.g., 0.95) used to calculate the HII threshold
  within protected areas if `hii_threshold` is NULL.

- agriculture_lulc_value:

  LULC value representing agriculture (default: 4).

- built_area_lulc_value:

  LULC value representing built-up areas (default: 7).

- agriculture_threshold:

  Minimum fraction for agriculture to exclude a cell (default: 0.1).

- built_areas_threshold:

  Minimum fraction for built-up area to exclude a cell (default: 0.1).

- filter_patch_size:

  Logical. Whether to remove small isolated patches (default: TRUE).

- min_patch_size:

  Integer. Minimum patch size to retain (in raster cells; default: 20).

- make_locked_out:

  Logical. If TRUE, invert the raster to produce a locked-out constraint
  (default: FALSE).

- output_path:

  Optional directory to write the result as a COG.

## Value

A `SpatRaster` with values 1 (eligible) and 0 (excluded), or the inverse
if `make_locked_out = TRUE`.

## Details

The Human Footprint Index (HII) threshold can be defined either as a
fixed value (`hii_threshold`) or estimated from the upper 95% quantile
(`hii_quantile`) of values within protected areas.

## Examples

``` r
if (FALSE) { # \dontrun{
protect_zone <- make_protect_zone(
  pus = planning_units,
  iso3 = "NPL",
  hii_input = hii_raster,
  agricultural_areas_input = crop_raster,
  built_areas_input = built_raster,
  lulc_raster = NULL,
  hii_quantile = 0.95,
  output_path = "outputs/"
)
} # }
```

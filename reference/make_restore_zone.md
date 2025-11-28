# Generate a Degraded Areas Layer for Restoration Planning

This function identifies degraded areas for potential restoration based
on a combination of:

- **SDG productivity degradation data**

- **Agricultural and built-up areas** (either provided or derived from a
  LULC raster)

- **Human Industrial Footprint Index (HII)**

- **IUCN GET forest ecosystems coverage** (used to create
  `restore_zone_v1` and `restore_zone_v2`)

## Usage

``` r
make_restore_zone(
  iso3,
  pus,
  sdg_degradation_input = NULL,
  agricultural_areas_input = NULL,
  built_areas_input = NULL,
  lulc_raster = NULL,
  hii_input = NULL,
  iucn_get_forest_input = NULL,
  sdg_threshold = 0.1,
  lulc_threshold = 0.1,
  hii_threshold = 4,
  iucn_get_forest_threshold = 0.1,
  agriculture_lulc_value = 4,
  built_area_lulc_value = 7,
  filter_patch_size = TRUE,
  min_patch_size = 10,
  output_path = NULL
)
```

## Arguments

- iso3:

  Character. ISO3 country code (e.g., "CHL").

- pus:

  SpatRaster. Planning units raster used to define resolution and
  extent.

- sdg_degradation_input:

  SpatRaster. SDG degradation raster input.

- agricultural_areas_input:

  SpatRaster or NULL. Optional input raster for agricultural areas. If
  NULL, `lulc_raster` must be provided.

- built_areas_input:

  SpatRaster or NULL. Optional input raster for built-up areas. If NULL,
  `lulc_raster` must be provided.

- lulc_raster:

  SpatRaster or NULL. LULC raster used to derive agriculture/built areas
  if not directly provided. Assumes using the ESRI 10m LULC dataset.

- hii_input:

  SpatRaster. Human Industrial Footprint Index (HII) raster.

- iucn_get_forest_input:

  SpatRaster. IUCN GET forest raster used to create `restore_zone_v2`.

- sdg_threshold:

  Numeric. Threshold for SDG degradation to classify as degraded
  (default: 0.1).

- lulc_threshold:

  Numeric. Threshold for agri/built classification (default: 0.1).

- hii_threshold:

  Numeric. HII threshold for defining high human pressure (default: 4).

- iucn_get_forest_threshold:

  Numeric. Minimum forest cover value to retain in restore zone 2
  (default: 0.1).

- agriculture_lulc_value:

  Integer. LULC value representing agriculture if derived from
  `lulc_raster` (default: 4).

- built_area_lulc_value:

  Integer. LULC value representing built-up areas if derived from
  `lulc_raster` (default: 7).

- filter_patch_size:

  Logical. Whether to remove small isolated patches (default: TRUE).

- min_patch_size:

  Integer. Minimum number of connected pixels to retain (default: 10).

- output_path:

  Character or NULL. Directory to save output rasters. If NULL, outputs
  are returned but not saved (default: NULL).

## Value

A `SpatRaster` with two layers:

- `restore_zone_v1`: Degraded areas based on SDG, LULC, and HII
  thresholds

- `restore_zone_v2`: `v1` masked by IUCN forest coverage

## Details

The function applies user-defined thresholds to generate two outputs:

- `restore_zone_v1`: Based on SDG degradation, agriculture, built-up,
  and HII thresholds

- `restore_zone_v2`: Same as `v1` but masked by IUCN GET forest extent

If `output_path` is provided, the intermediate layers and final output
are saved as Cloud Optimized GeoTIFFs.

## Examples

``` r
if (FALSE) { # \dontrun{
restore_zone <- make_restore_zone(
  iso3 = "CHL",
  pus = planning_units,
  sdg_degradation_input = sdg_raster,
  agricultural_areas_input = NULL,
  built_areas_input = NULL,
  lulc_raster = lulc_input,
  hii_input = hii_raster,
  iucn_get_forest_input = forest_raster,
  output_path = "outputs/"
)

restore_zone <- make_restore_zone(
  iso3 = "CHL",
  pus = planning_units,
  sdg_degradation_input = sdg_raster,
  agricultural_areas_input = ag_areas_input,
  built_areas_input = built_areas_input,
  lulc_raster = NULL,
  hii_input = hii_raster,
  iucn_get_forest_input = forest_raster,
  output_path = "outputs/"
)
} # }
```

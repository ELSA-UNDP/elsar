# Generate Sustainable Management Zones for ELSA Analysis

This function identifies planning units eligible for the "Manage" zone
under the ELSA framework. The zone (default) includes areas with a
moderate level of human influence (HII between 20th and 80th
percentiles), all managed forest types, and agricultural and pastureland
areas, but explicitly excludes built-up areas. An alternative zone
includes is limited to all agricultural areas and pasturelands.

## Usage

``` r
make_manage_zone(
  iso3,
  pus,
  managed_forests_input,
  raster_mf = NULL,
  agricultural_areas_input = NULL,
  pasturelands_input = NULL,
  built_areas_input = NULL,
  lulc_raster = NULL,
  hii_input = NULL,
  pasturelands_threshold = 0.35,
  agriculture_lulc_value = 5,
  built_area_lulc_value = 7,
  forest_classes = c(20, 31, 32, 40, 53),
  forest_class_threshold = 0.1,
  agriculture_threshold = 0.1,
  built_areas_threshold = 0.1,
  filter_patch_size = TRUE,
  min_patch_size = 10,
  output_path = NULL
)
```

## Arguments

- iso3:

  Character. ISO3 country code (e.g., "CHL").

- pus:

  SpatRaster. Planning units raster to which all inputs are aligned.

- managed_forests_input:

  SpatRaster. A preprocssed raster of managed forest extent.

- raster_mf:

  SpatRaster A raster of forest classes to identify managed forests.

- agricultural_areas_input:

  SpatRaster. A binary or categorical raster representing agricultural
  areas.

- pasturelands_input:

  SpatRaster. A binary or categorical raster representing (actively
  managed/improved)pastures.

- built_areas_input:

  SpatRaster. A binary or categorical raster representing built-up/urban
  areas.

- lulc_raster:

  SpatRaster or NULL. Optional raw LULC input used to extract
  agriculture and built-up layers if those inputs are categorical.

- hii_input:

  SpatRaster. Human Footprint Index raster.

- pasturelands_threshold:

  Numeric. Probability threshold above which cells are considered
  pasturelands (default = 0.35).

- agriculture_lulc_value:

  Integer. LULC value for agriculture (default = 4).

- built_area_lulc_value:

  Integer. LULC value for built-up areas (default = 5).

- forest_classes:

  Integer vector. LULC values representing managed forests (default =
  c(20, 31, 32, 40, 53)).

- forest_class_threshold:

  Numeric. Minimum proportion of managed forest in a cell to include
  (default = 0.1).

- agriculture_threshold:

  Numeric. Minimum proportion of agriculture in a cell to include
  (default = 0.1).

- built_areas_threshold:

  Numeric. Threshold above which cells are considered built-up and
  excluded (default = 0.1).

- filter_patch_size:

  Logical. Whether to remove small patches (default = TRUE).

- min_patch_size:

  Integer. Minimum size of patches to retain (in cells; default = 10).

- output_path:

  Character or NULL. If provided, save result to this path as a COG
  (default = NULL).

## Value

A SpatRaster with two layers: `manage_zone_v1` and `manage_zone_v2`.

## Details

Two layers are returned:

- `manage_zone_v1`: Based on HII quantile range, agriculture, and
  managed forests (excluding built-up areas)

- `manage_zone_v2`: Simplified zone based on agriculture only

If `output_path` is provided, both layers are saved as a (multiband)
Cloud Optimized GeoTIFF (COG).

## Examples

``` r
if (FALSE) { # \dontrun{
manage_zone <- make_manage_zone(
  iso3 = "CHL",
  pus = planning_units,
  managed_forests_input = forest_raster,
  agricultural_areas_input = agri_raster,
  pasturelands_input = pastures_raster,
  built_areas_input = built_raster,
  lulc_raster = landcover_raster,
  hii_input = hfp_raster,
  output_path = "outputs/"
)
} # }
```

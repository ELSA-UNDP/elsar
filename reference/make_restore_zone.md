# Generate Restore Zones for Spatial Planning

Identifies potential restoration areas by combining degradation
indicators while excluding areas unsuitable for restoration (e.g.,
active farmland, urban areas).

## Usage

``` r
make_restore_zone(
  iso3,
  pus,
  degradation,
  human_pressure,
  forest_mask,
  agricultural_areas = NULL,
  built_areas = NULL,
  lulc = NULL,
  degradation_threshold = 0.1,
  human_pressure_threshold = 4,
  agriculture_threshold = 0.1,
  built_area_threshold = 0.1,
  forest_threshold = 0.1,
  lulc_agriculture_class = 4,
  lulc_built_class = 7,
  filter_small_patches = TRUE,
  min_patch_size = 10,
  output_path = NULL
)
```

## Arguments

- iso3:

  Character. ISO3 country code (e.g., "CHL").

- pus:

  SpatRaster. Planning units raster defining the output resolution and
  extent.

- degradation:

  SpatRaster. Raster indicating land degradation. Values of -1 are
  treated as degraded (e.g., SDG 15.3.1 productivity degradation layer).

- human_pressure:

  SpatRaster. Raster of human pressure/impact values (e.g., Human
  Footprint Index). Higher values indicate greater pressure.

- forest_mask:

  SpatRaster. Raster indicating forest ecosystem extent, used to create
  the forest-only restore zone (v2). Values represent forest cover
  proportion.

- agricultural_areas:

  SpatRaster or NULL. Pre-computed raster of agricultural area
  proportions (0-1). If NULL, derived from `lulc`.

- built_areas:

  SpatRaster or NULL. Pre-computed raster of built-up/urban area
  proportions (0-1). If NULL, derived from `lulc`.

- lulc:

  SpatRaster or NULL. Land use/land cover raster used to derive
  agriculture and built-up areas when not provided directly.

- degradation_threshold:

  Numeric. Proportion threshold above which an area is considered
  degraded (default: 0.1).

- human_pressure_threshold:

  Numeric. Human pressure value at or above which an area is considered
  degraded (default: 4).

- agriculture_threshold:

  Numeric. Proportion threshold above which an area is excluded as
  agricultural land (default: 0.1).

- built_area_threshold:

  Numeric. Proportion threshold above which an area is excluded as
  built-up land (default: 0.1).

- forest_threshold:

  Numeric. Minimum forest cover proportion to include in restore_zone_v2
  (default: 0.1).

- lulc_agriculture_class:

  Integer. Class value in `lulc` representing agricultural land
  (default: 4, for ESRI 10m LULC).

- lulc_built_class:

  Integer. Class value in `lulc` representing built-up areas (default:
  7, for ESRI 10m LULC).

- filter_small_patches:

  Logical. Whether to remove small isolated patches from the output
  (default: TRUE).

- min_patch_size:

  Integer. Minimum number of connected pixels to retain when filtering
  patches (default: 10).

- output_path:

  Character or NULL. Directory to save output rasters as Cloud Optimized
  GeoTIFFs. If NULL, outputs are returned but not saved.

## Value

A SpatRaster with two layers:

- restore_zone_v1:

  Degraded areas excluding agriculture and built-up land

- restore_zone_v2:

  restore_zone_v1 masked to forest ecosystems only

## Details

The function generates two restore zone outputs:

- `restore_zone_v1`: All degraded areas excluding agriculture and
  built-up land

- `restore_zone_v2`: Same as v1, but further masked to forest ecosystems
  only

Areas are classified as degraded based on either:

- Land degradation indicators (e.g., productivity decline)

- Human pressure exceeding a threshold

Agricultural and built-up areas can be provided directly as rasters, or
derived from a land use/land cover (LULC) raster by specifying class
values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using LULC raster to derive agriculture/built areas
restore_zones <- make_restore_zone(
  iso3 = "CHL",
  pus = planning_units,
  degradation = degradation_raster,
  human_pressure = hfi_raster,
  forest_mask = forest_raster,
  lulc = lulc_raster,
  output_path = "outputs/"
)

# Using pre-computed agriculture/built area rasters
restore_zones <- make_restore_zone(
  iso3 = "CHL",
  pus = planning_units,
  degradation = degradation_raster,
  human_pressure = hfi_raster,
  forest_mask = forest_raster,
  agricultural_areas = ag_raster,
  built_areas = urban_raster,
  output_path = "outputs/"
)
} # }
```

# elsar 0.3.1

## Bug Fixes

* `exact_rasterise()` no longer exhausts the OS open-file limit or crawls on
  vector layers with very many features. Previously it built one
  coverage-fraction raster per feature and stacked them, which is
  `O(nfeatures)` in both open file handles and time - wall-to-wall class maps
  with tens of thousands of polygons failed with "cannot open N files". A new
  `max_exact_features` argument (default 1000) switches large layers with a
  standard `fun` (`mean`/`sum`/`max`/`min`) to a single `terra::rasterize()`
  pass; the exact per-feature coverage path is retained for smaller layers. A
  large layer with a non-standard `fun` now warns before falling back.

# elsar 0.3.0

## Deprecations

* `make_custom_projection()` is renamed to `make_custom_mollweide_projection()`
  (the projection has always been Mollweide). The old name still works as a
  deprecated alias that warns.

## New Features

* Regional / sub-national planning runs: `iso3` may carry a suffix (e.g.
  `"ECU_REG"`, `"ECU-GEF8"`). `is_valid_iso3()` and the functions that validate
  it now accept these custom regional codes, which rely on a supplied boundary
  rather than an ISO lookup.

* `make_boundary()` and `make_custom_mollweide_projection()` support whole-file
  regional boundaries: use a custom boundary with no ISO3 column, dissolving
  multiple features into a single planning region.

* `save_raster()` now builds overviews (`OVERVIEWS=AUTO`) so outputs are fully
  valid Cloud Optimized GeoTIFFs, and gains an overview-resampling override
  (`resampling=`, e.g. `"mode"` for categorical layers).

* `make_protected_areas()` tolerates custom local protected-area layers that
  lack the WDPCA schema (STATUS / SITE_TYPE / MAB filters apply only when those
  columns exist).

## Bug Fixes

* `make_custom_mollweide_projection()` computes the projection centre in WGS84,
  fixing a wrong central meridian when the input boundary is in a projected CRS
  (e.g. UTM); it also tolerates a `NULL` iso3.

* CRS-aware spatial filter: `elsar_load_data()` / `filter_sf()` reproject the
  `wkt_filter` geometry into each layer's own CRS before filtering, so a filter
  built in one CRS no longer returns zero features against a layer stored in
  another (e.g. a Mollweide gpkg vs planning units on a different central
  meridian).

* `make_normalised_raster()` masks unflagged NoData sentinels (e.g.
  `-2147483648`, `-2147483647`) that would otherwise dominate the 0-1 rescale
  and flatten real values to ~1; it warns when it does.

# elsar 0.2.0

## Breaking Changes

* Renamed `from_wdpa` parameter to `from_wdpca` in `make_protected_areas()` to
  reflect the rename of the World Database on Protected Areas (WDPA) to the
  World Database on Protected and Conserved Areas (WDPCA).

## New Features

* Added file naming conventions vignette documenting standardised naming
  patterns for ELSA pipeline inputs, outputs, and figures.

* Added dark mode toggle to the pkgdown website.

* Added `make_urban_greening_zone()` to the pkgdown reference index.

* `make_protect_zone()` now adapts the HII threshold when mean HII inside
  protected areas is high (>= 4). Uses the PA mean as the threshold instead
  of the quantile-based approach to avoid excluding nearly all areas in
  high-HFI countries (#79). `hii_input` is now optional.

## Improvements

* Updated Global Data Processing Reference vignette:
  - Fixed stale and incorrect function names.
  - Added missing datasets (degraded areas, flood abatement, indigenous managed
    lands).
  - Added "Since" column tracking which version introduced each function.
  - Documented removed/deprecated datasets in a dedicated section.

* Added version tracking column to the dataset reference table.

## Internal Changes

* Bumped version to 0.1.0 to reflect API stability.

---

# elsar 0.0.7

## New Features

* Added Land Use/Land Cover (LULC) download functions for multiple data products:

  - `download_lulc_data()` - unified interface for LULC data
  - `download_dynamic_world_data()` - Google Dynamic World
  - `download_esa_worldcover_data()` - ESA WorldCover
  - `download_esri_lulc_data()` - ESRI Land Cover
  - `download_lulc_class_proportion()` and `download_lulc_proportions()` - class proportion calculations

* Added GEE/conda helper functions for improved Google Earth Engine integration:
  - `create_gee_conda_env()` - automated conda environment setup

  - `find_conda_base()` and `find_env_python()` - conda path utilities
  - `get_crs_wkt()` - CRS WKT string extraction

* Added multi-LULC product support with GEE-side aggregation

* Exported `check_and_download_required_layers()` for checking and downloading required data layers

## Bug Fixes

* Fixed "missing value where TRUE/FALSE needed" error in `make_urban_greening_opportunities()` when input data contains NA values in filter columns

* Fixed "missing value where TRUE/FALSE needed" error in `make_normalised_raster()` when checking for empty raster values

* Fixed "[subset] no (valid) layer selected" error in `get_binary_layers()` when raster stack has zero layers or when subsetting returns empty indices

* Fixed "Python int too large to convert to C long" error on Windows when accessing GEE task status (timestamps exceed 32-bit integer range)

* Fixed GEE multi-tile export bug where only some tiles were downloaded for large areas (e.g., Chile pasturelands). Now waits for all tiles to appear before downloading

* Fixed curve geometry handling in `make_ramsar_sites()` for improved gpkg support

* Fixed KBA geometry handling issues

* Fixed extent alignment bug in zone functions (`make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()`)

* Fixed operator precedence bug in `make_threatened_ecosystems_restoration()`

* Fixed syntax error in `make_restore_zone()`

## Improvements

* Updated `make_protected_areas()` for WDPCA schema changes

* Standardized logging and input validation across all functions

* Improved package documentation and README

* Sanitized filenames for saved plots

* Added logging for HII threshold values

* Refactored restore zone creation to allow single version output

## Deprecations

* Deprecated `elsar_plot_static_raster_c()` and `elsar_plot_static_raster_d()` in favor of unified plotting API

* Deprecated `get_iucn_ecosystems()` and `get_iucn_forests()` functions

* Deprecated `pa_def` parameter in `make_protected_areas()` - use `status_filter` instead

## Internal Changes

* Removed unused `make_elsa_problem.R`

* Consolidated utility files (`utils-elsa_plotting.R`, `utils-elsa_posthoc.R`, `utils-pipeline.R`)

* Improved logging function and standardized messaging

* Added lifecycle, future, purrr, rappdirs, and xml2 to Imports

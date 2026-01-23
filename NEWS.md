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

* Fixed curve geometry handling in `make_ramsar_sites()` for improved gpkg support

* Fixed KBA geometry handling issues

* Fixed extent alignment bug in zone functions (`make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()`)

* Fixed operator precedence bug in `make_threatened_ecosystems_restoration()`

* Fixed syntax error in `make_restore_zone()`

## Improvements

* Updated `make_protected_areas()` for WDPA schema changes

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

# elsar (development version)

## New features

* `elsar_calibrate_weights()` computes per-feature calibration weights for an ELSA
  prioritization so that features achieve more even representation relative to their
  single-feature maxima. This is the ELSA pre-calibration step, previously living in
  the downstream tool; the objective and framework are unchanged (spread-of-shortfall
  minimisation via an additive, increase-only weight update), with added robustness:
  it returns the best-seen weights, stops once the allocation freezes, and excludes
  zero-total features instead of aborting. It returns a structured `elsar_calibration`
  object (weights, representation profile, per-iteration trajectory, metadata) and
  writes no files. `prioritizr` and a solver (`gurobi`, or `highs` for a free/local
  option) are optional dependencies, checked at call time with an actionable message.

## Installation / Dependencies

* The core package is now much lighter to install. `wdpar`, `reticulate`,
  `googledrive`, and `rappdirs` moved from `Imports` to `Suggests`, so installing
  elsar no longer forces a Chromium browser, `libarchive`, conda/Python, or two
  Google OAuth flows on users who only need the core (boundaries, planning units,
  normalisation, zone builders from local rasters, plotting). The optional
  features now check for their packages and give an actionable install message
  when one is missing:
  - `make_protected_areas(from_wdpca = TRUE)` requires `wdpar`;
  - the `download_*` (Earth Engine) functions require `reticulate` and
    `googledrive` (run `elsar_setup_gee()` to configure them).

* `scales` and `png` moved from `Suggests` to `Imports`: they are used
  unconditionally by the core plotting functions, so they belong there.

## Bug Fixes

* elsar now loads the `sf` namespace when it is loaded, so `sf`'s S3 methods
  (e.g. `[.sf`, `filter.sf`) are registered. Previously the package referred to
  `sf` only via `sf::` and never imported from it, so `sf`'s namespace was not
  loaded at package-load time. As a result, operations on `sf` objects -
  including subsetting the bundled `boundary_dat` and the `dplyr` calls inside
  `make_boundary()` - could dispatch to the wrong method and fail with a `vctrs`
  "incompatible scalar type" error unless `sf` happened to be loaded some other
  way (e.g. `library(sf)`). Surfaced by running the tests against the installed
  package under `R CMD check`.

* `make_planning_units()` now errors clearly when `boundary_proj` is in a
  geographic (lat/long) CRS or has no CRS. Previously a lat/long boundary was
  silently rasterised into a **single cell** (the metre resolution was
  interpreted as degrees) and every downstream step ran on that one cell with no
  error or warning. Use `make_boundary(custom_projection = TRUE)` (the default)
  to get a suitable projected boundary.

* `make_boundary(limit_to_mainland = TRUE)` now works. It previously always
  errored (`sf::st_area()` was called on an rlang data pronoun). It now keeps the
  single largest polygon, and does so **after** any `iso3` filter, so the
  mainland is chosen within the target region rather than across the whole input
  dataset. The result is also normalised to WGS84 as the non-mainland path
  already was.

* `make_boundary()` now errors with an actionable message (listing the available
  codes) when `iso3` matches no feature, instead of silently returning a 0-row
  `sf` (or later failing with a cryptic GDAL projection error). It also checks
  that `iso3_column` / `col_name` are actually columns in the input, and that the
  boundary has a coordinate reference system before reprojecting it.

* `elsar_load_data()` no longer silently returns a non-`sf` object when reads
  fail. When loading multiple files/layers it now **errors** if every read
  fails (previously it returned a `0 x 0` tibble while logging "Loaded 0
  features"), and **warns but continues** when only some fail, naming them.
  Loading a single unreadable file now errors instead of returning `NULL`.
  Relatedly, `filter_sf()` now honours its "return `NULL` on failure" contract
  for corrupt files: the layer-name probe (`sf::st_layers()`) is now inside the
  read guard, so a bad file is skipped rather than aborting the whole load. When
  the successfully-read layers have different coordinate reference systems, the
  load now stops with a clear message naming them instead of failing deep inside
  `bind_rows()`.

* `elsar_load_data()`'s PostgreSQL path now works. The connection list is spliced
  into `DBI::dbConnect()` via `do.call()` instead of the rlang `!!!` operator,
  which never worked there (`dbConnect()` is a plain S4 generic, so `!!!x` was
  parsed as `!(!(!x))`). The path now also guards on `RPostgres`/`DBI` with an
  install hint, closes the connection with `on.exit()`, quotes table/column
  identifiers and the `iso3` literal to prevent SQL injection, and omits the
  `WHERE` clause when no `iso3`/`iso3_column` is supplied.

## New Features

* New `elsar_check_setup()` diagnoses your installation tier by tier (core /
  protected areas / Earth Engine) and reports what is present, what is missing,
  and the exact command to fix each gap. It makes no changes, needs no
  credentials, and never errors, so it is safe to run any time - and its output
  is what the bug-report template asks Earth Engine users to include.

* New `elsar_setup_gee()` bootstraps the Earth Engine environment in one call:
  it finds conda (and can install miniconda for you if you allow it), creates a
  **persistent** conda environment (`elsar_ee`) with `earthengine-api`, and
  optionally walks you through Earth Engine and Google Drive authentication. Run
  it once, ahead of time, instead of letting the first `download_*` call build
  the environment mid-analysis.

## Improvements

* The Earth Engine conda environment is now **persistent and reused** across
  sessions rather than created per-process and removed after each download.
  Previously each `download_*` call could build a `gee_temp_env_<pid>`
  environment and delete it on exit, re-downloading Python and `earthengine-api`
  every session (and, if a run was interrupted before cleanup, leaving stray
  `gee_temp_env_*` environments behind). `initialize_earthengine()` now creates
  and reuses the stable `elsar_ee` environment.

* Conda detection (`find_conda_base()`) now also consults `reticulate` (its
  managed miniconda, `conda_binary()`, `miniconda_path()`), so installations in
  non-standard locations - including a `reticulate`-installed miniconda and
  micromamba/Homebrew installs - are found instead of triggering
  "Could not find conda installation".

# elsar 0.4.0

## Breaking Changes

* Zone-function parameters were harmonised for a coherent API. The human-pressure
  input is now `human_pressure` everywhere (was `hii_input` in
  `make_protect_zone()`, `make_manage_zone()`, `make_degraded_areas()`); raw
  input rasters use bare names without the `_input` suffix
  (`agricultural_areas`, `built_areas`, `pasturelands`, `managed_forests` -
  previously `*_input`); `make_degraded_areas()` now takes `iso3`/`pus` (was
  `country_iso`/`pu`); the built-area threshold is `built_areas_threshold`
  everywhere (was `built_area_threshold` in `make_restore_zone()` /
  `make_urban_greening_zone()`); and `make_restore_zone()`'s patch filter is
  `filter_patch_size` (was `filter_small_patches`). Callers must update named
  arguments accordingly.

* `make_degraded_areas()` writes its final output as `degraded_areas_{iso3}.tif`
  (was `restore_zone_{iso3}.tif`, which was easily confused with
  `make_restore_zone()`'s `restore_zones_{iso3}.tif`).

## New Features

* The zone builders (`make_protect_zone()`, `make_restore_zone()`,
  `make_manage_zone()`, `make_urban_greening_zone()`) now write their
  region-aligned intermediate input layers to `output_path` when it is supplied,
  alongside the final zone raster - matching what `make_degraded_areas()` already
  did. The files written are, per function:
  - protect: `agriculture_areas_`, `built_areas_`, `human_pressure_`
  - restore: `agriculture_areas_`, `built_areas_`, `human_pressure_`,
    `degradation_`, `forest_mask_`
  - manage: `agriculture_areas_`, `pasturelands_`, `built_areas_`,
    `hii_mid60pct_`, `managed_forests_`
  - urban greening: `built_areas_`
  Each intermediate is `terra::mask()`-ed to the planning units, so cells outside
  the study area are NoData (consistent with the zone outputs).

  NOTE: this changes the on-disk output for existing callers that pass
  `output_path` - they now receive these additional GeoTIFF sidecars. Callers who
  want only the final zone should point `output_path` at a dedicated directory.

## Bug Fixes

* `make_degraded_areas()` now writes its resampled agriculture and built-area
  sidecars (`esa_agriculture_resample_`, `built_areas_`) as `FLT4S`. They are
  bilinear proportions (0-1) and were previously saved as `INT1U`, which
  truncated them to all-zero.

* `make_restore_zone()` now always saves the `agriculture_areas_` and
  `built_areas_` intermediates when `output_path` is set, including when they are
  derived from LULC (previously only saved when passed as explicit rasters).

* `make_normalised_raster()` no longer emits a `warning()` on every
  `rescaled = FALSE` call (demoted to an informational message), and its internal
  `output_path` writer uses a valid `INT1S` NoData sentinel (`-128`) instead of
  the out-of-range `255`.

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

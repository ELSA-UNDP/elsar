# elsar (development version)

## New features

* New tools to analyse ELSA results after a run. Areas are planar in the
  equal-area projections ELSA uses and geodesic for lon/lat rasters:
  - `elsar_action_scheme()` defines the action values, labels, colours and
    reporting groups, so custom ELSA runs carry their own legend through every
    table and plot.
  - `elsar_prepare_actions()` applies the scheme to an ELSA action raster and
    aligns existing protected areas (fractional raster or polygons).
  - `elsar_summarise_actions()` reports the area of each action nationally or
    by region, split into inside and outside existing protected areas.
  - `elsar_protection_gap()` compares current and ELSA-proposed new protection
    against an area-based target (30% by default, as in KMGBF Target 3).
  - `elsar_plot_actions()`, `elsar_plot_action_bars()`,
    `elsar_plot_protection_gap()` and `elsar_plot_region()` /
    `elsar_plot_regions()` draw maps, stacked bars, the protection gap and
    regional map-and-bars panels, all coloured by the action scheme.
  - `elsar_export_results()` runs the whole workflow in one call and writes
    CSV tables and figures; `elsar_save_plots()` saves plot lists with safe
    file names.
  - `elsar_summarise_overlap()` and `elsar_plot_overlap()` show how much of
    any layer (KBAs, IPLC lands, ELSA features, policy overlays) falls under
    each action, nationally or by region, and inside existing protected
    areas. They complement the national representation reported by the run.
  - `elsar_results_report()` renders an HTML or Word report (key figures,
    maps, tables, protection gap, overlap and regional profiles) from a new
    R Markdown template, also available in RStudio's template list.
  - `elsar_example_results()` loads a small synthetic ELSA result to try them.
  - New vignette: "Analysing and reporting ELSA results".
  - patchwork moves from Suggests to Imports.

# elsar 0.4.2

## Values change: rebuild these layers

0.4.2 fixes several calculations that gave wrong values without any error.
Layers built with 0.4.1 or earlier should be rebuilt; the pipeline should stay
on 0.4.1 until a country is rerun.

* **Every layer that goes through `make_normalised_raster()`** (almost all
  feature layers). Inputs are now aggregated to the planning units with the
  requested method (`method_override`: `"mean"`, `"sum"`, `"max"`, `"mode"`,
  ...), weighting every input cell by the share of the planning unit it
  covers. Before, they were warped onto the planning grid with bilinear
  interpolation first, which made `method_override` a no-op, point-sampled
  proportion layers (agriculture, built areas, LULC classes) and blurred
  coarse inputs. Inputs in another CRS are aggregated through a sub-grid of up
  to 10 x 10 cells per planning unit (area-weighted to within 1/100 of a
  planning unit, with bounded memory, also across the antimeridian).
* **Layers with gaps inside the planning units.** Missing data is now filled
  (`fill_na`) *after* inversion and rescaling, so it always gets the lowest
  score. Before, it was filled first: inverted layers ranked missing data
  highest, and the fill value stretched the rescale of other layers. For a
  layer with negative values (e.g. a difference), missing cells now score the
  same as the lowest value instead of mid-scale.
* **Constant layers** (e.g. a region fully covered by protected areas, or an
  all-1 zone) are no longer turned into `NaN` by `rescale_raster()`; they
  become 1 (or 0 when the value is 0), with a message.
* **Zones built from LULC, degradation or pasture rasters**
  (`make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()`,
  `make_urban_greening_zone()`, `make_urban_greening_opportunities()`).
  Thresholds such as `agriculture_threshold = 0.1` are now compared with the
  true share of each planning unit. Before, these paths rescaled the share to
  the national maximum first, so the threshold meant something different in
  every country.
  Pasture probability given as a percentage (0-100, as in the Global Pasture
  Watch layers) is divided by 100 first, so `pasturelands_threshold = 0.35`
  means a 35% probability (new `pasturelands_scale`, `"auto"` by default).
* **Layers whose values are all zero or negative** (e.g. differences) are no
  longer mistaken for empty layers and returned as all 0.
* **`make_urban_greening_opportunities()`** inverted NDVI twice, ranking green
  cells as most in need of greening; low vegetation now ranks highest and water
  lowest. It also no longer requires the planning-unit layer to be named
  "Planning Units".

* **KBA layers (`make_kbas()`).** Point KBAs are now buffered to their reported
  area: `repareakm2` is in km², but was converted as hectares, so the circles
  were 100 times too small.
* **Wetlands and Ramsar (`make_wetlands_ramsar()`).** Ramsar polygons are now
  always rasterised, with buffered points added when `buffer_points = TRUE`
  (centroids of sites that also have a boundary are ignored, via the new
  `id_column`, so those sites are not counted twice).
  Before, they were only rasterised when the data contained points and
  `buffer_points = FALSE`, so Ramsar sites were usually missing. Any positive
  wetland class now counts as wetland, so 0/1 wetland masks work (the CIFOR
  map, coded 10-100, is unaffected).
* **Protected areas with `buffer_points = FALSE`
  (`make_protected_areas()`).** When the data contained points, only the first
  protected-area polygon was rasterised; all are now included.
* **Protected areas and indigenous lands with point data
  (`convert_points_polygon()`).** Polygons whose reported area is 0 or missing
  (common in WDPCA) are no longer dropped, and a multipoint's reported area is
  shared between its points instead of given to each (also when points and
  multipoints are mixed).
* **Threatened and under-represented ecosystems, urban heat
  (`exact_rasterise()`).** `fun = mean` averaged each planning unit over *all*
  features, including ones not touching it, and the method changed above 1,000
  features. It is now the coverage-weighted mean of the features present, with
  one rule for any number of features. `max` and `min` no longer shrink at the
  edges of features. (With tens of thousands of features it is slower than the
  old approximate path; the callers in elsar pass tens to hundreds.)

* **Restore Zone (`make_restore_zone()`).** It is now the intended band of
  degraded but restorable land: `(degradation > degradation_threshold |
  HII >= hii_min) & HII < hii_max`, excluding farmland and built-up areas
  (they are managed, not restored). The upper cap (`hii_max`, default 21 on
  the HII's 0-50 scale, as in the original ELSA design) had been lost, so land
  under very high pressure was included. `human_pressure_threshold` is
  deprecated in favour of `hii_min`. The `degradation` input can now be a 0/1
  degraded-areas layer as well as an SDG 15.3.1 layer (-1 = degraded): before,
  a 0/1 layer such as `make_degraded_areas()` output silently added no
  degradation. `degradation_encoding = "auto"` reads the values within the
  planning region and stops when they are unclear. This affects any 0/1
  "Degraded Areas" input, not only `compute_degraded_areas`. Note: the new
  arguments come before `agriculture_threshold`, so positional calls shift.
* **All zones with `filter_patch_size = TRUE`.** Removing small patches used
  `terra::sieve()`, which also filled small excluded holes (farms, towns)
  back into the zone, and could keep small patches next to other small
  patches or NoData. Small eligible patches are now removed exactly and
  excluded cells always stay excluded. Patches are now edge-connected: cells
  touching only at a corner count as separate patches (they were joined
  before).

* **Planning-unit grids (`make_planning_units()`).** The grid now covers the
  whole boundary: its extent is snapped outwards to whole cells, where before
  up to one cell along the east and north edges could be lost. Grids can gain
  a row or column, so every layer should be rebuilt on the new grid.
* **Countries crossing the antimeridian** (Fiji, Kiribati, Russia, New Zealand
  with the Chathams, the US with the Aleutians, the US Minor Outlying Islands).
  The custom Mollweide projection was centred near 0 degrees instead of on the
  country, which stretched it across the whole projection (Fiji and Russia
  could not get planning units at all). Other regions keep their centre,
  including wide ones that straddle 0 degrees (e.g. France with overseas
  territories). Kiribati's planning units are slow to build at the default
  threshold (minutes, several GB): consider a lower `pu_threshold`.
* **`limit_to_mainland = TRUE`.** In `make_boundary()` it kept the largest
  *province* when the boundary had several features; it now merges them and
  keeps the largest polygon. In `make_planning_units()` it was accepted but
  ignored; it now drops islands. "Mainland" is the single largest polygon:
  the South Island for New Zealand, the contiguous states for the US.

* **Calibrated weights with lock-ins (`elsar_calibrate_weights()`).** PA
  lock-ins are now enforced, as in the original ELSA pre-calibration: locked
  cells are added to their zone and removed from every other zone, including
  `do_nothing`. Before, lock-ins only raised the zone's budget, so the weights
  were fitted to a problem without the lock-in that the ELSA tool solves (in a
  test, none of the locked cells went to Protect). Calibrations made since the
  December 2025 rewrite should be rerun. Zone masks given as 0/1 (as the zone
  builders return) are now read as 1/NA; a 0 used to make an ineligible cell
  available. New `lockin_threshold` (default 0.1, as in the ELSA tool) and
  `lockin_priority` (overlapping lock-ins go to the zone with the lowest
  raster value, as in the tool: protect, manage, restore, urban greening). A
  zone's budget floor now counts the locked cells, so fractional lock-ins no
  longer make the problem infeasible.

* **Forest integrity (`make_forest_integrity()`).** FLII (published on a
  0-10000 scale) and FSII are now each rescaled to 0-1 before they are
  averaged; before, FLII swamped FSII. Inputs that are all 0 give an all-0
  layer with a warning instead of `NaN`.
* **Flood abatement opportunities (`make_flood_abatement_opportunities()`).**
  Now `rescale(flood_risk * (1 - NDVI))`: flood-prone land lacking vegetation
  scores highest (where restoring, managing or greening it can reduce floods;
  the ELSA impacts credit the layer fully to those actions and half to
  protect), water 0. The old `flood_risk / (1 - NDVI)` inverted this (it rose
  with vegetation) and grew without bound for dense vegetation. NDVI is used on
  its own 0-1 scale, not rescaled within the region.
* **Underrepresented ecosystems (`make_underrepresented_ecosystems()`).** The
  value is now each planning unit's largest gap to the 30% target divided by
  the target (0 = target met, 1 = unprotected), as published ("30% minus the
  protected percent"). Before, the gaps were rescaled across ecosystems, so
  the largest gap in the region became 1 however small it was, and a region
  with a single ecosystem (such as Andorra) became a constant 1.
* **Threatened ecosystems for protection
  (`make_threatened_ecosystems_protection()`).** Ecosystems in another CRS
  than the planning units gave an all-NA layer (the intact-area mask was
  relabelled instead of transformed).
* **Layers written with `save_raster()`.** Each data type now has its own
  NoData value and predictor: `FLT8S` failed, and in `INT2U`/`INT4U` data the
  valid value 255 was written as NoData.
* **`get_binary_layers()`** tests each layer exactly: near-0 or near-1 floats
  were classed as binary (and then written as integers, losing them), and an
  all-NA layer shifted the labels of the others.

## Other changes

* elsar is now released under the Apache License 2.0 (was CC BY 4.0, which
  is not meant for software), as agreed by the authors.
* `make_postgres_connection()` is back (it was removed in 0.4.0 without
  deprecation, but existing code, including the pipeline, calls it). It
  returns a named list.
* `get_coverage()` counts the zone only inside the planning units, whether
  cells outside are NA or 0 (it could exceed 100%).
* `crop_global_raster()` returns an empty layer only when the input does not
  overlap the planning units; other errors are raised instead of becoming an
  all-0 layer.
* `filter_sf()` and `elsar_load_data()` warn when an ISO3 filter matches no
  features (usually a wrong `iso3_column`); multi-layer loads warn once, only
  if nothing matched. sf's "layer is ignored" warning is gone.
* `save_raster()` returns the file path invisibly and no longer emits GDAL's
  "MEM driver does not support creation option" warnings.

* `make_planning_units()` accepts boundaries with several features (it failed
  when estimating the cell size), and grows the cell size when the first
  estimate gives too many planning units instead of stopping with an error.
* ISO3 codes are matched regardless of case, both the argument (e.g. `"npl"`)
  and the codes in the data; before, a case mismatch silently matched
  nothing.

* `make_manage_zone()` works without a pasture layer (`pasturelands = NULL`,
  the documented default, used to fail), and its alternative zone (v2,
  agriculture and pasture) now also excludes built-up areas, like the default
  zone.
* `make_protect_zone()` accepts protected areas as a multi-row `SpatVector`
  and in any CRS; the docs no longer claim that `NULL` loads protected areas.

* `exact_rasterise()` accepts `fun` as `mean`, `sum`, `max` or `min` (function,
  terra generic or string); other functions now give an error. `cores` and
  `max_exact_features` are ignored. With no features it returns zeros. It
  accepts `SpatVector` input, uses a factor attribute's labels (not its level
  codes), rejects non-numeric attributes and ignores non-polygon geometry.
* `make_kbas()` and `make_wetlands_ramsar()` require an `iso3` column and say
  so, instead of silently keeping every site worldwide when it is missing.
  `make_kbas()` no longer fails when excluding AZE sites leaves no sites (it
  returns, and saves, an empty layer), and geometries are re-validated after
  reprojection.

* `make_managed_forests()` gains `rescaled` (default `TRUE`). When its output
  feeds `make_manage_zone()`, use `rescaled = FALSE`: the zone's
  `forest_class_threshold` is a true share of each planning unit.
* `make_normalised_raster()`: cells outside the planning units are now always
  NoData, also with `fill_na = NULL` (which leaves gaps inside them `NA`).

* `make_normalised_raster()` names its output layer after `name_out` (or the
  input layer), and also saves the empty layer it returns when there is no
  data.
* R CMD check now runs on the `develop` branch and on pull requests into it.

# elsar 0.4.1

## Bug fixes

* `make_underrepresented_ecosystems()` gave ecosystems that no protected area
  overlaps an `NA` gap, which ended up as the *lowest* value after rasterising.
  They now get the full gap (they are 0% protected), so the least-protected
  ecosystems score highest, as intended. Layers built with earlier versions
  under-rate completely unprotected ecosystems and should be rebuilt.

* Countries with no usable protected-area geometry (no records, none inside
  the planning units, or only points without a reported area, e.g. Somalia in
  WDPA) no longer fail:
  - `make_protected_areas()` returns an all-0 layer and a 0-row `sf`, with a
    warning that says why. New argument `return_sf` returns the cleaned
    polygons for a supplied `sf_in` too (default unchanged).
  - `make_protect_zone()` accepts 0-row protected areas; it then needs a fixed
    `hii_threshold`, since there is nothing to derive it from.
  - `make_underrepresented_ecosystems()` accepts 0-row protected areas and
    warns that the result is uniform (1 wherever there is an ecosystem).

* `make_protected_areas()` now re-validates geometries after reprojecting to the
  planning-unit CRS and before dissolving. Reprojection can turn polygons that
  were valid in the source CRS invalid, and dissolving them failed with a GEOS
  `TopologyException: side location conflict` (seen with California's 30x30
  Conserved Areas layer; 77 polygons broke on reprojection).

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

* Sub-national region codes (e.g. `"USA_CA"`) now work with global datasets:
  `make_kbas()`, `make_wetlands_ramsar()`, `make_urban_greening_opportunities()`
  and `make_protected_areas(from_wdpca = TRUE)` filter or fetch by the national
  part of the code (`"USA"`) instead of matching nothing.
* `make_protected_areas()` drops protected areas outside the planning-unit
  extent before dissolving them. Using a national WDPA dataset for a small
  study area is now much cheaper (California: 48,010 of 50,976 US sites
  skipped).

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

## Improvements

* `median_from_rast()` errors now say how to fix a missing or histogram-less
  GDAL side-car: they give the `gdalinfo -hist` command (and the
  `sf::gdal_utils()` equivalent) for the exact file terra opened, with a rough
  time estimate for large rasters. Previously the message was only
  "Side-car XML not found for <file>", which surfaced mid-run in
  `make_threatened_ecosystems_protection()` with no hint.

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

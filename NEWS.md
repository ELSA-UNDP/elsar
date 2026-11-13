# elsar 0.6.0

## Values change: rebuild these layers

Two ecosystem layers change values without any error. Rebuild them for
countries built with 0.5.0 or earlier:

* **Underrepresented ecosystems** (`make_underrepresented_ecosystems()`):
  * planning units outside every ecosystem polygon now take the nearest
    ecosystem's gap instead of 0 ("no gap");
  * overlapping protected areas, and overlapping polygons of one ecosystem,
    are now counted once (before, overlaps were counted twice).
* **Threatened ecosystems** (`make_threatened_ecosystems_protection()`, and so
  the restoration layer built from it): planning units outside every ecosystem
  polygon now take the nearest ecosystem's score instead of 0 ("not
  threatened").

The planning units outside every polygon are coasts and small islands that
coarse ecosystem polygons miss: 71,543 of Denmark's 869,106.

Nothing else changes values. The tiled overlays below give the same polygons
as before; Denmark's protected-area raster is identical cell for cell.

## Faster protected areas and ecosystem overlays

Large national polygon layers are now dissolved and intersected tile by tile,
on a grid aligned to the planning units, instead of in one GEOS operation.
Denmark's WDPCA (9 million vertices, 230,000 small "§3 protected habitat"
polygons overlapping Natura 2000 and marine sites) took 4.5 hours through the
pipeline; the full run now takes 14 minutes.

| Step (Denmark) | Before | 1 core | 16 cores |
|---|---|---|---|
| `make_protected_areas()` | 46 min | 4.2 min | 80 s |
| `make_underrepresented_ecosystems()` | about 85 min | 2.8 min | 47 s |
| `make_protect_zone()` | 11 min (dissolve alone) | 2.3 min | 43 s |

* Tiles run in parallel on Linux and macOS with `options(elsar.cores = n)`
  (default 1). Windows runs them one after another.
* Inputs below 200,000 vertices (`options(elsar.tile_min_vertices)`) use a
  single tile, so small layers do not pay a per-tile cost.
* Uses the tiled overlays: `make_protected_areas()`, `make_protect_zone()`,
  `make_underrepresented_ecosystems()`,
  `make_threatened_ecosystems_protection()`, `make_kbas()`,
  `make_wetlands_ramsar()`, `make_mangroves()`,
  `make_indigenous_managed_lands()`, `get_iucn_forests()`,
  `elsar_plot_extra_data(include_pas = )`, and protected areas or overlap
  layers given as polygons to the results functions.
* Tiled overlays log their progress (for example "Dissolving protected areas:
  105/207 tiles (51%), 29 s"), so long runs no longer look stuck.
* Not changed: `get_iucn_ecosystems()` (deprecated), and the dissolves of
  study-area outlines and regions used for plotting.

## New features

* `make_threatened_ecosystems_protection()` and
  `make_underrepresented_ecosystems()` warn when every ecosystem gets the same
  score, so the layer cannot rank planning units. The warning has class
  `elsar_uninformative_layer`, so callers can catch it; the pipeline then
  leaves the layer out. In Denmark both WWF ecoregions lie entirely below the
  global EII median, so the threatened layer was a constant 1 inside the
  ecoregion polygons and 0 in coastal gaps.
* `exact_rasterise(fill_nearest = TRUE)` gives planning units that no feature
  covers the value of the nearest feature (default `FALSE`, as before).

## Behaviour changes to know about

* `make_protected_areas(return_sf = TRUE)` still returns single polygons with
  no internal edges. Polygons cut by tile edges are merged back, so drawn
  polygons show no tile lines.
* `elsar_plot_extra_data(pas_look = "area")` draws overlapping protected areas
  once instead of stacking their transparency.
* `make_threatened_ecosystems_protection()` is documented correctly: its
  values are rescaled to 0-1, not 0-100.

## Dependencies

* `make_threatened_ecosystems_protection()` no longer needs the
  `future.apply` and `progressr` packages (still Suggests, for
  `get_iucn_ecosystems()`).

# elsar 0.5.0

## Values change: rebuild these layers

0.5.0 changes the values of two Earth Engine layers, without any error.
Rebuild them for countries built with 0.4.2 or earlier:

* **LULC proportions** (`*_proportion_*` files from
  `download_lulc_proportions()`) now read the 10 m land cover pixels. Before,
  Earth Engine's default `maxPixels` made planning units over about 80 m read
  coarser pyramid levels.
* **Layers from `download_gee_layer(aggregate_to_pus = TRUE)`** are now
  aggregated in Earth Engine with `aggregation_reducer`, where categorical
  data used to be resampled by averaging class codes. They have a new name
  (`<prefix>_<year>_<iso3>_pus_<reducer>.tif`), so old files are not reused.

See "Earth Engine downloads" below for the details.

## New features

* New `fetch_wdpca()` gets a country's WDPCA records from the first source
  that works: a per-country cache, the monthly global release (zip or `.gdb`,
  read without unzipping), the UNEP-WCMC ArcGIS services behind the Protected
  Planet map, and finally `wdpar::wdpa_fetch()`. `make_protected_areas()` now
  uses it, so a change to the Protected Planet website (which `wdpar` scrapes;
  in November 2026 the download button's class changed and every new download
  failed with "couldn't find element using query '.download__trigger'") no
  longer stops a run. Pass the global release with the new
  `make_protected_areas(wdpca_global = )` argument, the `elsar.wdpca_global`
  option or the `ELSAR_WDPCA_GLOBAL` environment variable, and choose sources
  with `wdpca_sources`. Results are cached as `WDPCA_<ISO3>_<MonYYYY>.gpkg`;
  existing `wdpar` zips in `wdpa_downloads/` are still read, and a cache older
  than the configured global release is refreshed. Transboundary sites
  (`ISO3 = "DEU;DNK;NLD"`) are kept for each country they list.

* Plots can be made in Spanish, French, Portuguese or Russian, the languages
  of the documentation. Set `options(elsar.language = "es")` once and the
  default action names, legend labels, axis titles, the national row of
  summaries, 0/1 legends, map captions, graticule degree labels (`79,5°O`)
  and number formats (`12.345`) follow it. English stays the default and its
  output is unchanged. Change single pieces of text with
  `options(elsar.labels = c(pct_of_region = "% de la provincia"))`;
  `elsar_labels()` lists the keys. Action labels written by the ELSA tool in
  any of these languages match the scheme, so an English-labelled raster
  fits a Spanish scheme without a warning. Bar charts now tilt category
  labels holding a word too long to wrap, common in translations.

* `elsar_plot_actions()` and the regional panels now fill existing protected
  areas with `pa_colour`, matching their segment in the bar charts, which
  makes them easier to see. Use `pa_fill = FALSE` for the outline only, or
  `pa_alpha` to keep the actions inside them visible.

* A new, modern map style for every map elsar draws, exposed as small
  functions that return ordinary ggplot2 components (see
  `vignette("map-style")`):
  - `elsar_theme_map()` (maps) and `elsar_theme()` (other charts): maps in a
    thin box with a light latitude/longitude graticule labelled in degrees,
    also for elsar's custom Mollweide projections (`graticule = FALSE` for a
    frameless map; the ELSA results maps of `elsar_plot_actions()` and
    `elsar_plot_region()` get the same box and graticule, inset locator maps
    stay frameless), a clear title / subtitle / caption hierarchy, legends
    with long thin colourbars or stepped keys, and a `dark = TRUE` mode.
  - Legends go on the right of maps that are clearly taller than wide, and
    under the title otherwise (`legend_position = "auto"` in
    `elsar_plot_static_raster()` and `elsar_plot_feature()`).
  - 0/1 layers (zones, lock-ins, presence layers) get a two-class legend,
    "Outside" / "Inside" (`binary_labels`), instead of value ranges such as
    "0-0.1" / "0.9-1", in both styles.
  - `elsar_palette()`, `elsar_inks()`, `scale_fill_elsar_c()`,
    `scale_fill_elsar_d()` and their `scale_colour_*` versions: viridis
    sequential ramps, a two-hue diverging palette with a neutral midpoint,
    colour-blind-safe categorical hues, and the established ELSA action
    colours (Protect `#06C723`, Restore `#8803FC`, Manage `#FFEA00`, Urban
    Greening `#FF0000`). Guides: `guide_elsar_colourbar()`,
    `guide_elsar_legend()`, `guide_elsar_steps()`.
  - Map furniture: `elsar_map_context()` / `elsar_map_outline()` (study area
    with optional neighbours, soft halo and drop shadow),
    `elsar_map_furniture()` (compact scale bar and north arrow),
    `elsar_map_caption()` / `elsar_attribution()` (attribution as a caption),
    and `elsar_map_locator()` / `elsar_add_locator()` (inset locator map).

* `elsar_plot_static_raster()`, `elsar_plot_feature()`,
  `elsar_plot_distribution()`, `elsar_plot_background()` and
  `elsar_plot_optics()` use the modern style by default. All existing
  arguments keep working. New argument `style = "classic"` (or
  `options(elsar.plot_style = "classic")` for a whole session) restores the
  previous look. Also new: `boundary` in `elsar_plot_static_raster()` (study
  area context and outline), `plot_title` in `elsar_plot_feature()` (and
  `plot_title` is now used by `elsar_plot_static_raster()` in the modern
  style), and `dark` in `elsar_plot_optics()`. In the modern style
  `elsar_plot_optics()` puts the attribution in a caption instead of a boxed
  label over the data.

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

## Earth Engine downloads

Tested with mocked Drive and Earth Engine calls, and live on Earth Engine
(a fresh export, a cache hit, `force_download`, a rerun while the export was
still running, a four-tile export and `aggregate_to_pus`).

* **Exports are matched exactly in Drive.** A file now counts only if it is
  `<export>.tif` or one of its tiles (`<export>-0000000000-0000000000.tif`).
  Before, any name starting with the export's name matched, so the `ECU`
  export could pick up `ECU_REG` files, or an old copy.
* **The wait follows the export task.** Downloads start when Earth Engine
  reports the task COMPLETED and Drive lists the same files twice in a row (its
  listing can lag). A FAILED or CANCELLED task is an error with Earth Engine's
  message. Before, the wait only watched for files to appear in Drive.
* **Every tile is downloaded.** `download_lulc_proportions()` took only the
  first tile of a split export, and `download_lulc_class_proportion()`
  stopped at the first file to appear. Running out of `wait_time` is now
  always an error: `download_gee_layer()` used to merge whatever tiles had
  arrived, with a warning.
* **A rerun waits for a task that is still running** instead of exporting the
  same layer again. That applies to all three export functions; before, only
  `download_gee_layer()` checked. The running task is looked for *before*
  Drive, because a running export's first tiles can already be there.
  `download_lulc_class_proportion()` also downloads a finished export from
  Drive instead of exporting it again.
* **`force_download = TRUE` refreshes.** `download_lulc_proportions()` now
  moves the previous export's Drive files to the bin before exporting again.
  Before, the wait picked up the old file straight away.
* **Cached proportions are found without Earth Engine.**
  `download_lulc_proportions(year = NULL)` uses the newest year already
  downloaded in `output_dir`, without starting Python or Earth Engine. Before,
  it looked only for last year's files, which miss whenever the data lag a
  year.
* **`download_gee_layer(aggregate_to_pus = TRUE)` now aggregates in Earth
  Engine** (`reduceResolution()` with `aggregation_reducer`). Before, it
  exported at planning-unit scale without aggregating, so Earth Engine
  resampled categorical data by averaging class codes. Aggregated outputs are
  now named `<prefix>_<year>_<iso3>_pus_<reducer>.tif`, so neither a
  native-resolution file nor one aggregated the old way is reused as one. They
  get the same `pre_aggregated` tag whether downloaded or read from the cache.
* **LULC proportions use the 10 m pixels** (values change: rebuild the
  `*_proportion_*` files). `download_lulc_proportions()` called
  `reduceResolution()` with Earth Engine's default `maxPixels` (64), so for
  planning units over about 80 m it read coarser pyramid levels of the land
  cover instead of the 10 m pixels.
* **`download_gee_layer()` looks for the latest year with data in the region**,
  not anywhere in the world.
* **`wait_time` is the maximum wait in minutes and may be fractional.** It must
  be positive: 0 was accepted and timed out at once, and 2.5 was rejected.
* **`year` is passed on for ESRI.** `download_lulc_data(product = "esri_10m",
  year = )` used the latest year whatever was asked. For ESA WorldCover, a year
  other than 2021 now gives a warning.
* **`merge_tiles()` takes the files to merge** (a directory still works).
  Tiles with another CRS or resolution are an error, where GDAL used to drop
  them silently. The output is written atomically, so an interrupted write no
  longer leaves a truncated file that later runs reuse.
* **`initialize_earthengine()` stops at once if Python is already running
  without `ee`.** Before, it went on to build a conda environment that
  reticulate could not switch to.
* **Network errors no longer stop a download.** Each Google Drive download is
  tried up to three times (`options(elsar.drive_download_attempts = )`), and a
  failed Drive listing while waiting for an export is retried at the next
  check. Before, one dropped connection (e.g. curl's "SSL_read: decryption
  failed or bad record mac") stopped the run partway through a multi-tile
  download. A download that keeps failing is an error that leaves no partial
  file; the export stays in Drive for the next run.
* **Quieter COG writes.** GDAL's harmless "driver MEM/GTiff does not support
  creation option" warnings are muffled wherever elsar writes a COG
  (`merge_tiles()`, `download_gee_layer()`, `download_lulc_proportions()`,
  `make_normalised_raster()`); before, large merges and the proportions
  printed them.
* `download_lulc_class_proportion()` warns about arguments passed in `...`,
  which it never used.
* The GEE download test called a function that does not exist
  (`elsar_download_gee_layer()`). It still needs `GEE_PROJECT` set to run.

## Bug fixes

* Maps narrower than they are tall, such as a region in `elsar_plot_region()`,
  draw fewer longitude lines, so their degree labels no longer run into each
  other. Wide maps keep sf's own graticule.

* `elsar_plot_distribution(categorical = TRUE, col_interest = ...)` failed
  because of a missing pipe; it now uses the given column. With
  `categorical = FALSE` and no `col_interest`, it uses the first data column.

* `elsar_plot_static_raster(data_type = "discrete")` now defaults
  `data_layer` to the first layer instead of failing when it is not given.

* `elsar_calibrate_weights()` uses `add_max_wtd_sum_objective()` with
  prioritizr 9 or later, which renamed `add_max_utility_objective()` and warns
  on every solve when the old name is used. Earlier prioritizr versions keep
  the old name.

* `save_raster()` no longer warns "driver GTiff does not support creation
  option OVERVIEWS" (and `OVERVIEW_RESAMPLING`) for every layer when terra
  writes to disk in chunks (`terraOptions(todisk = TRUE)`, as the pipeline
  sets). The files were always valid COGs with overviews; the warning came from
  terra's intermediate plain GeoTIFF.

* `make_threatened_ecosystems_protection()` and `get_iucn_ecosystems()` no
  longer warn "UNRELIABLE VALUE: One of the 'future.apply' iterations ...
  unexpectedly generated random numbers": their parallel loops now declare
  `future.seed = TRUE`. Results are unchanged.

## Installation / Dependencies

* `ggplot2 (>= 3.5.0)` is now required (for the legend layout of the new map
  style). `systemfonts` (font fallback) is a new `Suggests`.

* The deprecated `elsar_plot_static_raster_c()`, `elsar_plot_static_raster_d()`,
  `elsar_plot_background_c()` and `elsar_plot_background_d()` now signal
  deprecation through lifecycle (once per session) instead of `.Deprecated()`.

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

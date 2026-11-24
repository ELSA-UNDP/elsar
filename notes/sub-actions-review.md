# Sub-actions: review of the Peru script and a design for elsar

Source reviewed: `per_national_v2/analyses/sub_actions/Peru_sub_actions.R` (2,232 lines),
with `Sub_Actions.csv`, `ELSA_summary.csv`, `PeruSubActions.gpkg` and the two
"ELSA Adaptation report – Peru" drafts in the same folder.

## 1. What the sub-action analysis does

ELSA gives each planning unit one broad action: Protect, Restore or Manage. The
script splits each action into specific interventions, using rules over
context layers. It works on 1 km planning units turned into polygons. Every input
raster is read onto them with `exact_extract(..., "median")`, and the script then
filters rows.

| Parent  | Sub-action                         | Rule (as written)                                                                                   | Area (km²) |
|---------|------------------------------------|------------------------------------------------------------------------------------------------------|-----------:|
| Manage  | Forest management                  | (FSII > 0.1 or FSCI > 0.1), not coastal, no agriculture, ecoregion in {Yungas, Bosque Seco Ecuatorial, Amazonico} | 8,322 |
| Manage  | Sustainable agriculture            | not coastal, agriculture > 0 (then split by quartile of 2071–2100 crop suitability)               | 54,119 |
| Manage  | Coastal management                 | within 50 km of the marine ecoregions                                                                | 478 |
| Restore | Riparian restoration               | within 1 km of main rivers, not coastal, not wetland or Ramsar                                     | 60,911 |
| Restore | Wetland & mangrove restoration     | not riparian, and (coastal or wetland/flooded vegetation or Ramsar)                                 | 1,932 |
| Restore | Forest restoration                 | not coastal/riparian/wetland/Ramsar, forest ecoregion                                                | 11,605 |
| Restore | Non-forest ecosystem restoration   | same, non-forest ecoregion                                                                           | 26,469 |
| Protect | Existing PAs / Expansion / New / Biological corridor | Protect cells; New/Expansion assigned **by hand** (written to `new_PA.gpkg`, edited, read back) | 238k / 153k / 59k / 192k |

Each sub-action is then summed by department (GADM level 1) and ecoregion, and
mapped (figures 9–20). The facet bar chart (figure 21) and the context-layer maps
(FSII, crop suitability, agriculture, wetlands, riparian zone; figures 5–15) feed
the Adaptation report.

**The idea is sound and worth having in elsar.** It turns "Restore 15%" into "about
61k km² of riparian restoration in these departments", which is what ministries
and funders plan and budget around. It is also what most country teams ask for
after a run. Right now each country redoes it by hand.

## 2. Problems with the current script (why the numbers can't be reused as they are)

Correctness:

1. **Area is double-counted by `st_join`.** `pu` is joined to departments, the
   coastal buffer (two overlapping marine-ecoregion buffers), every overlapping
   river buffer and ecoregions, each time as a left join. A planning unit that
   touches *k* features is copied *k* times, and `sum(area)` counts every copy.
   Riparian restoration is probably the most inflated: a cell where buffers of
   several river segments meet is counted once per segment. I have not measured
   how large the effect is. Until it is checked, every area in
   `Sub_Actions.csv` is suspect.
   (`coast.x`/`coast.y` exist because the coast column was joined twice.)
2. **The saved script does not reproduce the saved outputs.** Actions are labelled
   `"Manage (10%)"`, `"Restore (15%)"` and `"Protect (30%)"`, but the sub-action
   filters test `"Manage (5%)"`, `"Restore (5%)"` and `"Protect (17%)"`, which are
   labels from earlier runs. As saved, those filters select nothing.
3. **Sub-actions are not exhaustive, and the rules are inconsistent.** Manage cells
   that are not forest, agriculture or coast get no sub-action. Restore cells that
   are both riparian and coastal/wetland are dropped. Forest management lists
   "Serrania Esteparia" in its filter and then removes it again through
   `ordered()`. Nothing checks that the sub-actions add up to their parent action.
4. **Protect sub-actions are not reproducible.** New, Expansion and Corridor come
   from a hand-edited GeoPackage.
5. **Outputs overwrite each other.** Every `st_write(..., delete_dsn = TRUE)`
   deletes the file, so `PeruSubActions.gpkg` keeps only one layer. The PA summary
   is written to a layer named `Riparian_Restoration`.
6. **Columns are picked by position** (`AE[,107]`, `AE[,-c(124)]`, `pu[78]`,
   `raster_files[71]`). The script breaks silently when an input changes.
7. **Medians of 1 km rasters on 1 km cells, then `> 0` tests.** With fractional
   cover layers this is effectively an arbitrary threshold, and it is not
   documented.

Maintainability and security:

- **A Mapbox API key is hardcoded (line 34)** in a file synced through Dropbox.
  Rotate it, and read keys from `Sys.getenv()`.
- It uses retired packages (`raster`, `rgeos`, `gdalUtils`, `ggsn`,
  `raster::getData`), calls `install.packages()` inline, uses absolute Windows
  paths, and has about 1,500 lines of repeated `ggplot`/`tmap` theme code.
- Vector work on about 1.3M planning-unit polygons is slow. elsar's results
  functions already do this exactly on rasters.

## 3. Fit with elsar: very good, because most of the machinery already exists

The 0.7 results module is the right home:

- `elsar_action_scheme()` already has a **`group`** column, and
  `elsar_plot_action_bars()` already stacks actions that share a group. A
  sub-action map is just an action map whose scheme has labels
  `"Riparian restoration"`, `"Forest restoration"`, ... with `group = "Restore"`.
- After that, `elsar_summarise_actions()` (by region, inside/outside PAs, exact
  areas), `elsar_summarise_overlap()`, `elsar_plot_actions()`,
  `elsar_plot_region(s)()`, `elsar_export_results()` and `elsar_results_report()`
  **work on sub-actions without changes**. Figure 21 becomes one
  `elsar_plot_action_bars()` call.
- The context layers mostly exist already as `make_*` builders:
  `make_forest_integrity()`, `make_wetlands_ramsar()`, `make_mangroves()`,
  `get_iucn_ecosystems()`, `get_lulc_classes()` (cropland), `make_protected_areas()`.
- It works on rasters, with exact area weighting, so there are no
  join-duplication bugs and no tile seams (in line with the decision that
  overlays stay exact).

So the new code is mostly one thing: **turning rules into a categorical raster**.

## 4. Proposed design

### 4.1 Core: declarative, ordered, exhaustive rules

```r
rules <- elsar_sub_actions(
  parent = "Restore",
  sub_action("Wetland & mangrove restoration", wetland > 0.5 | mangrove > 0.5 | coastal),
  sub_action("Riparian restoration",           riparian > 0.5),
  sub_action("Forest restoration",             forest_biome),
  other = "Other ecosystem restoration"        # remainder, so the parts sum to the parent
) |>
  add_sub_actions(
    parent = "Manage",
    sub_action("Coastal management",       coastal),
    sub_action("Sustainable agriculture",  cropland > 0.3),
    sub_action("Forest management",        flii >= 0.1 & forest_biome),
    other = "Other land management"
  ) |>
  add_protect_sub_actions(expansion_km = 2, corridor = TRUE)   # see 4.3

layers <- c(wetland = ..., mangrove = ..., riparian = make_riparian_zone(...),
            coastal = make_coastal_zone(pus, km = 10), cropland = ..., flii = ...,
            forest_biome = ...)

x   <- elsar_prepare_actions("04_ELSA_Action.tif", pas = pas)
sub <- elsar_assign_sub_actions(x, rules, layers)   # categorical SpatRaster + derived scheme
elsar_summarise_actions(sub, regions = adm1, region_col = "name", units = "km2")
elsar_plot_action_bars(...)                        # stacked by parent group
elsar_sub_action_diagnostics(sub)                  # see 4.2
```

Semantics:

- **First match wins**, in the order written. Priority is explicit, so a cell is
  never counted twice and there are no gaps. `other` (on by default) collects the
  rest of the parent action.
- Conditions are R expressions over the names in `layers`, captured with
  `rlang::enquo` and evaluated with terra arithmetic. The same rules can be read
  from CSV (`parent, sub_action, condition`). There, conditions are parsed
  against an allow-list of operators (`> >= < <= == & | ! is.na`), so a rules
  file can't run arbitrary code. This keeps a country's rules reviewable and
  reusable next to its analysis.
- Output: a categorical raster whose levels are the sub-action scheme
  (`group` = parent, colours derived from the parent colour as tints, labels
  translated through `elsar_labels()`). Do Nothing and PAs pass through unchanged.

### 4.2 Diagnostics (the part the Peru script had none of)

`elsar_sub_action_diagnostics()` returns, for each parent:
- area of each sub-action, and a check that they add up to the parent
- the "first-match shadow": area that matched rule *i* and also a later rule
  *j*, so users can see what the ordering decided
- the share of the remainder (`other`). If it is large, the rules are not
  describing the action well
- NA coverage of each context layer within the parent action

### 4.3 Protect sub-actions, done by algorithm and not by hand

`add_protect_sub_actions()` splits Protect using `pa_fraction` and patch analysis
(`terra::patches`):
- **Existing PA**: `pa_fraction >= pa_threshold`
- **Expansion**: Protect patches adjacent to, or within `expansion_km` of, an
  existing PA
- **Corridor** (optional): patches that touch two or more different PAs, or a
  thinness test (area/perimeter) between PAs
- **New PA**: the rest, optionally only patches ≥ `min_patch_km2`; smaller ones
  become "Other area-based conservation (OECM candidate)"

This replaces the hand-edited `new_PA.gpkg`, and it reports directly against
GBF Target 3 (how much of the 30% is existing, expansion or new).

### 4.4 New context-layer builders

- `make_riparian_zone(pus, rivers = NULL, buffer_m = 1000, min_order = ...)`,
  with HydroRIVERS as the global default and a national layer as an option.
  Output is cover fraction.
- `make_coastal_zone(pus, km = 10)`, as distance to coastline. It should not be a
  50 km buffer of marine ecoregions, which reaches far inland in Peru.
- `make_forest_biome(pus)` / `get_iucn_ecosystems()` → forest/non-forest from
  IUCN GET functional groups (T1, T2, MFT1.2 mangroves). This **replaces the
  hand-picked Peruvian ecoregion names**, so the default rules work in any
  country.

### 4.5 A global default catalogue

Ship `elsar_sub_action_catalogue()`: a default rule set that uses only global
layers elsar can already build. It works out of the box for every country, and
countries can change thresholds or add national layers. Each sub-action carries
metadata:
- **IUCN Global Standard for NbS / GBF target** mapping (e.g. riparian and
  wetland restoration → Target 2; agriculture → Target 10; Protect splits →
  Target 3)
- optional **unit cost and benefit** columns (USD/ha, tCO₂e/ha), so that
  `elsar_cost_sub_actions()` can give a first budget envelope per department.
  Countries ask for this in NBSAP/NDC costing, and the Peru Adaptation report
  already frames sub-actions this way.

### 4.6 Characterising inside each sub-action

The Peru figures 6/10/11 show the distribution of a continuous layer (crop
suitability, FSII) within a sub-action. Add
`elsar_summarise_values(x, layers, breaks = "quantile")`: area by value class per
action/sub-action/region, so tables like "sustainable agriculture by
future-suitability quartile" are one call. It would use fixed or quantile breaks,
not k-means, so maps can be compared between runs.

### 4.7 Sensitivity

Thresholds (FLII 0.1, riparian 1 km, coast distance, cropland cover) are
judgement calls. `elsar_sub_action_sensitivity(x, rules, layers, vary = list(riparian_buffer = c(500, 1000, 2000)))`
reruns the assignment over a grid and reports how much each sub-action's area
changes, so the report can say how robust each figure is.

### 4.8 Reporting

- A sub-actions chapter in `elsar_results_report()`: a national treemap/sunburst
  (action → sub-action), stacked regional bars, sub-action maps, the
  diagnostics table, and the GBF mapping.
- `elsar_export_results()` writes the sub-action raster, the scheme and tables,
  with one GeoPackage layer per sub-action (fixing the Peru overwrite bug), plus
  the rules file used, so the run can be reproduced.

## 5. Phasing

1. **Done (#140):** `elsar_sub_actions()` / `sub_action()` / `elsar_assign_sub_actions()`,
   derived scheme, `other` remainder, sum-to-parent check, diagnostics, and a
   vignette section. All the downstream summaries and plots then work.
2. **Done (#141, #142):** `add_protect_sub_actions()` (patch-based), and
   `make_riparian_zone()` / `make_coastal_zone()`.
3. **Done (this branch):** global catalogue with GBF targets, CSV rule I/O with
   an allow-listed parser.
4. **Next:** `elsar_summarise_values()`, sensitivity, cost envelope, report
   chapter.
5. **Done:** Peru rebuild and comparison
   (Dropbox `per_national_v2/analyses/sub_actions_v2/`). It confirmed the
   double-counting: simulating the original joins reproduces its riparian total
   within 1%.

## 6. Future, if needed: pipeline integration

Sub-actions are a post-hoc analysis of a finished ELSA run, so they do not need
to run inside elsar_pipeline, and they are not planned there. Revisit this only
if countries start asking for sub-action outputs routinely, with every build.
Two possible routes:

- elsar_pipeline: an optional `sub_actions:` block in the country config,
  naming a rules CSV (from `elsar_write_sub_actions()`) or the catalogue with
  `drop` and Protect settings. It would build the layers that
  `elsar_sub_action_layers()` lists, and write the sub-action outputs beside the
  other results.
- elsa-config-mapper: an editor for the rules CSV, with the allow-listed
  conditions validated as they are typed.

Until then, a country's rules CSV can sit in its analysis folder next to the
script that uses it.

## 7. Open questions for the team

- Should sub-actions be strictly exclusive (first match) or allowed to overlap
  (multi-label, reported as overlap)? First match is recommended for area
  accounting, with overlaps shown in the diagnostics.
- Should the default catalogue include costs, or leave them to countries only?
- Should sub-action labels follow the ELSA tool's naming, or a new controlled
  vocabulary that needs translation?

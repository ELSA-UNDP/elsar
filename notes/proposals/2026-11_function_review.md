# elsar per-function review (November 2026)

**Status:** proposals only. No package code was changed.

**Companion to** [`2026-11_audit_and_roadmap.md`](2026-11_audit_and_roadmap.md),
which reviewed elsar area by area. This review goes through every exported
function (and the internal helpers they rely on) one at a time, and records how
`elsar_pipeline` calls each one.

**Scope:** `develop` at f42e0df (0.4.1), checked against `elsar_pipeline`
(1fd6f43) and `elsa_national` (c4f5f75). Plotting was also checked against
draft PR #103.

**Detailed cards**, one per function, are in [`function_review/`](function_review/):

| File | Group | Functions |
|---|---|---|
| [01_region_utils.md](function_review/01_region_utils.md) | Region, I/O and shared utilities | 29 |
| [02_features.md](function_review/02_features.md) | Feature layer builders | 17 |
| [03_zones_calibration.md](function_review/03_zones_calibration.md) | Zones, ELSA utilities, calibration, LULC config | 13 |
| [04_gee_setup.md](function_review/04_gee_setup.md) | Earth Engine, downloads and setup | 34 |
| [05_plotting.md](function_review/05_plotting.md) | Plotting | 12 |

Each card gives:

- what the function does;
- who calls it (pipeline call count and the arguments used);
- the method behind it, for layer and zone builders;
- a verdict;
- new issues, with evidence;
- improvements, with effort (S/M/L).

Evidence tags:

- **[ran]** or **[verified]**: reproduced on synthetic data with scratch
  scripts. Those scripts are not committed.
- **[code]**: read from the code but not run.
- **UNVERIFIED**: neither.

Findings already in the audit are cited as `A§2-#n` and not repeated.

---

## 1. Verdicts

| Verdict | Region & utils | Features | Zones & calibration | Earth Engine | Plotting | **Total** |
|---|---|---|---|---|---|---|
| keep | 2 | 1 | 3 | 8 | 0 | **14** |
| fix | 10 | 6 | 5 | 9 | 1 | **31** |
| refactor | 3 | 5 | 2 | 8 | 2 | **20** |
| merge into a shared helper | 0 | 2 | 1 | 5 | 0 | **8** |
| make internal | 7 | 0 | 0 | 0 | 2 | **9** |
| deprecate | 6 | 3 | 2 | 1 | 7 | **19** |
| delete | 1 | 0 | 0 | 3 | 0 | **4** |
| **Functions** | 29 | 17 | 13 | 34 | 12 | **105** |

Only 14 of 105 functions are fine as they are. Most "fix" verdicts are small
and local. The "refactor" and "merge" verdicts reduce to five shared building
blocks (§4).

### What the pipeline actually uses

Narrowing the public API is safer than it looks. `elsar_pipeline` calls:

- the region functions: `make_boundary`, `make_planning_units`,
  `elsar_load_data`;
- `save_raster`, `get_binary_layers`, `get_coverage`;
- the feature and zone builders;
- `elsar_calibrate_weights`;
- `elsar_plot_feature` (30 call sites, for preview PNGs in its Shiny app);
- `download_lulc_proportions` and `check_and_download_required_layers`
  (one call each).

None of the other Earth Engine functions, none of the other plotting
functions, and 14 of the 29 utilities have any caller outside elsar.

---

## 2. New findings not in the audit (proposed for 0.4.2)

These are additions to audit §2. Under decision 5 they ship in the same 0.4.2
release and join the "rebuild these layers" list where they change values.

| # | Problem | Card | Evidence |
|---|---|---|---|
| F1 | `make_protected_areas(buffer_points = FALSE)` rasterises **only the first protected area** when the data contains any point: the polygons are not dissolved before `coverage_fraction(...)[[1]]`. | 02 | [ran] 9 of 18 cells; [code] confirmed |
| F2 | `exact_rasterise(fun = mean)` (the default) averages every cell over **all** features, including ones that don't touch it, giving value/N. The result jumps at the 1,000-feature fast-path switch. The per-feature path is ~70× slower than a single `exact_extract(include_cell = TRUE)`. | 01, 02 | [ran] 0.333 instead of 1; 20.7 s vs 0.31 s |
| F3 | `make_normalised_raster()` checks for an empty layer **before** applying `conditional_expression`. Layers whose values are all ≤ 0 come back as all 0, which hits the pipeline's "Crop Suitability Difference" and "Agricultural Climate Stress" layers. The early return also skips `output_path` and names the layer "Planning Units". | 02 | [ran] |
| F4 | `make_forest_integrity()` adds raw FLII (0–10,000) to FSII, so FSII has no effect. | 02 | [ran] |
| F5 | `make_flood_abatement_opportunities()`: the greenest cell scores 0 and the next-greenest scores the maximum. A constant flood input gives NaN. | 02 | [ran] |
| F6 | `make_wetlands_ramsar()` treats wetland class value 1 as non-wetland (`x > 1`), so a 0/1 wetland mask contributes nothing. | 02 | [ran] |
| F7 | `make_underrepresented_ecosystems()` halves the score of edge planning units, and swallows intersection errors. | 02 | [ran] |
| F8 | `make_threatened_ecosystems_protection()` uses `st_set_crs` instead of `st_transform`: ecosystems in another CRS give an all-NA layer. The docs say 0–100 but the output is 0–1. Missing integrity values are treated inconsistently between modes. | 02 | [ran] |
| F9 | `make_boundary(limit_to_mainland = TRUE, dissolve = TRUE)` keeps the largest **province**, not the mainland, because it picks before dissolving. | 01 | [ran] 12,364 vs ~24,700 km² |
| F10 | `make_planning_units()`: a multi-feature boundary crashes, and the extent is cut to whole cells, losing up to one cell along the east and north edges. | 01 | [ran] |
| F11 | `save_raster()` treats every type other than FLT4S as 8-bit integer: FLT8S fails, INT2U turns 255 into NA, and every call emits 5 GDAL warnings (36 call sites). | 01 | [ran] |
| F12 | `get_binary_layers()` rounds before testing, so layers with values like 4e-5 count as binary and the pipeline writes them as INT1U, which zeroes them. | 01 | [ran] |
| F13 | Layers fail silently in several places: `crop_global_raster()` returns zeros on failure; `filter_sf()` with a wrong `iso3_column` returns 0 rows; `elsar_load_data()` ignores `iso3` without `iso3_column`; the PostGIS path skips filtering and repair. | 01 | [ran] |
| F14 | **Calibration doesn't enforce the PA lock-in, which the ELSA pre-calibration method requires.** The original pre-calibration locked PAs into Protect (`pre_global.R` in 2021, `pre_run.R` in Feb 2025, both with `add_locked_in_constraints`). The Dec 2025 rewrite in `elsa_national` stopped enforcing it: it loaded `by_zone_locked` but passed the unlocked masks. `elsar_calibrate_weights()` inherited that in Jul 2026: lock-ins only raise the area cap, and `do_nothing` covers every planning unit. The pipeline passes PA lock-ins believing "elsar forces those cells into the zone". **Weights calibrated since Dec 2025 were fitted without the lock-in**; rerun them after the fix. | 03 | [ran] 0 of 20 lock-in cells allocated to protect; history from `elsa_national` git log |
| F15 | Calibration accepts the 0/1 zone masks every zone builder returns, but needs 1/NA, so zone eligibility is silently ignored. The pipeline converts the masks first; direct users don't. | 03 | [ran] 10 eligible → 100 allocated |
| F16 | `make_protect_zone()`: the issue #79 rule is a cliff. When the mean HII inside protected areas reaches 4, the cutoff switches from q95 to the mean (−45% eligible cells for +0.1 HII). The rule is undocumented and the constant is hard-coded. | 03 | [ran] |
| F17 | `make_manage_zone()` crashes without a pasture layer (the default). Manage v2 does not exclude built areas. | 03 | [ran] |
| F18 | `get_coverage()` doesn't mask to the planning units and can exceed 100%. | 03 | [ran] 105% |
| F19 | Earth Engine: `download_lulc_proportions()` downloads only the **first tile** of each export and resubmits duplicate exports on reruns. `download_gee_layer(aggregate_to_pus = TRUE)` does no aggregation in Earth Engine. A tag set on fresh downloads, but missing on cache hits, makes first and cached runs give different results. | 04 | [code]; tag loss [verified] |
| F20 | Plotting (preview figures only): classic binned legends sort as text (`10–12.5` before `2.5–5`), scrambling the colour ramp. 0/1 layers are drawn on a continuous colour bar. The `type` switch matches single letters, so `"ggplot_vector"` draws a raster and `"tmap"` returns a ggplot. | 05 | [ran], images checked |

---

## 3. The pipeline contract

The pipeline stays private, so elsar's API is its contract. The function
review found that this contract is broken or ambiguous in eleven places. Several
of these silently change what countries get.

| # | Mismatch | Effect | Fix (elsar side) |
|---|---|---|---|
| P1 | **`wait_time` units.** Pipeline configs set `wait_time: 3` as "seconds between status checks"; elsar reads it as the maximum total wait in minutes. | Earth Engine exports time out after about 3 minutes. | Rename to `max_wait_minutes` and add `poll_seconds`, deprecating `wait_time`. Pipeline configs need updating too. |
| P2 | **`limit_to_mainland` in `make_planning_units()` does nothing.** 10 country configs set it, documented as "exclude islands". | Island settings have never had an effect. | Implement it (after fixing F9), or deprecate it with a warning. |
| P3 | **`compute_degraded_areas: true`.** The pipeline passes `make_degraded_areas()` output (0/1) into `make_restore_zone()`, which counts a cell as degraded only when it is `-1`. | Degradation is dropped from the Restore Zone (0 cells instead of 118 in the test). | `make_restore_zone()` accepts either encoding through an explicit `degradation_encoding`, or `make_degraded_areas()` documents its output as the restore input. |
| P4 | **The Restore Zone's human-pressure rule has lost its upper cap.** The intended design is a middle band: degraded land that is still restorable, with very high pressure excluded. The 2021 tool documented "Restore: … IN Forest Biome **AND HFP < 21**", and the maintainers confirmed this in Nov 2026. elsar today uses `(degradation > 0.1 OR HII >= 4)` with **no upper cap**, so HII ≥ 4 *adds* cells and the most pressured land qualifies unless it is over 10% farmland or 10% built-up. The pipeline's `defaults.yaml` says the zone "excludes cells with HII ≥ 4", which matches neither. `make_degraded_areas()` also counts agriculture and built-up land as degraded, which the Restore Zone then excludes. | The Restore Zone includes land the method meant to exclude; the docs, pipeline and code disagree. | Restore the band: `restore = (degradation > degradation_threshold OR HII >= hii_min) AND HII < hii_max AND agriculture <= t AND built <= t`. **Decided (Nov 2026):** both signals identify degraded land (the degraded-areas layer *or* HII ≥ `hii_min`), and the upper cap excludes land too pressured to restore. Defaults are `hii_min = 4` and `hii_max = 21`, both on the HII's existing 0–50 scale. Rename `human_pressure_threshold` to `hii_min` with a lifecycle deprecation, add `hii_max`, and return the applied rule as data (P5). Document the rule in the roxygen and a zones article. Fix the pipeline comment. Decide whether `make_degraded_areas()` should stop counting farmland and built-up land as degraded. Value-changing, so it goes in 0.4.2 with the "rebuild" list. |
| P5 | **The pipeline greps elsar's log messages** to recover the effective protect-zone HII cutoff. | Rewording a log line silently breaks the pipeline's record. | Return the rule that was applied as data (e.g. `attr(zone, "elsar_zone")`). |
| P6 | **Strict mode.** With `interactive = FALSE`, `check_and_download_required_layers()` turns download errors into warnings. | A strict `--auto` run continues after a Pasturelands timeout. | Error in non-interactive mode, or return a status table. |
| P7 | **`force_download` / `--force-gee` has no effect.** The wait loop picks up the old Drive file with the same name straight away. | Forced refreshes don't refresh. | Exact filename match and per-run export names (audit §2 #13). |
| P8 | **ISO3 validation differs.** elsar accepts `usa` and `USA-CA`; the pipeline rejects them. A lowercase code passes elsar's check, then matches nothing in the KBA, Ramsar, WDPA or GHSL filters. | Empty layers with no error. | Normalise to upper case and validate the suffix in one helper. |
| P9 | **Removed or copied functions.** The pipeline calls `elsar::make_postgres_connection()` (removed without deprecation), keeps its own inline copy of `exact_rasterise`, and shares `filter_patch_size`/`min_patch_size` keys that override the urban greening defaults. | Breaks on a fresh install; copied code drifts. | Restore `make_postgres_connection()` with a deprecation, or coordinate the removal; fix F2 so the pipeline can drop its copy. |
| P11 | **PA lock-in in calibration** (F14). The pipeline passes lock-ins expecting elsar to force them into their zone, as the original pre-calibration did. elsar only uses them as a budget floor. | Calibrated weights don't match the problem the ELSA tool solves. | Enforce lock-ins in `elsar_calibrate_weights()` (S–M) and rerun calibrations made since Dec 2025. |
| P10 | **The calibration sidecar** is defined in the pipeline and `elsa_national`, not in elsar, although elsar writes the weights. | Three repos must agree on an unwritten format. | Document the sidecar schema in elsar, with a version field. |

**Recommendation:** add `tests/testthat/test-pipeline-contract.R` to elsar. It
would call each function the way the pipeline does (same arguments, same
input encodings) on synthetic data and check the output types and encodings
the pipeline relies on. It is a cheap guard while the pipeline stays private.

---

## 4. Shared building blocks

The 20 refactor and 8 merge verdicts come down to five pieces. The cards
contain signatures and examples.

1. **`align_to_pus()`** (audit §6). It would replace four separate alignment
   implementations, including `make_degraded_areas()`'s 12 warps. Rules:
   area-weighted; thresholded inputs never rescaled; NA kept until the
   caller decides.
2. **`vector_coverage()`** (audit §6). Mangroves and indigenous lands merge
   into it directly. Protected areas, KBAs and Ramsar use it, which also
   fixes F1 and A§2-#5/#6.
3. **An ecosystem-score helper** built on one `exact_extract()` call (new,
   card 02). It would replace the polygon-intersection loops and the
   `future` machinery in the threatened and underrepresented ecosystem
   builders (fixes F7 and F8).
4. **A shared zone builder** (card 03, Cross-cutting). It would remove about
   450 of the 1,145 lines in the four zone builders. Each zone becomes a list
   of include/exclude criteria. Inputs are aligned once per country instead
   of 3–4 times, the sieve touches only the 1-class, and the applied rule is
   returned as data (P5). **Issue #96** becomes an `exclude =` argument:

   ```r
   # Netherlands: keep HII and built-area exclusions, let farmland into Protect
   make_protect_zone(region, protected_areas = pas, lulc = lulc,
                     exclude = c("human_pressure", "built_areas"))
   ```

5. **An Earth Engine core** (card 04, Cross-cutting). It would replace the
   12 current exports with 9 public functions: `elsar_setup_gee`,
   `elsar_check_setup`, `elsar_gee_session`, `elsar_gee_products`,
   `download_gee_layer`, `download_lulc`, `download_lulc_proportions`
   (returning one multi-layer stack), `read_lulc` and `elsar_gee_tasks`.
   Card 04 maps each old function to its replacement.
   - Correction to audit §5: sending `EPSG:xxxx` doesn't work for elsar's
     own custom Mollweide grids, which have no EPSG code. The projection
     definition has to be rebuilt from `lon_0`.

**Plotting in 1.0** (card 05):

- `elsar_plot_feature()` and `elsar_plot_static_raster()` are kept but
  rebuilt on PR #103's building blocks, with 0/1 layers drawn as categories.
- `elsar_plot_distribution()` is fixed.
- Seven functions are deprecated: the four `_c`/`_d` variants and
  `elsar_plot_optics`, `elsar_plot_background` and `elsar_plot_extra_data`.
- `elsar_save_plots()` (results branch) becomes the only plot-file writer.

The pipeline gets classic plots until PR #103 merges, so the plotting fixes
in 0.4.2 go into the classic code path.

---

## 5. Effect on the roadmap

- **0.4.2** grows by F1–F20 and the elsar-side fixes for P1–P3, P6 and P8.
  The pipeline-contract test goes in the same release. Pipeline configs
  need small changes for P1 and P2.
- **0.5** adds the #96 design (now concrete), the P5 rule-as-data and the
  P10 sidecar schema, and makes the 9 "internal" functions internal through
  a deprecation cycle.
- **0.6** builds the five shared blocks above. Most refactor and merge
  verdicts close there.

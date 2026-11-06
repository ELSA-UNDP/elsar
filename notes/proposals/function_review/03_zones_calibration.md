# Function review 03: zones, ELSA utilities, calibration, LULC config

**Code:** `/home/scottca/gitrepos/elsar-audit` (branch `audit/repo-review` = `develop` f42e0df + audit doc).
**Downstream checked:** `elsar_pipeline` 1fd6f43 (R/, app/, run_*.R, config/defaults.yaml), `elsa_national` c4f5f75.
**Audit references:** "A§2-#n" = row n of §2 of `notes/proposals/2026-11_audit_and_roadmap.md`; "NAT-Bn/P3.n" = `elsa_national/docs/proposals/2026-11_audit_and_roadmap.md`.
**Evidence:** scripts in this folder: `t_zones.R`, `t_utils.R`, `t_calib.R`, `t_calib2.R` (synthetic 20x20 / 10x10 grids, EPSG:3857; prioritizr 8.1.0 + highs). "[ran]" = reproduced with those scripts; "[code]" = read from code, not run; UNVERIFIED = neither.

---

## Zone builders

How the four zones are built today (defaults; all thresholds strict unless marked):

| Criterion | Protect | Restore v1 | Manage v1 | Manage v2 | Urban greening |
|---|---|---|---|---|---|
| Start set | all PUs | degraded (SDG == -1 share > 0.1) **or** HII >= 4 | HII in [q20, q80] (national) **or** managed forest > 0.1 **or** ag > 0.1 **or** pasture > 0.35 | ag > 0.1 **or** pasture > 0.35 | built > 0 |
| HII | exclude HII >= PA q95 (or >= PA mean if mean >= 4, or fixed) | inclusion (absolute 4) | inclusion (relative percentile) | – | – |
| Agriculture | exclude > 0.1 | exclude > 0.1 | **include** > 0.1 | include > 0.1 | – |
| Built | exclude > 0.1 | exclude > 0.1 | exclude > 0.1 | **not excluded** | include > 0 |
| Sieve default | 20 cells | 10 | 10 | 10 | off |
| Output | 1 layer `protect_zone` | 2 layers (v2 = v1 and forest > 0.1) | 2 layers | – | 1 layer |
| File | `protect_zone_` | `restore_zones_` | `manage_zone_` | (same file) | `urban_greening_zone_` |

Three different HII scales are in use: a protected-area-derived cutoff (protect), a fixed absolute 4 (restore) and a national percentile band (manage). Nothing documents why, and one cell can fall into no zone or into all of them. Built cells with 0 < built <= 0.1 are in both Protect and Urban Greening ([ran] T8: 11/11 cells with 5% built are in both). That may be intended, since ELSA allows overlapping zones, but it is not written down anywhere. No vignette or article describes these rules: the four builders appear only in the README and `_pkgdown.yml`.

### `make_protect_zone()`  (R/make_protect_zone.R:62)
**Does:** builds a 0/1 Protect eligibility raster. It starts from low HII (or all PUs) and removes agriculture and built cells. **Used by:** elsar_pipeline, 1 call (R/05_zones.R:1092). The call passes every argument except `agriculture_lulc_value`, `built_area_lulc_value` and `make_locked_out`. It supplies `hii_threshold` (null → `hii_threshold_no_pa` = 4 when there are no PAs), `hii_quantile`, the shared `agriculture_threshold`/`built_areas_threshold`/`filter_patch_size`/`min_patch_size`, and `output_path` = the intermediates dir. It wraps the call in `withCallingHandlers()` and **greps elsar's message text** (`"HII threshold|using PA mean as threshold"`) to recover the effective cutoff. It then builds "Protect Zone with Lockin" (zone ∪ PA mask) itself. elsa_national: not used.
**Method:** `protect = HII < cutoff` (or all PUs if `human_pressure = NULL`), set to 0 where `built > t_b | ag > t_a`, then sieve. The cutoff is a fixed value, else the coverage-weighted `hii_quantile` of HII inside dissolved PAs, else (issue #79) the PA mean when that mean is >= 4. The q95-inside-PAs idea is sound and documented ("no more pressured than 95% of current PAs"). The #79 override is **not** in the roxygen docs, its constant 4 is hard-coded, and it makes the rule discontinuous.
**Verdict:** refactor: becomes a recipe on the shared zone builder with `exclude =` (#96), the cutoff returned as data, and the zone guaranteed to contain the lock-in on request.
**Issues:**
- **#79 rule is a cliff.** [ran] T3b: PA mean 3.904 → 369 eligible cells; PA mean 4.004 → 202 cells (-45%) for the same landscape shifted by +0.1 HII. A country just above 4 gets a much tighter Protect Zone than one just below. That also drives the "lock-in larger than zone" problem (unepwcmc/unbl-elsa#146). The constant 4 assumes the 0–50 Human Footprint scale. If a user passes a rescaled 0–1 HII, the rule never fires and q95 is used silently.
- **Agriculture and built exclusions cannot be switched off** (#96). [ran] T10: `built_areas` only, with no `lulc_raster` → `"If 'agricultural_areas' is NULL, 'lulc_raster' must be provided."`. Because of that assertion, the `else` branches at :258–266 (one or both inputs NULL, "Protection zone based on HII only") are **dead code**. `*_processed` can never be NULL.
- **`SpatVector` PAs fail** although they are documented and asserted as accepted. [ran] T2: 2-row `terra::vect` → `"subscript out of bounds"`, because `dplyr::summarise()` has no SpatVector method. A 1-row SpatVector goes straight to `exact_extract()` (UNVERIFIED whether exactextractr 0.10 accepts it).
- PAs are not transformed to the PU CRS before `exact_extract` ([code]). exactextractr reprojects with a warning, which `suppressWarnings` callers never see.
- HII NA is filled with 0 (`fill_na = 0`, the make_normalised_raster default) **before** the PA quantile and the `<` test ([code]). Cells with no HII data (coasts, water, gaps) count as "no pressure": they are eligible, and they pull the PA q95 and mean down. This is the threshold-side twin of A§2-#4.
- Pipeline coupling on message text: renaming the log line silently drops the cutoff from the pipeline's processing log.
- Docs: the `lulc_proportions` doc says it overrides `agricultural_areas`/`built_areas`, but the code gives the explicit rasters priority (`&& is.null(agricultural_areas)`); the same wrong sentence is in restore and manage. The "If NULL, make_protected_areas() will be used" claim is false (A§4 docs accuracy). The example omits the required `current_protected_areas`, so it fails. "Human Footprint Index (HII)" mixes two products.
- `make_locked_out` (invert to 1 - zone) is unused downstream. Locked-out logic belongs in the solve layer (elsa_national edits zone PU rasters).
- Known: thresholds on the rescaled LULC path (A§2-#3), the sieve re-adding exclusions (A§2-#7), ESRI codes assumed (A§2-#10).
**Improvements:**
- Expose the #79 rule as `hii_pa_mean_cap = 4` (NULL = off), document it, and replace the cliff with `cutoff = min(q95, max(cap, mean))` or simply `min(q95, cap)`. Agree the choice with the method owners. **S**
- `exclude = c("human_pressure", "agriculture", "built_areas")` per the #96 design (Cross-cutting). **M**
- Return the effective cutoff and source as `attr(x, "elsar_zone")` provenance, so the pipeline stops grepping messages. **S**
- `lockin = NULL` argument: when given, `zone <- zone | lockin` after sieving, so Protect always contains the PA lock-in. This replaces the pipeline's "Protect Zone with Lockin" block. **S**
- `sf::st_as_sf()` and `st_transform(crs(pus))` the PAs up front, so SpatVector works. **S**
- Deprecate `make_locked_out`. **S**
- Tests: no-HII path; PA-mean branch on both sides of 4; SpatVector input; exclusion of each criterion. Today only `test_empty_protected_areas.R` covers this function. **S**

### `make_restore_zone()`  (R/make_restore_zone.R:106)
**Does:** builds two 0/1 Restore layers: degraded-or-high-pressure land minus farms and towns (v1), and v1 restricted to forest (v2). **Used by:** elsar_pipeline, 1 call (R/05_zones.R:1267), when `restore_method = "standard"`. It passes all thresholds from the shared `zones:` block, and `degradation` = either the raw "Degraded Areas" raster or **the binarised output of `make_degraded_areas()`** when `compute_degraded_areas: true`. The pipeline has its own `custom_union` recipe (`build_union_zone()`, R/05_zones.R:825) as an escape hatch. elsa_national: not used.
**Method:** `v1 = (share(SDG == -1) > t_deg | HII >= 4) & ag <= t_a & built <= t_b`, `v2 = v1 & forest > t_f`, then sieve each layer. Treating degraded land and high pressure as restoration opportunity, minus land in active use, is a defensible rule. But HII >= 4 as "degraded" has no stated source, and nothing explains the asymmetry with Protect's PA-relative cutoff.
**Verdict:** fix then refactor: one silent wrong-result bug on a pipeline path, otherwise the same consolidation as Protect.
**Issues:**
- **The upper human-pressure cap is missing (design intent, confirmed by the maintainers in Nov 2026).** The Restore Zone is meant to target moderately degraded, still-restorable land and to exclude very high pressure. The 2021 tool documented "Restore: … IN Forest Biome AND HFP < 21". The code uses `HII >= human_pressure_threshold` (default 4) only as an *inclusion* criterion, with no upper limit (`R/make_restore_zone.R:289-293`). **Decided rule (Nov 2026):** `restore = (degradation > degradation_threshold | HII >= hii_min) & HII < hii_max & agriculture <= t & built <= t`, with `hii_min = 4` and `hii_max = 21` on the same 0–50 HII scale. Both the degraded-areas layer and the footprint identify degradation; the cap removes land too pressured to restore. Farmland and built-up land stay excluded: they belong to the Manage Zone, not Restore (decided Nov 2026). Value-changing, so it ships in 0.4.2. Test: a cell with HII 25 and no degradation is excluded; HII 10 is included; degradation 0.2 with HII 2 is included.
- **Pipeline `compute_degraded_areas: true` loses all degradation.** `make_degraded_areas()` returns 1 = degraded / 0 = not (R/make_degraded_areas.R:152), and the pipeline binarises it again. `make_restore_zone()` hard-codes `x == -1` (:204), so a 0/1 input contributes nothing. [ran] T4: SDG-coded input → 118 v1 cells; the same degradation as 0/1 → **0 cells**. Restore then reduces to `HII >= 4`, with no error or warning. Also note that `make_degraded_areas()` *counts* agriculture and built as degraded, while `make_restore_zone()` then *removes* them: the two functions encode contradicting definitions.
- The pipeline's config comment contradicts elsar. `config/defaults.yaml:174` says "Restore Zone **excludes** cells with human pressure (HII) at or above this value", but elsar **includes** them (`human_pressure_processed >= human_pressure_threshold` inside the OR).
- `human_pressure` and `forest_mask` are hard-required (asserted at :184–185) even when a team wants v1 only or has no HII. This is #96 for Restore.
- `degradation` is passed `method = "mean"` (:203). The argument does not exist and only works by partial matching onto `method_override` [code]. That breaks if `make_normalised_raster` gains another `method*` argument.
- v1 and v2 are re-run through `make_normalised_raster()` after being computed on the PU grid (:295, :305): two needless reprojections, plus a rescale that turns an all-1 zone into NaN (A§2-#2).
- Docs: `agriculture_lulc_value`/`built_area_lulc_value` are documented as "default: 5 / 7" but default to NULL. `degradation_threshold` is called a "proportion" but is compared with the rescaled share (A§2-#3). Example 3 duplicates example 2. The output file is `restore_zones_` (plural, unlike the others) and `built_areas_{iso3}.tif` collides with three other writers (A§4).
**Improvements:**
- `degradation_values = -1` argument (the values that mean "degraded"), plus a check that errors when no input cell matches and the input looks binary. The pipeline then passes `degradation_values = 1` for the computed path. **S**
- Decide the method: either `make_degraded_areas()` is the degradation definition (then Restore should not re-exclude ag/built differently), or retire it in favour of the Restore recipe. **M** (method decision)
- Drop the re-normalisation of v1/v2 (they are already on the grid); mask to PUs once. **S**
- `forest_mask = NULL` → return v1 only, or `variants = c("v1", "v2")`. `human_pressure = NULL` → degradation only. **S**
- Tests: none exist. Add SDG-coded input, binary input, and each exclusion. **S**

### `make_manage_zone()`  (R/make_manage_zone.R:68)
**Does:** builds two 0/1 Manage layers: moderate pressure or production land minus built (v1), and agriculture or pasture only (v2). **Used by:** elsar_pipeline, 1 call (R/05_zones.R:1318). It passes `managed_forests`, `pasturelands`, `human_pressure` (all possibly NULL), the shared `agriculture_threshold`/`built_areas_threshold`, `pasturelands_threshold`, `forest_class_threshold`, the shared patch settings and `output_path`. It does not pass `forest_classes` or `raster_mf`. elsa_national: not used.
**Method:** `v1 = (HII in [q20, q80] | mf > 0.1 | ag > 0.1 | pasture > 0.35) & !(built > 0.1)`; `v2 = ag > 0.1 | pasture > 0.35`. The "middle 60% of national HII" rule is relative, so its meaning changes with each country's pressure distribution. That is defensible for "already-used land", but undocumented as a choice. Using the same `agriculture_threshold` as an *inclusion* here and an *exclusion* in Protect/Restore means one config key does opposite things (the #96 issue text notes this too).
**Verdict:** fix then refactor: two crash or contract bugs on reachable inputs, plus dead parameters.
**Issues:**
- **`pasturelands = NULL` (the default) crashes.** [ran] T5: `unable to find an inherited method for function 'project' for signature 'x = "NULL"'`. The pipeline treats pasturelands as optional (it only adds "Pasturelands" to its input log when non-NULL) and passes NULL through, so a country without a pasture layer cannot build Manage.
- **v2 does not exclude built areas; v1 does.** [ran] T6: 11 cells with 90% built and 50% agriculture → v1 = 0, v2 = 11. The docs do not mention it; it looks unintended.
- **`forest_classes` is dead.** It is resolved and logged (:143–149) but never used: `make_managed_forests()` is called without it, and without `lulc_product`. It still **errors for `lulc_product = "local"`** when NULL, even when `managed_forests` is supplied, so local-LULC users must pass a value that is ignored.
- **The `raster_mf` path is unreachable.** :163 asserts `managed_forests` is a SpatRaster, so the `else` branch at :262 never runs. [ran] T7: `managed_forests = NULL, raster_mf = mf` → `"'managed_forests' must be a SpatRaster."`. Likewise `human_pressure` defaults to NULL but is asserted as required (:162).
- Pasture uses the default `rescaled = TRUE`, so `0.35` is compared with a rescaled value (A§2-#3 lists the pasture path). The HII quantile is computed on HII after `fill_na = 0` [code], so NA cells shift q20 down and count as "low pressure".
- `terra::global(fun = quantile)` pulls the whole raster into memory [code]. That is fine at 1 km, heavy at 100 m national grids (UNVERIFIED size limit).
- Docs: "An alternative zone includes is limited to"; `@return`/description say v2 is "agriculture only" but it includes pasture; "binary or categorical" inputs are not supported as categorical; the log says "is is".
**Improvements:**
- Make each inclusion criterion optional (NULL input → criterion dropped, with a message). This fixes the pasture crash and is the #96 `include =` design. **S**
- Exclude built from v2 (or document why not). **S**
- Remove `forest_classes` and `raster_mf`, or wire them to `make_managed_forests(raster_in = raster_mf, forest_classes = ..., lulc_product = ...)` with `managed_forests = NULL`. Deprecate whichever is dropped. **S**
- Expose `hii_probs = c(0.2, 0.8)`; compute the quantiles on the PU-masked, *unfilled* HII. **S**
- Tests: none exist. **S**

### `make_urban_greening_zone()`  (R/make_urban_greening_zone.R:49)
**Does:** `built > threshold` (default 0) on the PU grid, optionally sieved. **Used by:** elsar_pipeline, 1 call (R/05_zones.R:1361). It passes `built_areas_threshold = zones$urban_greening_built_threshold %||% 0` (its own key, so the shared key cannot shadow it), but **`filter_patch_size = zones$filter_patch_size %||% FALSE` and `min_patch_size = zones$min_patch_size %||% 10` use the shared keys**. A country that sets `filter_patch_size: true` for Protect/Manage therefore also sieves urban greening, which is exactly the shadowing that the threshold comment says was fixed. elsa_national: not used.
**Method:** urban greening is possible wherever any built-up share exists. That is sound and documented ("urban areas are small and should be preserved").
**Verdict:** merge into the shared zone builder; keep the exported name as a thin recipe wrapper.
**Issues:**
- About 110 of its 160 lines duplicate the other builders: `lulc_proportions` unpacking, class-value lookup, alignment, intermediate writes.
- An NA in the built input becomes 0 → "not urban" ([ran] T9: 0 NA cells inside PUs). That is fine here, but it is the same silent fill as in the other builders.
- `built_areas_{iso3}.tif` is written by this function too (collision, A§4).
**Improvements:** a recipe on the shared builder (`include = list(built_areas = crit(">", 0))`) (**S** once the builder exists). In the pipeline, give urban greening its own `zones.urban_greening.filter_patch_size` key (**S**, pipeline side).

---

## ELSA utilities

### `get_target()`  (R/utils-elsa.R:113)
**Does:** converts a percentage into a cell (or PU-value) count: `round(sum(pu) * target / 100)`, floored to `1e-4` when 0. **Used by:** internal `.elsar_calibration_problem()` (:425, the per-zone `<=` caps). elsar_pipeline: none. elsa_national: its own copy (`R/utils.R:278`, used at `define_prioritization_problem.R:120`).
**Method:** sound for a `<=` area cap on 1/NA PUs. The `1e-4` floor is justified in the docs as "avoid zero targets in optimisation", a relic of `>=` targets. For a `<=` cap on binary variables, 0 and 1e-4 are identical, so the floor is a no-op that only obscures the result. `round()` is half-to-even: [ran] 2.5 cells → 2.
**Verdict:** fix (small), keep exported, since it is the shared contract with elsa_national.
**Issues:**
- **Drift from elsa_national:** the argument is `pu` here and `PU` there; national has no validation and no floor; the docs differ. The behaviour is identical for `<=` caps, so this is not a numeric drift, but a call written as `get_target(PU = x, ...)` breaks against elsar.
- A vector `target` errors with an R internals message ([ran]: `'length = 2' in coercion to 'logical(1)'`).
- It always logs ("Calculated target: …"), so `elsar_calibrate_weights(verbose = FALSE)` still prints one line per zone [ran].
- Percent, while the audit wants fractions everywhere (A§4 units). A fraction passed by mistake (`0.3`) gives a 0.3% cap without warning.
**Improvements:** drop the floor (or keep it only for a `sense = ">="` use); vectorise; add `verbose`; document "count of PU cells (or summed PU value)". Make it the single implementation and let elsa_national import it once elsar is tagged (national CLAUDE.md allows the one-way import). Fractions: add a new `budget_cells(pu, fraction)` instead of changing units under the old name. **S**

### `get_min_lockin_target()`  (R/utils-elsa.R:17)
**Does:** raises the `zone_N_target` budgets to at least each lock-in's coverage. **Used by:** nobody. Not internal (calibration re-implements the same logic inline at R/calibrate_weights.R:409–416), not the pipeline. elsa_national has its **own, different** version (`R/get_min_lockin_target.R`): name-based, returns a list.
**Method:** sound idea (budget >= locked-in area). The implementation is a Shiny-era positional interface (`zone_1_target` … `zone_4_target`, `min_coverage[1..4]`).
**Verdict:** deprecate, and replace it with a name-based helper shared with calibration and elsa_national.
**Issues:**
- **It silently returns a 0-row tibble for name-keyed input.** [ran] `input = list(protect = 10, restore = 5, manage = 5)` → `# A tibble: 0 × 3`, with no error. This is the shape elsa_national and calibration use.
- A "list of rasters" is documented and asserted as accepted, but [ran] fails inside `terra::global` (`signature 'x = "list"'`).
- Zones are matched by layer position, not name; the log line omits zone 4; `ifelse` is used on scalars.
**Improvements:** `elsar_lockin_budgets(lockins, budgets, pu)` takes named lists or vectors and returns a named numeric (adopt elsa_national's version). Use it inside `.elsar_calibration_problem()`. `lifecycle::deprecate_soft()` the old function. **S**

### `get_coverage()`  (R/utils.R:189)
**Does:** `sum(zone) / sum(pu) * 100`. **Used by:** elsar_pipeline, 1 call (R/06_output.R:653 `report_zone_coverage()`, reached from run_pipeline.R:344), on 0/1 zones within PUs. Internal: `get_min_lockin_target`, `calculate_restore_and_urban_budgets`, calibration lock-in floors. elsa_national: identical copy (`R/utils.R:251`, longer docs only, no numeric drift).
**Method:** correct for 0/1 zones masked to the PUs. It neither masks the zone to the PUs nor checks binarity.
**Verdict:** fix (small), keep.
**Issues:** [ran] a zone covering cells outside the PUs → **105.26%**; a 0.5-valued zone → 52.6%, i.e. "coverage" means summed value. The docs say "binary" but nothing checks it. It works on multi-layer stacks (returns a vector) but the docs do not say so.
**Improvements:** `terra::mask(zone, pu)` before summing (or count `zone > 0 & !is.na(pu)`); a warning for non-0/1 values; document the multi-layer return; `as = c("percent", "fraction")`. **S**

### `calculate_restore_and_urban_budgets()`  (R/utils-elsa.R:164)
**Does:** reads three rasters by file name and returns 30% of restore-zone and urban-area coverage as budget percentages. **Used by:** nobody (pipeline, national and internal all checked).
**Method:** "budget = 30% of the zone's extent" is a policy heuristic, not a method, and it is undocumented: why 30%, and why the urban-*areas* layer rather than the urban-greening zone?
**Verdict:** deprecate.
**Issues:** it reads `restore_zone_v1.tif` / `urban_areas.tif` / `planning_units.tif`, which nothing writes (A§4: covered). [ran] it works only when the files are hand-named. `iso3` is validated but used only in a log line. It does file I/O inside a numeric helper.
**Improvements:** `lifecycle::deprecate_soft()`; point users to `get_coverage(zone, pu) * fraction`, or a raster-taking `zone_budget(zone, pu, fraction = 0.3)` if the heuristic is worth keeping. **S**

---

## Calibration

### `elsar_calibrate_weights()`  (R/calibrate_weights.R:101)
**Does:** iteratively finds per-feature weights that equalise each feature's max-utility representation relative to its single-feature maximum. **Used by:** elsar_pipeline, 1 call (R/07_calibration.R:1287). It passes `features, impacts, zones_pu, planning_units, budgets`, plus `wgt_scale, gap, solver, threads, time_limit, freeze_tol, freeze_patience, max_iter, progress_file, verbose` from `config$calibration`. It does **not** pass `lockins`, `feature_totals` or `solve_retries`. It converts zone rasters to 1/NA itself (R/07_calibration.R:56). The pipeline writes the sidecar (`write_feature_weights_sidecar()`), which elsa_national reads (`read_feature_weights()`, `calibration_status()`).
**Method:** documented clearly in roxygen and the vignette: a_max per feature (n solves), an equal-weight baseline, then an additive increase-only update `adj = s - (delta - min)/(max - min) * s`, stopping when the allocation freezes, and returning the best-spread weights. It matches the deployed formulation. elsa_national puts impacts in the weights matrix (D15), elsar bakes them into zone features; for max utility both give the same objective, and elsar's `absolute_held / raw total` equals national's "effective" representation. The per-zone `<=` caps, mandatory allocation and `do_nothing` last also match national. **Lock-ins do not match** (below).
**Verdict:** fix: the lock-in path calibrates the wrong problem, and the zone-mask contract is unchecked.
**Issues:**
- **PA lock-ins are not enforced, although the ELSA pre-calibration method locks PAs into Protect.** The code raises a zone's cap to the lock-in coverage but never forces the locked-in cells into that zone. `do_nothing` is built internally from all PUs (`planning_units * 0`, :379), so callers cannot remove lock-in cells from it either. [ran] `t_calib.R`: protect budget 10%, 20% lock-in on cells holding no feature value → cap raised to 20 cells, and **0 of 20 lock-in cells allocated to protect**. The solver gets twice the free protect budget. *History (checked in elsa_national's git log):*
  - The original pre-calibration locked PAs in: `pre_global.R` (2021) and `pre_run.R` (Feb 2025) both call `add_locked_in_constraints(PA, PA0, ...)`.
  - The Dec 2025 rewrite (`calibrateSetup.R`) loaded `by_zone_locked` but passed the unlocked `by_zone`. From then on the lock-in was only a budget floor, through the final local version (May 2026).
  - `elsar_calibrate_weights()` (Jul 2026) inherited that behaviour, and documents it accurately ("feasibility floor").
  - `elsar_pipeline` passes PA lock-ins with the comment "elsar forces those cells into the zone" (`R/07_calibration.R:51-53`). It does not.

  **So every calibration since Dec 2025 has fitted weights to a problem without the PA lock-in that the method, the ELSA tool's solve and the pipeline all assume.**
- **0/1 zone masks are accepted silently and ignore eligibility.** The documented contract is 1/NA, but every elsar zone builder returns 0/1. [ran] `t_calib2.R`: a protect mask with 10 eligible cells (90 zeros) and a 50% cap → **100 cells allocated to protect**. The zeros are available at zero cost and also count 0 toward the cap. The pipeline converts; a direct user following the elsar vignette with builder outputs would not.
- The default `feature_totals` sums features over the whole raster, not the PUs (:179–181). elsa_national defines the denominator as "raw totals masked to the PUs". It is identical only while national's `check_within_planning_units()` holds; elsar does not check.
- `meta` lacks the elsar version, budgets, impacts, lock-ins, PU count/hash and a timestamp. The pipeline rebuilds `calibrated_for` from its config (`calibration_provenance()`), so the sidecar contract (CSV `feature, file_name, weight_calibration, a, a_max, ratio`; JSON `calibrated_for`) lives in two private or downstream repos and none of it is in elsar (national P3.9).
- `verbose = FALSE` still prints: `get_target()` logs once per zone, and the retry message is unconditional (:468) [ran/code].
- Performance: n_feat + iterations full solves, each recompiling the problem (`add_feature_weights` + `solve`). [ran] 0.64 s per solve on 100 PUs, i.e. overhead-dominated. The a_max solves are independent and could run in parallel. UNVERIFIED at national scale.
- An edge case: if no feature has a finite delta in an iteration, `max()` over an empty vector gives -Inf with a warning (:286), and `max_change` is NA when a feature is missing from the summary [code].
- Tests (3) cover a balanced run, a zero-total feature and validation; none covers lock-ins or mask values.
**Improvements:**
- Restore the original method: enforce lock-ins in the internal problem with `prioritizr::add_locked_in_constraints()` per zone (as `pre_run.R` did), or edit the zone stack as the ELSA tool does: add lock-in cells to their zone and remove them from every other zone, including `do_nothing`. Then raise caps via `elsar_lockin_budgets()`. Add a test that lock-in cells are allocated to their zone. Calibrations since Dec 2025 should be rerun after the fix. **S–M**
- Validate `zones_pu` and `planning_units`: error (or convert, with a message) when values other than 1/NA appear. **S**
- Default `feature_totals` masked to `planning_units`. **S**
- `meta$calibrated_for = list(budgets, impacts, lockin_coverage, zones, n_pu, elsar_version, time)`. Add `elsar_write_calibration(cal, dir, iso3, file_names)` / `elsar_read_calibration()` with `schema_version`, so elsar owns the sidecar contract (national P3.9) and the pipeline and national both call it. **M**
- Thread `verbose` into `get_target()` and the retry message; add `print.elsar_calibration()`. **S**
- Optional `parallel_amax = TRUE` (future.apply) for step 1. **M**

### `.elsar_calibration_problem()` (R/calibrate_weights.R:368), `.elsar_zone_representation()` (:444), `.elsar_traj_row()` (:496)
**Does:** builds the weightless zoned problem / solves and sums representation / builds one trajectory row. **Used by:** internal only.
**Method:** the problem duplicates elsa_national's `define_prioritization_problem()` (objective, caps, mandatory allocation). The representation sums `absolute_held / raw total` across zones, which matches national's effective measure.
**Verdict:** fix (`.elsar_calibration_problem`, for the lock-in and mask issues above); keep (the other two).
**Issues:** inline lock-in floor logic duplicates `get_min_lockin_target`. The muffled-warning hack depends on prioritizr's exact message text ("no non-missing arguments to min"); a prioritizr wording change brings the warning back (harmless). `retries` uses a fixed 5 s sleep.
**Improvements:** a shared exported `elsar_problem(features, impacts, zones_pu, pu, budgets, lockins)` that elsa_national could also import, so calibration and deployment cannot drift (**M**, needs agreement with national D15: impacts in features vs in weights).

---

## LULC config

### `get_lulc_classes()`  (R/lulc_config.R:41)
**Does:** returns a hard-coded named list of class codes for one of three products. **Used by:** internal (`get_lulc_class_value`, gee_helpers.R:2055). elsar_pipeline / elsa_national: not called directly.
**Method:** DW and ESA WorldCover codes match the published legends (checked from memory of the GEE catalogue; UNVERIFIED here). ESRI 1/2/4/5/7/8/9/10/11 are correct.
**Verdict:** refactor into a product registry (A§5 "product registry with legends and nodata"), keeping this function as an accessor.
**Issues:**
- **The names are not standardised**, although the docs promise "standardized class mappings": `rangeland` (ESRI) vs `grass` (DW) vs `grassland` (ESA); `bare_ground` vs `bare_sparse`; `shrub_scrub` vs `shrubland`; ESA has both `tree_cover` and `trees`. [ran] Only `agriculture, built_area, water, snow_ice, trees, forest_*` are common to all three. A product-agnostic call for "grassland" fails on two of three.
- `forest_managed` for DW (1) and ESA (10) is **all tree cover**. If applied to a DW/ESA raster, every forest becomes "managed". The ESRI entry holds Lesiv forest-management codes (A§2-#11: covered). The comment labels 40 = agroforestry and 53 = silviculture; in Lesiv et al. 2022, 40 = oil palm and 53 = agroforestry (UNVERIFIED against the source). `forest_all = c(2, 20, 31, …)` mixes ESRI and Lesiv codes in one vector.
- There is no `local` entry, but `list_lulc_products()` and all four zone builders list `"local"` [ran]: `get_lulc_classes("local")` → `'arg' should be one of …`. There are no nodata values (ESRI/ESA use 0) or product versions.
- The product list is hard-coded 15 times across R/ (zone builders, `make_managed_forests`, `make_urban_greening_opportunities`, gee_helpers ×4, lulc_config ×5).
**Improvements:** a single internal `.lulc_registry` data frame (`product, class_key, code, label, nodata, version, source`) with `forest_management` as its own product (A§2-#11). Standard keys enforced by a test. Every `match.arg` vector taken from `names(registry)`. A reverse lookup `lulc_legend(product)` for the "values fit the legend" check (A§2-#10). **M**

### `get_lulc_class_value()`  (R/lulc_config.R:134)
**Does:** looks up one class code. **Used by:** internal, in all four zone builders, `make_managed_forests`, `make_urban_greening_opportunities` and gee_helpers (×2). Not used downstream.
**Method:** trivial and correct.
**Verdict:** keep, as a registry-backed accessor.
**Issues:** product validation is duplicated (its own list plus `match.arg` inside `get_lulc_classes`). `"local"` gives `Invalid product 'local'`, though "local" is a valid `lulc_product` everywhere else. That error is avoided only because each builder checks "local" first, with four copies of that check.
**Improvements:** accept `"local"` with a classed error that says which `*_lulc_value` argument to supply; vectorise `class_name`. **S**

### `list_lulc_products()`  (R/lulc_config.R:179)
**Does:** returns a tibble of product metadata. **Used by:** nobody (internal, pipeline and national all checked). Its only use is the example.
**Method:** n/a. The data duplicate the ids held in `get_lulc_classes()` (two sources of truth; "local" exists only here).
**Verdict:** keep exported, but generate it from the registry (the user-facing discovery function).
**Issues:** WorldCover is labelled "Static (2021 baseline)" although v100 (2020) also exists; no version or nodata columns.
**Improvements:** derive it from `.lulc_registry`; add `version`, `nodata`, `classes` (list column). **S**

---

## Cross-cutting

### 1. Duplication in the zone builders (measured)
Of 1,145 lines in the four files, these blocks are copy-pasted:
- `lulc_proportions` unpacking: 4 × ~30 lines.
- class-value resolution with the "local" checks: 4 × ~20.
- ag and built alignment: 3 × ~45.
- intermediate writes: 4 × ~15.

That is about 450 lines, 40%, before roxygen. The copies have already drifted:
- urban greening unpacks `lulc_proportions` only when `built_areas` is NULL; the others always do;
- restore asserts `iso3`/`pus` types; protect does not;
- protect writes intermediates with explicit `datatype = "FLT4S"`; restore does not;
- restore writes intermediates *before* building the zone, the others after.

Run together, the pipeline aligns agriculture 3×, built 4× and HII 3× per country (and restore and manage re-align their own outputs again). [ran] Each aligned-input `make_normalised_raster` call costs about 0.9 s per 10⁶ cells even when nothing needs warping; on raw 10 m LULC the cost is the crop, project and resample, which is much larger (UNVERIFIED magnitude).

### 2. Proposed shared zone builder

```r
# Aligned once, rescaled = FALSE for every thresholded input (fixes A§2-#3 by construction),
# NA left as NA (not 0) so each criterion decides how to treat missing data.
zi <- zone_inputs(
  pus,
  human_pressure = hii,
  lulc_proportions = props,           # or agriculture = , built_areas = , lulc = , lulc_product =
  pasturelands = pasture, managed_forests = mf,
  degradation = sdg, degradation_values = -1,
  forest = forest_mask
)                                     # class "elsar_zone_inputs": named list of aligned layers + provenance

# One criterion = one input + comparison + value
crit <- function(input, op = c(">", ">=", "<", "<=", "between"), value, na = c("exclude", "include"))

# Generic builder: zone = (any include, or all PUs if none) AND NOT (any exclude),
# masked to PUs, sieve on the 1-class only (A§2-#7), lock-in OR-ed in last.
elsar_build_zone(
  zi,
  include = list(),                   # named list of crit()
  exclude = list(),
  min_patch_size = NULL,              # NULL = no sieve
  lockin = NULL,                      # SpatRaster 0/1: guaranteed in the zone
  name = "zone"
)
# returns a 0/1 SpatRaster with attr(, "elsar_zone") = data.frame(
#   criterion, role = include/exclude, op, value, cells_hit), which the pipeline logs instead of grepping messages
```

The four exported builders become recipes of about 20 lines each, with their current names and argument names kept:

```r
make_protect_zone <- function(iso3, pus, current_protected_areas, ..., hii_threshold = NULL,
                              hii_quantile = 0.95, hii_pa_mean_cap = 4,
                              exclude = c("human_pressure", "agriculture", "built_areas"),
                              agriculture_threshold = 0.1, built_areas_threshold = 0.1,
                              min_patch_size = 20, lockin = NULL, inputs = NULL, output_path = NULL) {
  zi  <- inputs %||% zone_inputs(pus, ...)                    # reuse across builders
  cut <- protect_hii_cutoff(zi$human_pressure, current_protected_areas,
                            hii_threshold, hii_quantile, hii_pa_mean_cap)
  ex  <- list(human_pressure = crit("human_pressure", ">=", cut),
              agriculture    = crit("agriculture", ">", agriculture_threshold),
              built_areas    = crit("built_areas", ">", built_areas_threshold))
  elsar_build_zone(zi, exclude = ex[.check_exclude(exclude, names(ex), zi)],
                   min_patch_size = min_patch_size, lockin = lockin, name = "protect_zone")
}
```

Restore: `include = {degradation > t, human_pressure >= 4}`, `exclude = {agriculture, built_areas}`; v2 adds `include` AND `forest > t` (a second call with `exclude` extended by `crit("forest", "<=", t)`). Manage: `include = {hii between q20,q80, managed_forests, agriculture, pasturelands}`, `exclude = {built_areas}`. Urban greening: `include = {built_areas > 0}`.

Writing outputs moves to the caller or to `elsar_write()` (A§4 "no writes inside builders"). Keep `output_path` for one deprecation cycle.

Net effect: about 1,145 → 450 lines. Every eligibility rule sits in one table that a "How zones are built" article can be generated from (the table at the top of this file). The pipeline passes one `zone_inputs` object to all four builders.

### 3. Issue #96: concrete design
Rules (one per zone, same semantics everywhere):
- `exclude` is a character vector naming the exclusion criteria to apply. The default is today's behaviour, so the change is non-breaking:
  - protect `c("human_pressure", "agriculture", "built_areas")`
  - restore `c("agriculture", "built_areas")`
  - manage `"built_areas"`
- `include` (restore, manage) names the inclusion criteria:
  - restore `c("degradation", "human_pressure")`
  - manage `c("human_pressure", "managed_forests", "agriculture", "pasturelands")`
- An input is required **only if a criterion that uses it is selected**. The assertions at protect :172/:196, restore :184–191 and manage :157/:162–163 become per-criterion checks with the message `"exclude = 'agriculture' needs agricultural_areas, lulc_proportions$agriculture or lulc_raster"`.
- An input supplied but not used by any criterion gives a message, not an error.
- An unknown name errors and lists the allowed set.
- Back-compat: `human_pressure = NULL` in protect still drops `"human_pressure"` from `exclude`, with a message. The "threshold = 1 disables it" workaround keeps working but is documented as superseded.
- Guardrail: if the resulting zone has no criteria (protect with `exclude = character(0)`), warn `"Protect zone is all planning units"`; the pipeline errors in `strict_data` mode.
- Provenance: the criteria actually applied, with their values and cells affected, go in `attr(z, "elsar_zone")`. The pipeline writes them to the processing log and the run metadata, as #96 asks.
- `zone_thresholds(zone)` returns the documented per-zone defaults (thresholds, patch size, include/exclude), so the pipeline config can be validated against one source.

```r
zone_thresholds("protect")
#> $exclude        "human_pressure" "agriculture" "built_areas"
#> $agriculture_threshold 0.1 ; $built_areas_threshold 0.1
#> $hii_quantile 0.95 ; $hii_pa_mean_cap 4 ; $min_patch_size 20

# Netherlands: keep HII and built exclusions, let farmland into the Protect Zone
pz <- make_protect_zone("NLD", pus, pas, human_pressure = hii, lulc_proportions = props,
                        exclude = c("human_pressure", "built_areas"),
                        built_areas_threshold = 0.3, lockin = pa_mask)
attr(pz, "elsar_zone")
#>        criterion    role op value cells_hit
#> 1 human_pressure exclude >=  6.20     41210
#> 2    built_areas exclude  >  0.30      8120
#> 3         lockin include  =     1      2210
```

Pipeline side (S, after elsar ships): per-zone blocks that fall back to the shared keys:

```yaml
zones:
  agriculture_threshold: 0.1          # shared fallback
  protect:  { exclude: [human_pressure, built_areas], built_areas_threshold: 0.3 }
  urban_greening: { filter_patch_size: false }   # also fixes the shared-key shadowing
```

It should also stop returning NULL for Protect when HII is missing (R/05_zones.R:911), now that `exclude` makes that explicit.

### 4. Other cross-cutting findings
- **Zone value convention.** Builders return 0/1; calibration (and prioritizr) need 1/NA, and elsar never converts or checks ([ran] t_calib2: eligibility silently ignored). Pick one convention: builders keep 0/1, with `as_pu_mask(zone)` exported and used inside `elsar_calibrate_weights()`.
- **Lock-ins are handled in three places and three ways:**
  - the pipeline ORs PAs into "Protect Zone with Lockin";
  - elsa_national edits the zone stacks and raises budgets;
  - elsar's calibration raises caps only.

  One elsar helper set fixes all three: `lockin =` on the builders, `elsar_lockin_budgets()`, and lock-in constraints in calibration.
- **elsa_national drift:** `get_coverage` is identical; `get_target` differs in its argument name (`PU`) and the 1e-4 floor, but not numerically for `<=` caps; `get_min_lockin_target` is a different, name-based design, and national's version is the one to keep; `log_message` is equivalent. Once elsar is tagged, national should import `get_target`, `get_coverage` and the new `elsar_lockin_budgets` and the calibration reader.
- **Tests:** none exist for restore, manage, urban greening, `get_target`, `get_coverage`, `get_min_lockin_target`, budgets or `lulc_config`. Protect has only the empty-PA test. The shared builder makes one parametrised test file across all four recipes cheap (S–M).

### Verdict counts (13 cards)
- keep: 3 (`get_lulc_class_value`, `list_lulc_products`, `.elsar_zone_representation`/`.elsar_traj_row` counted as one)
- fix: 5 (`make_restore_zone` [fix then refactor], `make_manage_zone` [fix then refactor], `get_target`, `get_coverage`, `elsar_calibrate_weights` incl. `.elsar_calibration_problem`)
- refactor: 2 (`make_protect_zone`, `get_lulc_classes`)
- merge into shared zone builder: 1 (`make_urban_greening_zone`)
- deprecate: 2 (`get_min_lockin_target`, `calculate_restore_and_urban_budgets`)
- make internal: 0

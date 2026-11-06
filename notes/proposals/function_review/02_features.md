# elsar per-function review 02: feature layer builders

Scope: `develop` at f42e0df (0.4.1) in `/home/scottca/gitrepos/elsar-audit`;
downstream `elsar_pipeline` at 1fd6f43. Known findings are cited from
`notes/proposals/2026-11_audit_and_roadmap.md` as "audit §N #M" and not
re-argued. Repro scripts are in
`(scratch, not committed)/t02{a,b,c}.R`
(synthetic 10x10 grid, 1 km cells, ESRI:54009). "[ran]" means I reproduced
the finding with those scripts. "[code]" means it follows from the code but I
did not run it. UNVERIFIED means it depends on external data I could not check.

Pipeline call sites are in `R/03_features_default.R` (FD),
`R/04_features_nondefault.R` (FN), `R/05_zones.R` (Z) and
`R/utils_ecosystems.R` (UE). The pipeline never passes `output_path` to any
of these builders except `make_degraded_areas`. It writes the stack itself,
so the in-builder writes matter only to standalone users.

---

### `make_normalised_raster()`  (R/make_normalised_raster.R:89)
**Does:** aligns a raster to the PU grid, optionally applies pre- and post-alignment expressions, fills NA, inverts, rescales to 0-1 and writes a COG.
**Used by:** elsar_pipeline (8 calls). FD:644 is the generic path for every "default" feature, with only `raster_in, pus, iso3` and optionally `conditional_expression` from `build_normalizer()`. FN:625-669 handles generic vector features the same way. Z:786 uses `rescaled = FALSE` plus `input_raster_conditional_expression` for inclusion layers. The pipeline never uses `invert`, `method_override`, `fill_na`, `output_path` or `name_out`. Internally it is called by 20+ builders, `exact_rasterise()` and the plot helpers.
**Method:** it calls `terra::project()` onto the PUs (bilinear, or near for factors), then `exact_resample()` on the already-aligned grid, which does nothing (audit §2 #1). It then masks NoData sentinels, applies the post expression, fills NA with 0 and masks to the PUs, negates if `invert`, and finally does a min-max rescale over the country (audit §2 #2 constant → NaN; #4 fill before invert). The "normalisation" is therefore a per-country min-max stretch, so values are not comparable between countries or runs. Nothing in the docs says so.
**Verdict:** refactor: split into `align_to_pus()` + `normalise()` (audit §6.4). The current function hides at least three independent decisions behind one call.
**Issues:**
- **The "empty layer" short-circuit runs before the post-expression.** Lines 183-187 return all-0 when `max == 0`, which is tested before `conditional_expression` runs. Any layer whose informative values are ≤ 0 is lost. The pipeline's own legacy normalisers for "Crop Suitability Difference" and "Agricultural Climate Stress" (`ifel(r > 0, 0, -r)`, FD:501-502) are exactly that case. [ran] t02b T1b: input 0…-10 with that expression returns min = max = 0. The expected output is 0-1.
- **The early return also skips the write and the naming.** With `output_path` set and an all-0 input, nothing is written and no error is raised. [ran] t02a T2: `list.files(td)` returned `character(0)`. The returned layer is named "Planning Units" (inherited from `pus`), not `name_out`.
- `invert = TRUE` with `rescaled = FALSE` returns negated values (−x). That is not an inversion in any useful sense, and nothing documents it [code, line 232].
- The pre-aggregated path (lines 134-162) applies `input_raster_conditional_expression` *after* `terra::project()`. For a mode-aggregated integer class raster that is not a factor, `project()` defaults to bilinear and blends class codes at any grid mismatch. Equality tests on class codes then fail [code; effect UNVERIFIED because it needs a non-identical grid].
- The INT1S writer branch (lines 244-253) is practically dead. After `exact_resample()` the data are always float (`is.int` FALSE, [ran] t02a T3), so integer layers are always written FLT4S. The function also uses its own `writeRaster(COG)` call instead of `save_raster()`, which every other builder uses, so it writes with different options and a different NAflag.
- The docs are wrong in several places: `pus` is documented as a `SpatVector`; `method_override` claims to control `terra::project()` and accepts 11 `exact_resample` methods that are all ignored; the example does not parse (audit §4). `crop_global_input` only applies when a pre-expression is given, and the docs do not make that clear.
- Tests: only two tests (`test_make_normalised_raster.R`), both of which check that min = 0 and max = 1 on bundled data. No test covers `fill_na`, `invert`, `method_override`, the expressions or the empty path.
**Improvements:**
- Move the `max == 0` check after the post-expression, or drop it and let the constant-layer rule (audit §2 #2) handle all-0 layers. Always write when `output_path` is set. Set `names(out) <- name_out %||% names(raster_in)`. **S**
- New signature in 0.6: `align_to_pus(x, pus, type = c("continuous","proportion","categorical"), fun)` → `normalise(x, invert, rescale = c("minmax","none","fixed"), range = NULL, fill = 0)`. A `range =` option (fixed min/max) makes layers comparable across countries and runs. Keep `make_normalised_raster()` as a wrapper. **M**
- Replace the two expression hooks with `pre = ` and `post = `, keeping the old names as soft-deprecated aliases. **S**
- Write via `save_raster()` only. Choose the datatype from the values after normalisation (INT1U for {0,1}, FLT4S otherwise). **S**
- Tests: all-0, constant, ≤0 plus post-expression, invert with and without rescale, a NoData sentinel, `output_path` written, layer name. **S**

### `make_protected_areas()`  (R/make_protected_areas.R:69)
**Does:** filters WDPA (via wdpar) or a user `sf` by STATUS/SITE_TYPE/MAB, buffers points, dissolves, and returns PU coverage, binarised at `threshold`.
**Used by:** elsar_pipeline, 1 call through `build_protected_areas()` (Z:286), which serves the feature, the Protect Zone and the lock-in. That call passes every argument explicitly: `binary = TRUE`, `threshold = pa$coverage_threshold` (0.1 in defaults.yaml:68), `return_sf = TRUE`, `output_path = NULL`, `site_type = "PA"`. Internal callers: `make_protect_zone()`, `elsar_check_setup()`, plot utils.
**Method:** the filter matches Protected Planet practice (Designated/Inscribed/Established, no MAB, points buffered by REP_AREA in km² with `area_multiplier = 1e6`, which is correct here, unlike KBAs). It dissolves before coverage, so overlaps are not double-counted. The output is "PU is protected if more than 10% of it is covered". That is a sensible lock-in rule, but it overstates protected *area* when the layer is used as a feature, and the docs do not say which use it is meant for. `binary = FALSE` is rescaled, not raw coverage (audit §2 #12). Fully protected countries give NaN (audit §2 #2).
**Verdict:** fix: the method is sound and the function is well guarded (empty-PA handling and the pre-dissolve crop are good). A few small consistency fixes remain.
**Issues:**
- Whether polygons with `REP_AREA` 0/NA are kept depends on whether any point exists in the input. With `buffer_points = TRUE` and at least one point, `convert_points_polygon()` keeps only polygons with `REP_AREA > 0` (utils.R:131-133). With no points, the polygon branch (line 250) keeps all of them. So adding a single point record to a country silently removes its zero-area polygons. This extends audit §2 #12 (`convert_points_polygon` drops polygons) [code].
- The `buffer_points = FALSE` branch (lines 242-246) does not dissolve. Overlapping PAs are then coverage-summed by `coverage_fraction()` per feature, and only the first layer `[[1]]` is used (line 272). The raster therefore comes from **the first feature only**. [ran] t02d: two disjoint 3x3-cell PAs plus one point, with `buffer_points = FALSE` → 9 protected cells. The same polygons on the dissolving path → 18. This happens whenever the input contains any point and the user disables buffering.
- The return type depends on `from_wdpca` (`return_sf = from_wdpca`), so the same call returns a raster or a list depending on the data source (audit §4 return types).
- `status` is validated, but `threshold` is not checked to lie in [0, 1). `n_quad_segs` here is `nQuadSegs` in KBAs and Ramsar (audit §4 glossary).
- The custom-layer fallback ("no STATUS column → keep all") is good, but the user is told only through `log_message`. A `custom PA layer` warning would be more visible.
**Improvements:**
- Dissolve in the `buffer_points = FALSE` branch too, or better, route all branches through one `vector_coverage()` (audit §6.4). **S**
- Apply the area filter only to points. Never drop polygons for a missing `REP_AREA` (fix in `convert_points_polygon`). **S**
- Return one type: always a list, or a raster with an `attr(, "sf")`. Soft-deprecate `return_sf`. **M**
- Add `coverage = c("binary","fraction")` and return raw coverage (not rescaled) for `"fraction"`. Document "PU counted as protected if coverage > threshold". **S**
- Tests: points-only with no area (exists), mixed points and zero-area polygons, `buffer_points = FALSE` with overlaps, full coverage → 1 not NaN. **S**

### `make_kbas()`  (R/make_kbas.R:34)
**Does:** filters KBAs by country, AZE status and class, buffers points, dissolves, and returns rescaled PU coverage.
**Used by:** elsar_pipeline, 2 calls (FN:192 KBA with `include_aze_sites = !include_aze`; FN:240 AZE with `aze_only = TRUE`). Both pass `iso3 = base_iso3`. The input is pre-filtered by `iso3` in the loader. Not used internally.
**Method:** coverage of the dissolved KBA polygons, then a min-max rescale. Coverage max is usually 1, so this is close to raw coverage, but a country whose KBAs never fully cover a PU gets stretched values. Point buffer area is 100x too small (audit §2 #5). The `iso3` filter falls back to the argument when the column is missing (audit §2 #12).
**Verdict:** fix. Merge its geometry half into `vector_coverage()` later.
**Issues:**
- **Three empty-result paths return before the write.** Line 80 (no KBAs after AZE exclusion), and lines 70, 93 → 97-98 (`if (inherits(kba, "SpatRaster")) return(kba)`), all return before the `output_path` block. Only the "no features at all" path writes a file. [ran] t02b T9: AZE-only input with the default `include_aze_sites = FALSE`, and regional-only input, both returned with no file in `output_path`.
- The docs contradict the defaults. The description says "By default, it includes all KBAs except regional-only sites", but the default `include_aze_sites = FALSE` excludes AZE sites (line 75).
- The file names are inverted. Excluding AZEs writes `kba_aze_sites_{iso3}.tif`, and including them writes `kba_sites_{iso3}.tif` (lines 163-168).
- `aze_only = TRUE` ignores `include_regional_kba`, which is undocumented.
- The function does `st_transform` → `summarise()` → `st_make_valid()` (lines 147-150): it unions *before* repairing. `make_protected_areas()` (lines 221-225) documents that this ordering throws GEOS TopologyExceptions after reprojection [code].
- `area_crs = sf::st_crs(pus)` buffers in the PU CRS. That is correct only because the PUs are equal-area, which nothing asserts.
**Improvements:**
- One exit point. Build the empty raster via a helper, write once, name the layer. **S**
- `st_make_valid()` before `summarise()`. Fix the docs and the file stem (`kba_excl_aze`, `kba`, `aze`). **S**
- Replace `include_aze_sites`/`aze_only` with `aze = c("exclude","include","only")`. **S**
- Return raw coverage (`rescaled = FALSE`), as for mangroves and PAs, so a KBA layer means "fraction of PU in a KBA" in every country. **S**
- Tests: each filter combination × empty result × `output_path`; a point buffer area check (km²). **S**

### `make_mangroves()`  (R/make_mangroves.R:48)
**Does:** dissolves mangrove polygons and returns rescaled PU coverage.
**Used by:** elsar_pipeline, 1 call (FN:92) with `sf_in, pus, iso3, name_out = name`, no `output_path`. Not used internally.
**Method:** coverage of the union of GMW polygons, then a min-max rescale. Sound for a presence/extent feature, but rescaling makes it "coverage relative to the national maximum" (see the KBA note). The docs say neither.
**Verdict:** merge into `vector_coverage()`. The body is 6 lines and identical in effect to the KBA, ICCA and IUCN-forest paths.
**Issues:**
- The example cannot run. It calls `load_data()`, which does not exist (it is `elsar_load_data()`), assigns `ist_dat` and then uses `list_dat`, and omits the required `iso3` and `name_out` [code, lines 13-45].
- `name_out` has no default even though it is used only when `output_path` is set. The `# rename function name later` comment is still in the signature.
- Performance: `summarise()` on all national GMW polygons (hundreds of thousands for IDN, BRA, AUS) with no crop to the PU extent first (unlike PAs). `terra::rasterize(vect(x), pus, cover = TRUE)` gives the union coverage without a GEOS union. [ran]: two overlapping polygons covering 50% of a cell returned 0.5, not 0.7. Precision versus exactextractr is UNVERIFIED.
**Improvements:**
- Make it `vector_coverage(sf, pus, ...)` plus a 1-line wrapper, soft-deprecated after 0.6. Default `name_out = "mangroves"`. **S**
- Crop to the PU bbox before the union (reuse `crop_to_extent()`). **S**
- Return raw coverage. Fix the example. Add a test (no tests exist). **S**

### `make_wetlands_ramsar()`  (R/make_wetlands_ramsar.R:27)
**Does:** combines Ramsar site coverage and a thresholded wetlands raster as `0.5·wetlands + ramsar`, rescaled.
**Used by:** elsar_pipeline, 1 call (FN:301) with `wetlands_in`, `ramsar_in` (a gpkg *path* or an `sf`), `iso3 = base_iso3` and **`buffer_points = FALSE`**. Not used internally.
**Method:** the wetland raster is first reclassified with `ifel(is.na(x), 0, ifel(x > 1, 1, 0))`, so class value 1 counts as *non*-wetland, then aligned, then thresholded at `wetland_threshold = 0.25` "fraction". Under audit §2 #1 the fraction is point-sampled, so the threshold has no effect today. Ramsar coverage is added with weight 1 against 0.5 for wetlands. The `x > 1` legend and the 2:1 weighting are both undocumented, and neither the source product nor the reason for the weights is stated. Ramsar polygons are dropped in most configurations (audit §2 #6).
**Verdict:** refactor: split the reader, define the formula explicitly, and give it the same skeleton as the other builders.
**Issues:**
- [ran] t02c: wetlands input with values {1, 2} → output {0, 1}. Every pixel with class 1 is discarded. Whether 1 means "no wetland" depends on the product (UNVERIFIED which product the pipeline uses; `find_file_by_pattern(fnames, "wetland")`). If the input is a 0/1 binary wetland mask, **the wetland component is always empty**.
- The pipeline's `buffer_points = FALSE` only works because its gpkg has a `boundaries` layer, so the points-present branch at lines 152-176 rasterises the polygons. A polygon-only `sf` skips the whole block and is dropped (audit §2 #6).
- There is no `output_path` argument, unlike every sibling builder, and no validation of `pus`/`iso3`.
- The 60-line GeoPackage reader with three nested `tryCatch` fallbacks lives inside the builder. Reading belongs in `elsar_load_data()`. The `OGR_STROKE_CURVE` fallback also restores the env var through `on.exit` inside an error handler, which runs when the handler's own function exits, not the builder's.
- `return_all = TRUE` is ignored silently unless both inputs exist, so the return shape changes with the data (audit §4 return types).
- `has_ramsar` uses `exists("ramsar")`, which looks up the calling environment chain and is fragile [code, line 195].
**Improvements:**
- Make `wetland_classes =` explicit (no hidden `x > 1`) and `weights = c(ramsar = 1, wetlands = 0.5)`. Document both with the source product. **S**
- Move the gpkg reading to `elsar_load_data(layers = c("boundaries","centroids"))`. **M**
- Use `vector_coverage()` for Ramsar (always rasterise polygons, buffer points by `area_ha` × 1e4). Add `output_path`. Always return the same type. **M**
- Tests: polygon-only, point-only with and without buffers, wetlands-only with a 0/1 mask, both inputs. **S**

### `make_forest_integrity()`  (R/make_forest_integrity.R:40)
**Does:** aligns FLII and/or FSII and returns their rescaled sum ("mean").
**Used by:** elsar_pipeline, 1 call (FN:142) with `raster_flii, raster_fsii` (either may be NULL), `pus, iso3`. Internal: `get_all_features()`.
**Method:** `terra::mean(fsii + flii)` is the mean of a one-layer raster, i.e. the **sum** of the two *raw* indices, which is then rescaled. FLII is distributed as 0-10000 integers (Grantham et al. 2020). FSII uses a different scale (UNVERIFIED: the example's `zero_fsii < 14` implies 0-100 or similar). The sum is therefore dominated by FLII and FSII has almost no influence. None of this is documented: the help page has no `@details` and no citation for either index.
**Verdict:** fix: rescale each index before combining, then document the formula.
**Issues:**
- [ran] t02a T7: with FLII ∈ {0, 10000} and FSII ∈ {1, 0} in opposite cells, the output is {0, 1}. Cells with full FSII and zero FLII get 0, so FSII is ignored.
- Both inputs all-0: the "both zero" message branch (line 89) is unreachable. Line 71 (fsii sum == 0) fires first, and rescaling an all-0 FLII gives NaN [ran t02a T6: min = max = NaN]. Line 82 tests `fsii` twice; `flii` was intended.
- Both inputs NULL: `raster_rescaled` is never assigned, so the user gets "object not found" instead of a message [code].
- FLII NoData is −9999 in the published product (UNVERIFIED for the pipeline's copy). The sentinel guard in `make_normalised_raster()` only catches ±2^31 and >1e30, so an unflagged −9999 would become the minimum and compress the rescale.
- NA (non-forest) is filled with 0 inside the PUs, so "not forest" and "forest with integrity 0" become the same value. That should be a documented choice.
- No input validation of `pus`/`iso3`; `iso3` is required only for the file name.
**Improvements:**
- `fi = mean(rescale(flii), rescale(fsii), na.rm = TRUE)` with a fixed `range =` per index (FLII 0-10000), not a per-country min/max. Add `weights =`. **S**
- Add a docs section with the formula, the scales and the citations. Error clearly when both inputs are NULL or all-0. **S**
- Tests: each single input, both inputs at different scales, all-0, an unflagged −9999. **S**

### `make_degraded_areas()`  (R/make_degraded_areas.R:36)
**Does:** binary "degraded" if agriculture > t, built > t, SDG 15.3.1 = −1, or HII ≥ `hii_threshold`. It also writes agriculture, built and HII intermediates.
**Used by:** elsar_pipeline, 1 call (Z:1232), **opt-in** (`compute_degraded_areas: false` by default). That call passes `iso3, pus, sdg_degradation_input, human_pressure, lulc_proportions, agricultural_areas, built_areas`, `hii_threshold = degraded_hii_threshold` (4), `lulc_threshold` (0.1) and `output_path = intermediates dir`. It then re-binarises the result with `ifel(da > 0, 1, 0)`, which does nothing because the output is already binary. Not used internally.
**Method:** thresholds are applied to *raw* aligned values, which is good: unlike the zone builders, this avoids audit §2 #3. But alignment is a **fourth, private implementation**: `resample` to the PU grid projected into the source CRS, then `project(near)`, then `resample` (a no-op). Agriculture and built use bilinear (point sampling, not area-weighted, as in audit §2 #1). The HII threshold of 4 is the Human Footprint "low pressure" cut-off (Williams et al. 2020, 0-50 scale), but the docs call the input "Human Influence Index" and give no scale. NA holes are covered by audit §2 #12.
**Verdict:** refactor: use the shared alignment, drop the side-effect writes, and move it next to the restore-zone builder (it is a zone input, not a feature).
**Issues:**
- It writes `built_areas_{iso3}.tif`, `hii_{iso3}.tif` and `esa_agriculture_resample_{iso3}.tif` (the "esa" is wrong when the input is Dynamic World or ESRI) into `output_path` as side effects. These collide with other builders (audit §4 collisions). The pipeline then renders PNGs of these intermediates, so it depends on the side effect.
- Each of the four inputs is pushed through 3 warps (lines 207-261), 12 warps in total, where one area-weighted `align_to_pus()` per input would do.
- The `lulc_proportions` list/SpatRaster handling duplicates code in the zone builders. `iso3` is the first argument and is not validated (audit §4 argument order).
- The return variable is called `restore_zone` and the log says "Degraded areas/Restore Zone layer", which blurs it with `make_restore_zone()`.
**Improvements:**
- `align_to_pus(type = "proportion")` for agriculture and built, `type = "categorical"` for SDG, `"continuous"` for HII. **S** once the primitive exists.
- `return_intermediates = TRUE` returns a named stack instead of writing. Keep writes behind `output_path` only for the final layer. **S**
- Rename `hii_threshold` to `human_pressure_threshold` (glossary) and document the scale. Accept `NA` in SDG as "not degraded" explicitly. **S**
- Tests: each criterion alone, NA handling, raw-vs-rescaled threshold. **S**

### `make_flood_abatement_opportunities()`  (R/make_flood_abatement_opportunities.R:30)
**Does:** `rescale(GFD_rescaled / rescale(1 − NDVI))`, with 0 where the denominator is 0.
**Used by:** elsar_pipeline, 1 call (FN:336) with `gfd_raster, ndvi_raster, pus, iso3`. Not used internally.
**Method:** the denominator is "lack of greenness" rescaled to 0-1, so the score grows hyperbolically with NDVI (audit §2 #12 "ratio blows up"). The docs say only "based on their relationship" and do not state the formula, its direction (green or bare cells in flood areas?) or a source. If the intent is "vegetated land in flood-prone areas", a product `flood × ndvi` is the standard, bounded form. If the intent is "bare land in flood-prone areas, to restore", the ratio has the wrong direction.
**Verdict:** fix: redefine and document the method. As built, the layer is carried by a handful of cells.
**Issues:**
- **The greenest cell gets 0 and the next-greenest cells get the maximum.** The cell with maximum NDVI rescales to `1 − NDVI = 0`, which hits the `ndvi == 0 → 0` guard. [ran] t02c, uniform flood with NDVI 0.2-0.9: NDVI 0.2 → 0.010, 0.5 → 0.020, 0.88 → 0.50, 0.89 → **1.00**, 0.90 (greenest) → **0.00**. Two cells span half the range.
- Constant GFD (e.g. a single flood count everywhere) makes the whole layer NaN, via audit §2 #2 [ran t02b T8].
- `threads` is accepted and never used. `method_override = "mean"` is ignored (audit §2 #1).
- Water (NDVI < 0) is set to NA and then filled with 0 before rescaling, so water becomes "lack of greenness = 0", the same value as the greenest cell.
**Improvements:**
- Define the formula as `opportunity = flood_norm × ndvi_norm` (or `× (1 − ndvi_norm)` for restoration) after a maintainer decision. Add an `@details` section with the formula and citations (GFD: Tellman et al. 2021). **S**
- Mask water explicitly (`NA`, not 0). Drop `threads`. **S**
- Tests: monotonicity, bounded output, constant inputs, water handling. **S**

### `make_urban_greening_opportunities()`  (R/make_urban_greening_opportunities.R:50)
**Does:** `rescale(((invNDVI + heat) / 2) × urban)`, where urban = built fraction > `threshold` and heat = rasterised GHS-UCDB `avg_intens`.
**Used by:** elsar_pipeline, 1 call (FN:898). It passes `ndvi_raster, lulc_raster, built_areas_raster` (either may be NULL), `sdei_statistics` (the `wbgtmax` vector), `pus`, `iso3 = base_iso3` and `return_urban_areas = TRUE`. It relies on the two-layer return. `lulc_product` is left at the default "esri_10m" even when the LULC is Dynamic World (audit §2 #10). Not used internally.
**Method:** equal-weight mean of inverted NDVI and heat, masked to urban PUs. NDVI is inverted twice (audit §2 #4). The built fraction is rescaled and then compared with `threshold = 0.10`, so "urban" means "≥ 10% of the national maximum built fraction". This is the audit §2 #3 pattern; this function is not in the audit's list. The formula and its weights are not in the docs.
**Verdict:** fix.
**Issues:**
- **It requires the PU layer to be named exactly "Planning Units".** Line 213 filters on `` `Planning Units` == 1 ``. [ran] t02b T11: a PU raster named "pu" → error "object 'Planning Units' not found". PUs built by `make_planning_units()` carry that name, but PUs read from file or renamed do not.
- The heat layer goes through `exact_rasterise(fun = mean)`. In the per-feature path, mean is taken over *all* features, including the 0 coverage of the cities that do not touch the cell. [ran] t02a T4: four non-overlapping squares with value 10 → every cell 2.5 (= 10/4). The later rescale hides this here, but above 1000 cities (likely for IND and CHN from GHS-UCDB, UNVERIFIED) the function switches to the `terra::rasterize` path, which has different edge semantics, so results change with the city count.
- `x == built_area_lulc_value` (line 200): with `lulc_product = "local"` and a vector of class values, `==` recycles. It should be `%in%`.
- `cores` is documented as "currently unused". The country filter hard-codes the GHS column names `CTR_MN_ISO`, `ID_HDC_G0` and `avg_intens`.
- Projecting PU polygons to the SDEI CRS and intersecting (lines 211-222) is only a clip. `crop()` on the bbox would do the same job at a fraction of the cost.
**Improvements:**
- Use `!is.na(pus[[1]])` (the first layer, not a name). Use `%in%` for class values. **S**
- `rescaled = FALSE` for the built fraction before the threshold (via the shared helper from audit §2 #3). Remove one NDVI inversion. **S**
- Expose `weights = c(ndvi = 0.5, heat = 0.5)`, `heat_col`, `city_id_col`, `iso3_col`. Document the formula. **S**
- Rasterise heat with `terra::rasterize(field = "avg_intens", fun = "max")`, a single pass with the same result for any city count. **S**
- Tests: PU name, threshold on raw fraction, NDVI direction, a no-heat-data fallback. **S**

### `make_indigenous_managed_lands()`  (R/make_indigenous_managed_lands.R:34)
**Does:** buffers points, unions LandMark and ICCA polygons, and returns rescaled PU coverage.
**Used by:** elsar_pipeline, 1 call (FN:371) with `iso3, pus, sf_landmark, sf_icca`. Both inputs are pre-filtered by country in the loader (`ISO_Code` / `iso3`). Not used internally.
**Method:** coverage of the union, then a min-max rescale. The docs promise "applies a threshold, and returns a binary raster" with 1 = likely Indigenous-managed. The code applies no threshold.
**Verdict:** merge into `vector_coverage()`, keeping a thin wrapper that fixes the area columns.
**Issues:**
- **The output is not binary.** [ran] t02b T10: a 2.5 km square → values {0, 0.25, 0.5, 1}. The `@return` and title are wrong, or a threshold argument is missing.
- The area columns `Area_GIS` (LandMark) and `reported_area` (ICCA) are hard-coded, not arguments. Both use `area_multiplier = 1e4` (hectares). The units are UNVERIFIED, as audit §2 #5 already asks.
- With `buffer_points = TRUE`, `convert_points_polygon(append_original_polygons = TRUE)` drops polygons with `Area_GIS` NA/0 (audit §2 #12). For LandMark that can remove real polygons whose area was never calculated.
- Unlike KBAs and Ramsar, this function does no country filter of its own. The asymmetry is undocumented.
- No crop to the PU extent before the union. No input validation.
**Improvements:**
- Choose between binary and fraction: add `threshold = NULL` (NULL = fraction, raw) and fix the docs. **S**
- Add `area_cols = c(landmark = "Area_GIS", icca = "reported_area")` and `area_units = "ha"`. **S**
- Route through `vector_coverage()`. Add tests (none exist). **S**

### `make_managed_forests()`  (R/make_managed_forests.R:36)
**Does:** reclassifies forest-management classes to 1, aligns (meant as a fraction), rescales, and optionally multiplies by rescaled NPP.
**Used by:** elsar_pipeline, 2 calls (FN:797, FN:821), always with `include_disturbed_forest = TRUE` and the default `lulc_product` ("esri_10m"). FN:797 adds `make_productive = TRUE, raster_npp`. The input is the Lesiv FML raster. Internal: `make_manage_zone()` (R/make_manage_zone.R:264), which thresholds the *rescaled* output with `forest_class_threshold` (audit §2 #3).
**Method:** the class codes come from `get_lulc_class_value(product, "forest_managed")`, which returns Lesiv codes filed under ESRI (audit §2 #11). "Disturbed forest" (code 20) is Lesiv's "naturally regenerating forest **with signs of management**", which is managed forest by definition. The pipeline always adds it, and the default leaves it out. "Productive" = rescale(fraction_rescaled × NPP_rescaled): undocumented, and it double-rescales.
**Verdict:** fix.
**Issues:**
- `name_out` is a required formal (no default) that is never used ("not currently used" in the docs).
- `include_disturbed_forest` prepends the hard-coded code 20 regardless of `lulc_product` [code, line 224]. With Dynamic World or ESA that adds an unrelated class (DW has no 20; ESA 20 = shrubland).
- Validation: the first `assert_that` (lines 204-209) runs only when both rasters are non-NULL. `raster_in = NULL` passes through to a `terra::project` error.
- The return shape depends on `make_productive` (1 or 2 layers), and the layers are unnamed. The pipeline names them itself.
- The proportion is rescaled by the national maximum, so "fraction of PU managed" is lost. The pipeline's manage zone then thresholds it.
**Improvements:**
- Move the codes to a `forest_management` product (audit §2 #11). Rename `include_disturbed_forest` to `include_regenerating_managed` (default TRUE, matching pipeline practice) and look its code up in that product. **S**
- Remove `name_out`. Return named layers (`managed_forests`, `productive_managed_forests`). Use `rescaled = FALSE` for the fraction. **S**
- Document "productive" (formula, NPP source and units). **S**
- Tests: class mapping per product, fraction values, productive layer. **S**

### `make_threatened_ecosystems_protection()`  (R/make_threatened_ecosystems_protection.R:50)
**Does:** per ecosystem, % of area that is not intact (EII < global median, or degraded), rasterised by coverage and rescaled.
**Used by:** elsar_pipeline, 1 call (FN:452) with `ecosystems_sf = pipeline_state$ecosystems` (in the PU CRS), `group_attribute = "eco_id"`, `boundary_layer`, `integrity_raster`, `integrity_type = "eii"`, `iso3`. Its output (`pipeline_state$threat_p`) feeds `make_threatened_ecosystems_restoration()`. Not used internally.
**Method:** the threat score is national ("share of this ecosystem's area *within the country* that is below the global EII median"). That is a reasonable RLE-criterion-D-style proxy, but neither its national scope nor its link to IUCN RLE is documented. It compares rescaled EII with the raw global median (audit §2 #12). Implementation: the intactness mask is polygonised, then `st_intersection` is run per ecosystem polygon in a `future` plan the function sets itself, then the threat is rasterised with `exact_rasterise(fun = mean)`.
**Verdict:** refactor: the same quantity is one `exact_extract()` call. The vector pipeline is slow, fragile, and wrong in edge cases.
**Issues:**
- **The CRS is relabelled, not transformed.** Line 103 runs `st_set_crs(st_crs(ecosystems_sf))` on polygons in the PU CRS. If `ecosystems_sf` is not in the PU CRS, the mask is mislabelled and nothing intersects. [ran] t02b T13: the same ecosystems in EPSG:4326 → output all NA (`numeric(0)` non-NA values), against {0, 0.5, 1} in the PU CRS. The pipeline is safe only because it projects ecosystems first (UE:150, 376).
- **`iso3` defaults to NULL but is required.** `exact_rasterise()` asserts `is_valid_iso3(iso3)` [ran t02a T5: error]. The same applies to `make_underrepresented_ecosystems()`.
- The docs say "values from 0 to 100", but the output is rescaled to 0-1 by `exact_rasterise` → `make_normalised_raster`.
- `fun = mean` across ecosystems divides by the number of groups and then rescales. The effective value is the coverage-weighted **sum** of threats, so cells where ecosystems overlap (IUCN GET realms, major and minor occurrence) score up to 2x [ran T4 shows the /N; the sum follows].
- The mask names are inverted throughout: the mask is 1 = intact, but the variables are `non_intact_areas` and `area_intact`. The arithmetic is right, but the names mislead the reader.
- NA EII (water, ice, gaps) → `fill_na = 0` → counted as *not intact*, which inflates threat for wetland and freshwater ecosystems. In "degraded" mode, NA → 0 → counted as *intact*. The two modes treat NA in opposite ways [code].
- Any geometry error in a worker → `area_intact = NA` → 100% threat, reported only via `message()` [code, lines 142-145, 164].
- Side effects: it sets the `future::plan()` (restored on exit), sets `progressr::handlers()` globally (not restored), and hard-codes workers to half the physical cores. It also emits a `future.seed` "UNRELIABLE VALUE" warning per worker [ran T13].
- `boundary_layer` is validated but used only in the docs ("to calculate the median"). It is unused in the code. `median_from_rast()` needs an `.aux.xml` histogram side-car next to the file, and this function's docs do not mention that.
**Improvements:**
- Replace lines 97-165 with `pct_intact <- exactextractr::exact_extract(mask, eco_summary, "mean")` (coverage-weighted, NA-aware), then `threat = 1 − pct_intact`. No polygonising, no `future`, no `progressr`, no `units`. **M**
- Rasterise with `fun = "max"` or a coverage-weighted mean (documented). Return 0-1 and fix the docs. **S**
- Take `threshold = NULL` (NULL = global median from the side-car, or a number) and compare with the *raw* aligned EII. Remove `boundary_layer` (soft-deprecate). **S**
- Validate or transform the ecosystems CRS. Make `iso3` optional (only for the file name). **S**
- Tests: tiny ecosystems with a known % intact, overlapping groups, NA EII, a CRS mismatch. **S**

### `make_threatened_ecosystems_restoration()`  (R/make_threatened_ecosystems_restoration.R:28)
**Does:** multiplies the threat raster by a degradation mask and rescales.
**Used by:** elsar_pipeline, 1 call (Z:1447) with `iso3, pus`, `threatened_ecosystems_input = threat_p` and `degradation_input = restore_zone_v1` (binary restore zone). Not used internally.
**Method:** threat × binary mask, then re-rescaled by the maximum *within the degraded area*. So the protection and restoration layers have different scales, and the restoration values change whenever the restore zone changes. Defensible, but undocumented.
**Verdict:** keep: a small, single-purpose helper the pipeline relies on. Fix the docs and validation.
**Issues:**
- The docs refer to `make_threatened_ecosystems()` (it does not exist) and say `degradation_input` comes from `make_restore_zone()`. The example uses file names (`threatened_ecosystems_for_protection_KEN.tif`, `restore_zones_KEN.tif`) that no function writes. The protection builder writes `threatened_ecosystems_{iso3}.tif`.
- No grid check: `*` errors on misaligned inputs with a terra message. A non-binary `degradation_input` (e.g. a fraction) silently weights the threat.
- `output_path` is validated only after the computation (line 254). It is the first argument after `iso3`, which is inconsistent with the protection builder.
**Improvements:**
- `compareGeom()` checks. Binarise or validate `degradation_input`. Add `rescaled = TRUE` so users can keep the protection scale. Fix the docs. **S**
- Tests: mask semantics, no overlap → 0 (not NaN), misaligned inputs. **S**

### `make_underrepresented_ecosystems()`  (R/make_underrepresented_ecosystems.R:33)
**Does:** per ecosystem, the gap = max(0, target − % protected), rasterised with coverage-weighted `max` and rescaled.
**Used by:** elsar_pipeline, 1 call (FN:410) with `ecosystems_sf, group_attribute = "eco_id"`, `protected_areas_sf = pipeline_state$current_pa_sf` (dissolved, cast to POLYGON), `pus, iso3`, default `target_percent = 30`. The pipeline skips the call when there are no PAs. Tests: `test_empty_protected_areas.R`.
**Method:** an absolute gap in percentage points, measured nationally against *vector* PA area. The PA feature uses a different definition of "protected": PU coverage > 10%. The two definitions can disagree, and neither doc mentions it. The rescale divides by the largest gap in the country.
**Verdict:** refactor: a raster-based computation is simpler, faster and consistent with the PA layer.
**Issues:**
- The docs contradict themselves. The description says "the average degree of underrepresentation", `@return` says "the maximum gap (0 to target_percent)", and the actual output is rescaled to 0-1 (except in the no-PA case). There is also a stray `#'#'` at line 6.
- **Coverage-weighted `max` deflates edge cells.** A cell 50% covered by a full-gap ecosystem scores half the gap. [ran] t02b T12: across the ecosystem boundary, 1, 1, 1, 1, 1, **0.5**, 0, ... Cells on the coast or a national border are systematically down-weighted, although the "max gap present in the PU" is the full gap.
- Performance: a sequential loop over *every PA polygon* runs `st_intersection(ecosystems_sf, pa)` on the whole ecosystem layer, i.e. O(n_PA × n_eco) GEOS calls. The pipeline passes the dissolved PAs cast to POLYGON, which can be thousands of parts.
- `try(..., silent = TRUE)` swallows intersection errors, so protection is silently underestimated and the gap overestimated.
- If a user passes raw (undissolved) WDPA, overlapping PAs are double-counted, and `percent_protected` can exceed 100.
- `iso3 = NULL` default, but it is required by `exact_rasterise()` (see the threatened-ecosystems issue). `group_attribute` is lowercased into the file name.
**Improvements:**
- Compute `pct_protected = exact_extract(pa_coverage_raster, eco_summary, "weighted_mean"/"mean")` from the same PA raster as the feature, or from one `st_intersection(eco_summary, st_union(pa))`. Drop the loop and the `try`. **M**
- Rasterise with plain `max` over ecosystems present (coverage > 0), or document coverage weighting. Add `gap = c("absolute","relative")`. **S**
- Fix the docs and the `iso3` default. Return 0-1 with the scale documented. **S**
- Tests: known gap values, overlapping PAs, edge cells. **S**

### `get_iucn_ecosystems()`  (R/get_iucn_ecosystems.R:52)
**Does:** reads IUCN GET gpkg files in parallel, crops and intersects them with the boundary, reprojects, and dissolves by (`get_id`, `occurrence`).
**Used by:** elsar_pipeline, 1 call (UE:277) inside `load_iucn_get()`, which the pipeline itself marks deprecated. It passes `iucn_get_directory, iso3, boundary_layer, pus`, `include_minor_occurrence = TRUE`, `output_path = NULL`. Internal: none. The roxygen links it to `get_iucn_forests()`.
**Method:** file-prefix filtering, then a bbox crop in WGS84, then an exact intersect with the boundary in the source CRS, then reprojection to the PUs. Sound for clipping. Minor and major occurrences are kept as separate features of the same `get_id`, and nothing documents that.
**Verdict:** deprecate: make the documented deprecation real (`lifecycle::deprecate_soft()`) and remove it in 1.0 together with the pipeline's `load_iucn_get`.
**Issues:**
- The deprecation exists only as a badge. No runtime `deprecate_*()` call is made [code; grep finds none].
- `allowed_prefixes` is a hard-coded list of 110 GET v2 codes, so any newer GET code is rejected. `iucn_get_prefixes` filters by file name only, while `excluded_prefixes` uses a different regex on the same file names.
- It sets the `future` plan and `progressr` handlers like the threatened-ecosystems builder, and adds `future.apply`/`progressr` to Imports for a deprecated function.
- It returns `NULL` (not a 0-row `sf`) when nothing intersects. The pipeline handles that.
- `assertthat::assert_that(is.string(dir), dir.exists(dir))` gives a generic message.
**Improvements:**
- `lifecycle::deprecate_soft("0.5.0", "get_iucn_ecosystems()")`. Point to the ecoregions path. **S**
- If kept until 1.0: run sequentially by default (`parallel = FALSE`), return a 0-row `sf`, and drop the prefix whitelist. **S**

### `get_iucn_forests()`  (R/get_iucn_forests.R:45)
**Does:** filters GET polygons to forest biomes, dissolves, and returns rescaled PU coverage.
**Used by:** unused (0 pipeline calls, 0 internal callers, no tests).
**Method:** coverage of the union of MFT1.2, T1.x, T2.x, TF1.1 and TF1.2, rescaled. The biome list is sensible but undocumented: TF1.2 and MFT1.2 (mangroves) are wetland or intertidal forests, which overlaps the mangrove feature.
**Verdict:** deprecate (soft now, remove in 1.0).
**Issues:**
- `boundary_layer` is required and asserted but never used.
- The example calls `get_iucn_forests_from_layer()`, which does not exist.
- The badge says deprecated, but there is no runtime warning.
**Improvements:**
- `lifecycle::deprecate_soft()`. If anyone needs the layer, it is `vector_coverage(dplyr::filter(eco, get_id %in% forest_ids), pus)`. **S**

### `get_all_features()`  (R/get_all_features.R:24)
**Does:** a hard-coded wrapper that looks up files by name pattern and runs `make_normalised_raster()` or `make_forest_integrity()` for six named features.
**Used by:** unused (0 pipeline calls, 0 internal callers, no tests). The pipeline's layer registry replaces it.
**Method:** n/a. It is a stub (audit §4, deprecation planned for 0.6 in audit §6).
**Verdict:** deprecate now (soft in 0.5), not in 0.6. It is broken in ways that the 0.6 registry will not fix.
**Issues (new):**
- The `drought_risk` branch searches for `data_name = " "` and writes `name_out = " "` (lines 197-210), so it matches any file containing a space and writes `" _ISO.tif"`.
- The `fi` branch calls `make_forest_integrity()` without `iso3` (line 239). With `output_path` set, the glue for `forest_integrity_{iso3}.tif` fails.
- `extract_filename_filetype()` returns the filename *with its trailing dot*, and the file type is never passed on, so `elsar_load_data(file_name = "FML.")` receives a truncated name [code].
- `cropsuit` uses `invert = TRUE`, while the pipeline uses `ifel(r > 0, 0, -r)` for the same layer. The package and the pipeline disagree on how that layer is defined.
**Improvements:**
- `lifecycle::deprecate_soft()`, pointing to `elsar_build()`/the registry once they exist. **S**

---

## Cross-cutting

1. **There are only three builder skeletons, plus copies of them.** Every function here is one of the following:
   - (a) **vector → coverage**: PA, KBA, mangroves, Ramsar, indigenous, IUCN forests. Six copies, differing in: order of `make_valid` and union (KBA unions before repairing); whether they crop to the PU extent first (only PA); how they treat points (three different `area_multiplier`s, hard-coded area columns); whether they dissolve on every branch; and whether they rescale the coverage (all do, except PA binary).
   - (b) **raster → align → normalise**: `make_normalised_raster()` and its direct users (FI, flood, urban greening, managed forests, threatened-restoration), plus a private fourth alignment in `make_degraded_areas()`.
   - (c) **ecosystem × condition → per-ecosystem score → rasterise**: threatened protection and underrepresented. Both use slow vector intersections where `exact_extract()` on a raster gives the same number.

   The audit's `vector_coverage()` and `align_to_pus()` (§6.4) cover (a) and (b). This review adds a third primitive: `ecosystem_score(eco, value_raster, fun)`, built on `exact_extract`. It removes `future`, `progressr`, `units` and roughly 150 lines, and fixes the CRS-relabel and edge-weighting bugs by construction.
2. **Rescaling coverage and proportion layers by the national maximum** is the silent default everywhere (KBA, mangroves, indigenous, managed forests, IUCN forests, wetlands combined). A "fraction of PU covered" layer should stay a fraction. Make `rescaled = FALSE` the default for coverage layers, and keep min-max for indices only. Then a value means the same thing in every country.
3. **Formulas are undocumented.** No help page in this group has a formula plus source section: forest integrity (sum of raw indices), flood (ratio), urban greening (equal weights), wetlands (`0.5·W + R`, `x > 1`), threatened (national share below the global median), underrepresented (absolute pp gap, coverage-weighted max). Add a `@section Method:` with formula, units, scale, thresholds and citation to each builder. The registry (audit §6.2) can generate the data-info vignette from these sections.
4. **Empty-result paths are inconsistent.** Some return before writing (KBA ×3, `make_normalised_raster`), some name the layer "Planning Units", some warn, some only log, and some error (wetlands with neither input; forest integrity with both inputs NULL gives "object not found"). One `empty_layer(pus, name)` helper with a single exit/write point is enough.
5. **`exact_rasterise(fun = mean)` averages over all features, including those that do not touch the cell** (value/N, [ran] T4). Its mode also flips at 1000 features. Three builders here depend on it. The fix belongs in `exact_rasterise` (`NA` outside coverage, or `fun = "weighted_mean"`), not in each caller.
6. **`iso3` is mandatory but means "file name".** Defaults of `iso3 = NULL` are contradicted by `exact_rasterise()`'s assertion. Several builders accept `iso3` they never validate, others validate it with three different checks (`is.string`, `is_valid_iso3`, none). With `elsar_region()` (audit §6.1) this disappears. Until then, make `iso3` optional wherever it only names output.
7. **Unused or dead arguments:** `threads` (flood), `cores` (urban greening), `name_out` (managed forests), `boundary_layer` (threatened protection, IUCN forests), `method_override` (everywhere, audit §2 #1). Soft-deprecate them in 0.5.
8. **Tests:** 11 of the 17 functions have no test. The cheapest win is one synthetic fixture (the 10x10 grid used in t02*.R) and one 1-2 s test per builder covering: values on a known geometry, the empty path, the `output_path` write, layer name and range. Every [ran] item above is a ready-made regression test.
9. **Pipeline coupling to watch when changing defaults:** the pipeline relies on the two-layer returns (urban greening, managed forests); on `return_sf = TRUE` (PA); on rescaled KBA, mangrove and indigenous values only through `raster_is_empty()`; and on the intermediates written by `make_degraded_areas()`. It overrides `buffer_points = FALSE` (Ramsar) and `include_disturbed_forest = TRUE`. It never uses `invert`, `method_override` or `output_path` on features, so changing those is low-risk downstream.

**Verdict counts (17):** keep 1 · fix 6 · refactor 5 · merge 2 · deprecate 3 · make internal 0.
- keep: threatened-restoration.
- fix: protected areas, KBAs, forest integrity, flood, urban greening, managed forests.
- refactor: `make_normalised_raster`, wetlands/Ramsar, degraded areas, threatened-protection, underrepresented.
- merge into `vector_coverage()`: mangroves, indigenous lands.
- deprecate: `get_iucn_ecosystems`, `get_iucn_forests`, `get_all_features`.

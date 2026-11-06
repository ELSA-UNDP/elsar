# elsar audit and roadmap (November 2026)

**Status:** proposals only. Nothing in this document has been applied to the
package code.
**Scope:** `develop` at f42e0df (= released 0.4.1) plus the pending work:
`feature/elsa-results-analysis` (results tools), PR #103 (map style, draft)
and PR #102 (Pages deployment). Downstream use was checked in
`elsar_pipeline` and `elsa_national`.

**Method:** five independent reviews:

1. core data preparation;
2. Earth Engine and external data;
3. infrastructure, CI and release;
4. API, documentation and user experience;
5. the pending branches.

Reviewers ran R on small synthetic data to reproduce problems, plus
`R CMD check`, covr, lintr and urlchecker. The most consequential claims were
re-checked against the code by a second reader.

- **[verified]**: reproduced by running code.
- **[code]**: follows directly from the code but was not run.
- **[unverified]**: depends on runtime behaviour we could not test (mostly
  Earth Engine).

---

## 1. Summary

elsar is a capable toolbox with a fragile core. Three findings matter more
than the rest.

1. **Several layers are numerically wrong today, without any error.**
   - The shared alignment step does not area-weight. Every layer passes
     through it.
   - Constant layers become NaN.
   - Thresholds are applied to rescaled values, so they mean something
     different in each country.
   - KBA point buffers are 1/100 of their reported area.
   - Ramsar sites are dropped in most configurations.
   - The Earth Engine land-cover "proportions" are probably not proportions.

   These numbers feed every country's ELSA run. Releases ship unpinned to
   `elsar_pipeline`.
2. **There is almost no safety net.**
   - Test coverage is 21.9%, and 21 source files have no tests at all.
   - CI does not run on `develop`.
   - The one Earth Engine test calls a function that does not exist.
   - No git tags or releases exist, so nobody can pin or roll back.
3. **The real workflow is outside the package.**
   - Country teams use `elsar_pipeline`, which is private: about 12,000
     lines with a config schema, a layer registry and a stack builder.
   - Public users get roughly 70 building blocks and no path through them.
   - The pipeline already calls an elsar function that was removed without
     deprecation (`make_postgres_connection`).

The fix is a sequence:

1. **Stop wrong numbers** (0.4.2, small patches with regression tests).
2. **Build a safety net** (CI, tags, tests).
3. **Consolidate the core**: one alignment primitive, one vector-to-raster
   helper, a region object and a layer registry, so the building blocks are
   consistent and dependable for the private pipeline and for public users.

The pipeline stays private (decided November 2026, §7), so elsar does not
take over its orchestration.

---

## 2. Fix first: silent wrong results (proposed 0.4.2)

> **See also** [`2026-11_function_review.md`](2026-11_function_review.md),
> the per-function review. It adds 20 more findings for 0.4.2 (F1–F20) and
> eleven mismatches between elsar and the pipeline (P1–P11), such as `wait_time`
> units, the unused `limit_to_mainland` option and the dropped degradation
> term. Those belong in the same release.

Each item is small (S) unless marked. Each one needs a regression test that
fails before the fix. Many change numbers in existing outputs, so NEWS must
say which layers change and that they should be rebuilt.

| # | Problem | Where | Evidence | Fix |
|---|---|---|---|---|
| 1 | **`make_normalised_raster()` does not area-weight.** It warps with `terra::project()` (bilinear, or near for categorical rasters) *before* `exact_resample()`, which then runs on an already-aligned grid and does nothing. `method_override` (`mean`/`sum`/`max`/`mode`) is ignored. Every proportion layer is point-sampled. | `R/make_normalised_raster.R:181,190` | [verified] 100 m binary to 1 km: mean, max and sum identical; up to 50% relative error | Area-weighted aggregation in the source CRS (`exact_resample` against the PUs in the source CRS, or `project(method = "average"/"sum"/"max"/"mode")`), then `near` onto the PUs. **M**, and every layer's values change. |
| 2 | **Constant layers become NaN.** `rescale_raster()` divides by `max - min`. Hits fully protected regions, small islands and all-1 zones. | `R/utils.R:55` | [verified] all-1 → NaN; full-cover PAs → NaN | If the range is 0, return 1 (or 0) inside the PUs and warn. |
| 3 | **Thresholds are compared with rescaled values.** On the LULC, degradation and pasture paths, `agriculture_threshold = 0.1` means "10% of the national maximum", not 10% of the PU. | protect/restore/manage/urban zone builders | [verified] 2–4% agriculture everywhere: 10 cells pass instead of 0 | `rescaled = FALSE` for every thresholded input, through one helper. |
| 4 | **Missing data ranks highest.** NA is filled *before* `invert`. Urban greening also inverts NDVI twice, so greener cells (and water) score as most in need. | `R/make_normalised_raster.R:226-233`; `R/make_urban_greening_opportunities.R:96-102` | [verified] water → 1, NDVI 0.99 → 0.99 | Invert and rescale first, then fill. Remove one of the two NDVI inversions. |
| 5 | **KBA point buffers are 100× too small.** `repareakm2` (km²) is converted with the hectare factor. | `R/make_kbas.R:125` | [verified] 1 km² → 0.01 km² | `area_multiplier = 1e6`. Check the units of LandMark and ICCA too. |
| 6 | **Ramsar sites are dropped.** They are only rasterised when there are points *and* `buffer_points = FALSE`. | `R/make_wetlands_ramsar.R:137-195` | [verified] polygon Ramsar ignored; Ramsar-only input errors | Always rasterise the Ramsar `sf`. |
| 7 | **`sieve()` re-adds excluded farms and towns.** Small exclusion holes inside zones are filled. | all four zone builders | [verified] 4 excluded cells → 0 | Sieve only the 1-class. |
| 8 | **The custom projection breaks across the antimeridian.** The central meridian lands near 0° for Fiji, Kiribati and Russia; the same bbox bug affects Earth Engine exports. | `R/make_custom_mollweide_projection.R:58`; `R/gee_helpers.R:962` | [verified] Fiji: lon_0 = −0.005, width 35,210 km | Longitude-shifted centre, computed in one helper. Also set `false_northing = 0`. **S–M** |
| 9 | **Earth Engine LULC proportions are probably not proportions.** `reproject()` runs before `reduceResolution()`, and elsewhere `bestEffort = TRUE` runs with the default pixel limit. | `R/gee_helpers.R:1852-1859, 2255-2263` | [code] wrong call order confirmed; runtime effect [unverified] | One correct pipeline: native projection → `reduceResolution(maxPixels computed)` → `reproject` onto the PU grid. **M** |
| 10 | **Zone builders assume ESRI class codes.** Download functions tag the product, but nothing reads the tag. Dynamic World input with the default `lulc_product` excludes shrubland as "agriculture". | zone builders; `R/lulc_config.R` | [verified] the tag is never read | Require the product for categorical LULC input, and warn when raster values don't fit its legend. |
| 11 | **Managed-forest class codes come from the wrong dataset.** Lesiv forest-management codes are filed under ESRI, so non-ESRI products give all-zero managed forests. | `R/lulc_config.R:59-97` | [code] | Separate `forest_management` product entry. |
| 12 | **Other wrong-value bugs:** flood abatement ratio blows up; threatened ecosystems compare rescaled EII with a raw global median; `make_protected_areas(binary = FALSE)` is not coverage; `get_binary_layers()` mislabels when a layer is all NA; KBA/Ramsar `iso3` filters fall back to the function argument when the column is missing; `make_degraded_areas()` leaves NA holes; `convert_points_polygon()` drops polygons with area 0/NA. | various (reviewer A, items 9–16, 21) | mostly [verified] | Individual small fixes. |
| 13 | **Earth Engine caching and Drive return stale or wrong files.** Cache keyed only by `prefix_year_iso3`; unanchored Drive regex matches `_REG` and `(1)` files and **trashes** them; failed exports never detected; leftover tiles merged into the next run. | `R/gee_helpers.R` (reviewer B, items 5–7, 9) | [verified] regex and stale-tile merge | Exact filename match, task-status polling, per-run temp dir with cleanup, cache keys that include the grid. **M** |

### In the pending results branch (fix before it is merged)

These bugs are in the results work done this week. It must not merge until
they are fixed.

| # | Problem | Evidence | Fix |
|---|---|---|---|
| R1 | **One region without planning units sets every region to 0 ha.** `exact_extract(summarize_df = TRUE)` returns a list when a callback returns `NULL`. | [verified] 343,692 ha → 0 | Return a zero-row data frame. Test mixed regions. |
| R2 | **Float action rasters become all NA.** The unknown-value check runs before rounding. | [verified] min/max NaN | Round first. |
| R3 | **The default scheme mislabels 3-action and fallback runs**, with no warning: Do Nothing or Avoid Loss is reported as Urban Greening. | [verified] | Read the tool's RAT when present; warn when no no-action value is found; lead with `elsar_action_scheme_from_config()`. |
| R4 | **The report and export crash** with `target = NULL`, with schemes that have no "Protect" label (e.g. Spanish configs), or with a region outside the raster. | [verified] | Guards, plus a `protect_actions` argument derived from the scheme. |
| R5 | **The export's "National" row is the sum of regions; the report uses the true national figure.** For Ghana: 30.08% "met" vs 29.99997% "not met". | [verified] | One shared helper, and a tolerance on `target_met`. |
| R6 | Smaller issues: categorical overlap layers summed as class ids; misleading "not in scheme" warning for categorical input; a region named "National"; `pa_threshold = 0` accepted; `.docx` written with a `.html` name; case-colliding file names. | mostly [verified] | Small. |

**PR #103 (map style):** ready with fixes.
- Some old arguments are silently ignored in the modern style:
  `color_map`, theme objects passed to `elsar_plot_optics()`, and `"tmap"`.
- One example labels raster values in the old Restore/Manage order.

**PR #102 (Pages):** ready with fixes.
- Push builds run in parallel, so an older build can deploy over a newer
  one. Serialise builds per ref.
- Guard the deploy job to `main`.
- Switch the Pages source *before* merging.

---

## 3. Safety net: CI, tests and releases

| Problem | Evidence | Proposal |
|---|---|---|
| **No CI on `develop`.** R CMD check runs only for `main`, so PRs into `develop` (#91, #100) were never checked. | workflow triggers | Add `develop` to push and PR triggers; add a weekly scheduled run (sf/terra/GDAL updates break things). |
| **Coverage 21.9%.** 0% for all zone builders except protect, all plotting, mangroves, wetlands, forest integrity, degraded areas, flood, urban greening and IUCN; `gee_helpers.R` 3.1%. 57 of 74 exports never appear in tests. | covr | A `helper-fixtures.R` with tiny synthetic PUs, rasters and `sf`, and a 1–2 s test per `make_*`. Target 60% on the core. Mock Earth Engine and Drive with `local_mocked_bindings`. vdiffr snapshots for the plot entry points. |
| **The Earth Engine test can never run.** It calls `elsar_download_gee_layer()`, which doesn't exist. | [verified] | Fix and mock it; keep one opt-in live test (`ELSAR_LIVE_GEE=1`). |
| **No tags or releases.** NEWS still heads 0.4.1 as "development version" and has duplicate subsections. The pipeline installs from the default branch, so every merge ships. | `git tag` is empty | Retro-tag v0.4.0 and v0.4.1. Tag every release; `.9000` dev versions on `develop`. Let the pipeline pin `@vX.Y.Z`. Optional: r-universe for Windows/macOS binaries. (The new `release` skill encodes this.) |
| **Branch model is undocumented.** CONTRIBUTING says branch and PR to `main`; practice is mixed (#92–#99 to `main`, #91/#100 to `develop`). | PR history | **Decision needed** (§7). |
| **GitHub hardening:** workflow token defaults to write; no branch protection or rulesets; no Dependabot for Actions; private vulnerability reporting off despite SECURITY.md; merged branches not auto-deleted (`old_functions`, `devel_SN`). | `gh api` | Read-only token by default; rulesets on `main` (and `develop`) requiring R-CMD-check; Dependabot; enable vulnerability reporting; prune branches. |
| **Docs build hides failures and can damage a checkout.** Language-build errors are caught and printed, so CI stays green. Restoring the English sources is not in `on.exit`, so a failure leaves translations over the sources. Each language reinstalls the package (~6 min). | `build-multilingual.R` | Fail on any language error; build each language from a temporary copy; `install = FALSE`. |
| **Translations are stale.** Translated vignettes set `eval = FALSE` (no figures); they still teach `make_custom_projection()`; last updated April vs July for English; `calibrating-weights` is untranslated and untracked. | diff | Freeze translations until the 1.0 API settles. Then generate translated `_pkgdown.yml` from the English file plus string tables, and track source hashes. |
| **Repo size.** About 82 MiB of old `.tif` blobs reachable from `main` (`test.tif` alone is 67 MiB) and about 24 MiB of old `docs/`, on top of the 42 MiB `gh-pages`. | blob scan | Optional, coordinated `git filter-repo`, done together with deleting `gh-pages`. |
| **Hygiene:** `.gitignore` `spat*` ignores any new `R/spatial*.R`; the tarball is 5.35 MB (hex sticker shipped twice, a 678 KB `.nb.html`); `Depends: R (>= 3.5)` is false (Imports need ≥ 4.1); 25 Imports (plyr, here, units, purrr, stringr, png, xml2 … for one or two calls each). | [verified] | Anchor ignore patterns; shrink assets; `R (>= 4.1)`; move rarely used packages to Suggests with `rlang::check_installed()`. |

---

## 4. API, documentation and user experience

**The workflow problem (high).** A country analyst has no supported path
from boundary to an ELSA-ready stack. `get_all_features()` is a stub that
cannot load files, and the template under `inst/rmarkdown/templates/` is
broken: wrong folder layout, and it contains a client notebook with
hard-coded paths and a funder's name. The real path is the private pipeline.
See §6.

**Consistency (high).** The same concept has many names. Examples:

| Concept | Names in use |
|---|---|
| Planning units | `pus`, `pu`, `pu_layer`, `planning_units` |
| Output location | `output_path`, `output_dir`, `figure_path`, `workspace_dir` |
| Human pressure | `hii_threshold`, `human_pressure_threshold` (and "Human Footprint" in some docs) |
| Buffer segments | `nQuadSegs`, `n_quad_segs` |
| Colour | `color_*`, `colour` |
| Threads | `threads = TRUE`, `cores = 4` |

Argument order varies too: `iso3` comes first in the zone builders but
later elsewhere. `iso3` is also required in about 20 functions where it only
names an output file. Agree a glossary, then rename through
`lifecycle::deprecate_soft()`.

**Issue #96 (high, a blocker for real countries).** Agriculture and built
areas cannot be switched off as zone exclusions. Add
`exclude = c("agriculture", "built_areas", "human_pressure")` and a
`zone_thresholds()` helper, so per-zone defaults live in one documented
place.

**Outputs and units (medium).**
- Builders write files as a side effect, with collisions: five functions
  write `built_areas_{iso3}.tif`.
- The naming-conventions vignette promises `<description>_<iso3>_<tier>` in
  lowercase, but nothing writes a tier and `iso3` keeps its case.
- `calculate_restore_and_urban_budgets()` reads file names that nothing
  writes.
- Return types change with arguments (`return_sf`, `return_all`,
  `return_urban_areas`, `raster_df_out`).
- Targets are percentages in one place and fractions in another.

Proposal: no writes inside builders, one return type per function, fractions
everywhere, and file names generated from a layer registry.

**Messages and deprecation (medium).**
- `log_message()` (290+ calls) cannot be silenced; there is no condition
  class on any error.
- Deprecation mixes `.Deprecated()`, lifecycle and silent removal.
- `type = "tmap"` is accepted but does nothing.

Proposal: `cli` with `options(elsar.verbose)`, classed errors, and
lifecycle everywhere.

**Docs accuracy (medium).**
- Wrong types (`pus` documented as a SpatVector) and wrong defaults.
- Documented behaviour that doesn't exist: "if NULL, `make_protected_areas()`
  is used".
- An example that doesn't parse (`make_normalised_raster`).
- 57 of 65 examples sit in `\dontrun{}`, although the README chain runs in
  4 seconds on bundled data.
- `boundary_dat` is credited to GADM but is UN geodata, and the WDPCA-derived
  `current_pas_sf` has no version or terms. That is a **licensing problem**
  for a CC BY package (§7).

**Exports (medium).**
- There are 72 exports, about 106 with the pending work, including generic
  helpers (`split_bbox_into_tiles`, `get_file_type`, `filter_sf`,
  `median_from_rast`, …).
- The reference index files the whole Earth Engine tier under "Helper
  Functions".
- Some helpers are used downstream (`exact_rasterise`, `get_binary_layers`),
  so trim exports through a deprecation cycle, after checking
  `elsar_pipeline`.

**Issue triage:**
- #96: top API priority.
- #13: largely covered by the results branch. Close it on merge and open
  "compare runs (kappa, no-regret)".
- #11: stale; close it.
- #12: rescope to an "add a layer" article.
- #70: an auto-migrated pipeline epic. Close it, or reopen as "1.0: bring
  the pipeline core into elsar".

---

## 5. Earth Engine and external data

Beyond the items in §2:

- **Export region.** Exports are built from only two transformed PU
  corners, which clips curved Mollweide extents: 5% of Chile, 45% of a
  Russia-sized extent.
- **Masked pixels become 0**, which means *Water* in Dynamic World [unverified].
- **Year selection.**
  - The year depends on today's date.
  - An explicitly requested year can be silently replaced with the latest.
  - Nothing records asset versions or the WDPCA release.
- **Cached runs still need the network.** A fully cached rerun still starts
  conda, Python and Earth Engine authentication.
- **Credentials.**
  - Authentication assumes an interactive browser and full Drive scope.
  - There is no service-account path.
  - The Python environment is unpinned.
  - `LD_LIBRARY_PATH` is changed at runtime.
- **CRS handoff.** It depends on the `gdalsrsinfo` command-line tool, which
  is usually missing on Windows/macOS. Send `EPSG:xxxx` instead.
- **Files.**
  - Writes are not atomic: an interrupted write is accepted as a cache hit.
  - Tiles go to `$HOME`, which is OneDrive-synced on many Windows machines.
- **Size.** `gee_helpers.R` is 2,713 lines with six concerns and duplicated
  wait loops, year searches and bounding-box code.

**Redesign:** split it into modules: environment, auth, region, export
engine, sinks, merge, product registry. Then:
- one correct aggregation pipeline;
- a product registry with legends and nodata values;
- a provenance-aware cache (a hash of product, version, grid and reducer,
  plus a JSON sidecar);
- an explicit `elsar_gee_session()`;
- replace the Drive hop with direct downloads or Cloud Storage, which
  removes the filename matching, Drive scope and trash side effects.

---

## 6. Target architecture: elsar 1.0

**Goal:** consistent, dependable building blocks with one contract that the
private pipeline and public users can rely on. The pipeline stays private
(§7), so points 1, 2, 4 and 5 below apply as written, and point 3 is
**deferred**. It is kept here as the option if the pipeline is ever opened.

1. **Region object.** `elsar_region()` holds the projected boundary, planning
   units, `iso3`, CRS and output directory. It is the first argument to
   every builder, replacing the repeated `pus`/`iso3`/`boundary_*`/`output_path`.
2. **Layer registry.** `elsar_layers()` lists each layer's id, group
   (feature/zone/lock-in), builder, default parameters, file stem, datatype,
   source and citation. It replaces `data_summary.csv`, the pipeline's sheet
   columns, `get_all_features()` and the scattered file names. The
   naming-conventions and data-info vignettes are generated from it.
3. **Config and stack builder**, ported from `elsar_pipeline` rather than
   invented:
   - `elsar_init()`;
   - `elsar_config()` (validated YAML);
   - `elsar_build()` (cached, writes `manifest.json` with provenance);
   - `elsar_check_stack()` (alignment, ranges, zone vs lock-in);
   - `elsar_export_stack()` (for elsa-config-mapper and the ELSA tool).

   The pipeline becomes a thin app, Docker and upload layer over the public
   API.
4. **Two shared primitives.** These fix most of §2 by construction:
   - `align_to_pus(x, region, type = c("continuous", "proportion", "categorical"), fun)`:
     area-weighted, with a fast path when the input is already aligned;
     normalisation becomes a separate, explicitly ordered step.
   - `vector_coverage(sf, region, buffer_points, area_col, area_units)`:
     transform, make valid, dissolve, coverage. It replaces six divergent
     copies in protected areas, KBAs, Ramsar, mangroves, indigenous lands
     and IUCN forests.
5. **Results and maps.** The results tools and the PR #103 style API, with
   argument names aligned to the glossary.

```r
cfg   <- elsar_config("elsa_npl/elsa.yml")
stack <- elsar_build(cfg)                        # features, zones, lock-ins; cached
elsar_check_stack(stack)
elsar_export_stack(stack, "elsa_npl/upload")
# after the ELSA run
elsar_export_results("04_ELSA_Action.tif", dir = "elsa_npl/post",
                     regions = admin1, region_col = "adm1nm",
                     scheme = elsar_action_scheme_from_config("config.json"))
```

### Release plan

| Release | Content |
|---|---|
| **0.4.2** (patch, now) | Everything in §2. Fix R1–R5 before the results branch merges. Retro-tags. CI on `develop`. Fix the GEE test. Delete or repair the broken template. |
| **0.5** (non-breaking) | Results tools + map style merged (names aligned to the glossary). #96 `exclude =`. LULC product checks. Glossary aliases with soft deprecation. Lifecycle everywhere. Docs-accuracy pass and runnable examples. Reference index by workflow stage. CI hardening and Dependabot. Imports diet.  PostgreSQL: `elsar_load_data()` accepts an open DBI connection, a PostgreSQL service name (`pg_service.conf`), or the standard `PG*` environment variables, so passwords never appear in scripts; `make_postgres_connection()` (restored in 0.4.2 for the pipeline) deprecated with a pointer to that.|
| **0.6** (the big one) | `align_to_pus()` and `vector_coverage()`. `elsar_region()`. Layer registry and `elsar_write()`. `elsar_check_stack()` (a QA step the pipeline can call). Earth Engine module split, correct aggregation and provenance cache. cli messaging. Deprecate `get_all_features()`, variable return types and the generic helper exports. |
| **0.7** | Pipeline moved onto the 0.6 contract, in its own repo. "Run one country on example data" and "Setup" articles. Old plotting trio deprecated in favour of the map-style building blocks. |
| **1.0** | Remove the deprecated functions. Freeze config schema v1. Refresh translations. |

---

## 7. Decisions

### Decided (November 2026)

All six decisions are made.

1. **Branch model: `develop` → `main`.**
   - Feature and fix PRs go into `develop`.
   - `main` only receives release PRs, plus agreed hotfixes, which are then
     merged back into `develop`.
   - Follow-ups:
     - run CI on `develop` (§3);
     - make `develop` the default branch for PRs;
     - protect both branches;
     - update CONTRIBUTING, which currently says `main`.
2. **The pipeline stays private.** elsar remains a set of building blocks,
   and §6 is scoped accordingly. Because elsar's public API is the
   pipeline's contract, tagged releases and pinning (§3) matter more, not
   less.
3. **Licence: Apache-2.0 for code (proposed).**
   - It needs explicit agreement from both authors, and from anyone else
     with a substantial code contribution.
   - It adds a patent grant and is OSI-approved. Documentation and data keep
     their own terms.
   - Steps: `usethis::use_apache_license()`, a `NOTICE` file, `cph` (UNDP)
     and `fnd` roles in `Authors@R`, and a NEWS entry.
4. **Bundled real data will be replaced.**
   - Redistribution of the UN boundaries and the WDPCA-derived data probably
     can't be cleared.
   - Ship a public-domain or synthetic boundary (Natural Earth, or a
     synthetic country like the results branch's "Elsaland") and **dummy
     protected areas** generated by a script in `data-raw/`.
   - Switch examples, tests and vignettes to them. Document the real-data
     workflow as "download it yourself", with citations.

5. **One release for the value-changing fixes.**
   - All of §2 (items 1–13) ships together as 0.4.2. NEWS lists every layer
     whose values change and must be rebuilt.
   - Before 0.4.2 merges to `main`: tag v0.4.1 and pin `elsar_pipeline` to
     `elsar@v0.4.1`.
   - The pipeline then moves to 0.4.2 deliberately, country by country, when
     a rerun is planned.
6. **Translations are frozen until the API is stable (1.0).**
   - Don't update `translations/` meanwhile.
   - Add a banner to the translated sites: "This translation describes
     elsar 0.4; see the English site for current documentation."
   - Relax the translation-staleness CI check to informational until the
     freeze lifts.
   - When it lifts: generated translated `_pkgdown.yml`, source-hash drift
     tracking, and no `eval = FALSE` (§3).

## 8. Agent tooling added on this branch

- `CLAUDE.md`: the sibling repos, API stability rules, commands, and the
  geospatial invariants that caused the bugs above.
- `.claude/settings.json`:
  - **allowed:** read-only git and gh commands, test and check commands, and
    reading sibling repos;
  - **denied:** reading credential files;
  - **asks first:** push, PR creation or merge, and releases.
- `.claude/hooks/check-file.sh`: after each edit, checks that R files parse,
  that package code is ASCII-only, and that `DESCRIPTION` and
  `_pkgdown.yml` parse.
- `.claude/agents/geospatial-reviewer.md`: reviews diffs against the
  invariants and tries to break them on synthetic data.
- `.claude/skills/`:
  - `check-package`: document, test and check against the known baseline;
  - `add-layer`: the `make_*` contract;
  - `release`: tags, NEWS and pinning.

The full reviewer reports, with every finding, file:line and reproduction
script, are kept outside the repo. They can be added as an appendix if
wanted.

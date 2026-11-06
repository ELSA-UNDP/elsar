# CLAUDE.md

Guidance for Claude Code (and humans) working in **elsar**, the R package that
builds the data stack for ELSA (Essential Life Support Areas) spatial
prioritisation. It also has tools to calibrate weights and to analyse ELSA
results. Human contributor rules are in `CONTRIBUTING.md`; this file adds what
an agent needs to work safely.

The current audit and roadmap is `notes/proposals/2026-11_audit_and_roadmap.md`.
Read it before any structural work.

## What elsar is part of

| Repo (`~/gitrepos/`) | Relationship to elsar |
|---|---|
| `elsar_pipeline` | **Main consumer; private, and staying private.** Runs elsar per country (Docker) and owns the orchestration: config, layer list and stack building. It installs `ELSA-UNDP/elsar` from the default branch **unpinned**, so whatever reaches `main` ships in its next build. |
| `elsa_national` | Shiny app that solves ELSA. It copies `get_target()`, `get_coverage()` and `log_message()` from elsar (elsar is canonical) and writes the action rasters that elsar's results tools read. |
| `elsa-config-mapper` | Produces the ELSA `config.json` (actions, `rasterValue`s, colours, fallbacks). |
| `unbl-elsa` | UN Biodiversity Lab hosted ELSA. Reference implementation of the solve. |

**API stability.** Exported functions are used downstream (`make_*`,
`elsar_load_data`, `exact_rasterise`, `get_binary_layers`, `get_coverage`,
`elsar_calibrate_weights`, `elsar_plot_feature`, ...). Never rename, remove,
un-export or change the meaning of an argument without a `lifecycle`
deprecation and a NEWS entry. Before you do, grep `../elsar_pipeline` and
`../elsa_national` for the function.

## Branches and PRs

- **`develop` → `main`.** Branch from `develop` and open feature and fix
  PRs into `develop`. `main` only receives release PRs (`develop` → `main`)
  and agreed hotfixes, which are then merged back into `develop`.
  CONTRIBUTING still says `main`; it is being updated.
- R CMD check CI runs only for `main` (pushes and PRs), so a PR into
  `develop` gets no checks. Run the `check-package` skill yourself.
- One logical change per PR. Commit or push only when asked.
- Pending work may be stacked (e.g. a PR whose base is another feature
  branch). Check `gh pr list` before starting something that overlaps.

## Commands

```bash
Rscript -e 'devtools::document()'                       # after any roxygen edit; commit man/ + NAMESPACE
Rscript -e 'devtools::test()'                           # full suite, ~2 min, no network needed
Rscript -e 'devtools::test(filter = "elsar_results")'   # one test file
Rscript -e 'devtools::check(vignettes = FALSE, args = "--no-manual", error_on = "never")'
```

Expected noise: one warning from `make_boundary()` (`st_cast` "repeating
attributes") in `test_correctness_fixes.R`, and a GEE test that is skipped
without `GEE_PROJECT`. Anything else is new.

To render a vignette against the working copy without touching the user's
installed elsar, install to a scratch library first:
`R CMD INSTALL --no-test-load -l <tmp>/lib .` then
`R_LIBS=<tmp>/lib Rscript -e 'rmarkdown::render("vignettes/x.Rmd", output_dir = "<tmp>")'`.

## Layout

- `R/make_*.R`: build one normalised layer or zone on the planning-unit grid.
  `make_normalised_raster()` is the shared last step.
- `R/gee_helpers.R`, `R/elsar_setup_gee.R`, `R/lulc_config.R`: Earth Engine
  downloads (need credentials; never run them in tests).
- `R/elsar_plot_*.R`, `R/utils-plotting.R`: plotting.
- `R/calibrate_weights.R`: pre-calibration of feature weights.
- `R/elsar_results*.R` (pending, branch `feature/elsa-results-analysis`):
  post-analysis of ELSA action maps (summaries, protection gap, overlap,
  plots, export, report template).
- `inst/extdata/`: small example data used by examples and tests;
  `data-raw/` holds the scripts that build it.
- `translations/`: es/fr/pt/ru copies of README, vignettes and
  `_pkgdown.yml`. **Frozen until 1.0** (decided Nov 2026): do not edit
  them; change only the English source.
- `notes/`: internal notes and proposals (build-ignored).

## Geospatial invariants and traps

These have each caused real bugs here. Check them in every change.

- **Areas.** Never use `res(x)^2`. Use `terra::cellSize(x, unit = "m",
  transform = FALSE)`: planar for projected rasters (ELSA uses equal-area
  Mollweide via `make_custom_mollweide_projection()`, so 1 km cells are
  exactly 100 ha) and geodesic for lon/lat. Avoid area maths in Web Mercator.
- **Region weighting.** Cells cut by a polygon are weighted by
  `exactextractr` coverage fractions; take cell areas from terra.
- **ELSA action raster values.** The ELSA tool writes 1 Protect, 2 Manage
  ("Reduce Pressures"), 3 Restore, 4 Urban Greening ("Urban Adapt"), then any
  fallback zones (e.g. 5 Avoid Loss), and Do Nothing **last**. Runs differ,
  so read the run's `config.json` (`elsar_action_scheme_from_config()` on the
  results branch) rather than assuming the order.
- **Empty inputs are normal.** Some countries have no usable protected areas
  (Somalia), KBAs or mangroves. Return a well-formed all-0 layer within the
  planning units and warn; don't error. Handle 0-row `sf`.
- **Geometry validity.** Run `sf::st_make_valid()` *after* `st_transform()`;
  transforming can invalidate geometries.
- **Sub-national codes.** `iso3` may be `"USA_CA"`; use `iso3_base()` to
  filter global datasets and `is_valid_iso3()` to validate.
- **terra quirks.** `levels(x) <- df` renames the layer (set `names()` after
  it). `levels()` returns only the active column; use `terra::cats()` for all.
  `terra::project()` of categorical or fractional data needs an explicit
  `method` ("near" or "average").
- **exactextractr quirk.** With `summarize_df = TRUE`, `include_cols` arrive
  as columns of the data frame passed to `fun`. They are not appended to
  the result.
- **R quirk.** `ifelse(test, a, b)` returns `length(test)` values; recycle
  scalars first.
- **Memory.** Countries reach tens of millions of cells. Prefer
  `terra::zonal()`, `global()` and on-disk processing to `values()` on
  whole rasters.

## Conventions (summary; details in CONTRIBUTING.md)

- British English (`normalise`, `colour`); snake_case; tidyverse style.
- Prefixes: `make_*` builds a layer, `get_*` fetches or derives, `download_*`
  pulls from GEE, `elsar_*` everything else (plots, setup, results).
- Validate inputs with `assertthat::assert_that(..., msg = )`; report
  progress with `log_message()`.
- Call elsar's own functions bare (no `elsar::`); qualify other packages'
  functions with `pkg::`.
- R code must be ASCII: write `"²"`, not `"²"` (R CMD check WARNING).
  A hook flags this.
- Every exported function: roxygen with `@param`, `@return`, and a runnable
  `@examples` when it can run on bundled data. Add new exports to
  `_pkgdown.yml` (the site build fails on missing topics) and user-visible
  changes to `NEWS.md`.
- Tests: testthat 3e, no network or credentials. Use small synthetic grids in
  an equal-area CRS (e.g. `ESRI:54009`, 1 km cells) so expected areas are
  exact. A bug fix comes with a test that fails without it.

## Secrets

GEE credentials, `.httr-oauth`, `.Renviron`, service-account JSON and
database passwords must never be read into the conversation or committed.
`.claude/settings.json` denies reading the usual locations.

## Project agents and skills

- `geospatial-reviewer` agent: reviews a diff against the invariants above.
  Use it before committing changes to `R/`.
- `check-package` skill: document, test and check, and interpret the results.
- `add-layer` skill: recipe for a new `make_*` layer function.
- `release` skill: `develop` → `main` release checklist.

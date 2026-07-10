# Contributing to elsar

Thanks for taking the time to contribute. This document explains how to file a
useful issue, how to propose a change, and the conventions the package follows so
your pull request can be merged smoothly.

If you only want to *ask for help* or *report a problem*, see
[SUPPORT.md](SUPPORT.md) — you don't need to read the rest of this page.

## Ways to contribute

- **Report a bug** — [open a bug report](https://github.com/ELSA-UNDP/elsar/issues/new?template=bug_report.yml). A reproducible example is worth more than a long description; see [SUPPORT.md](SUPPORT.md#before-you-open-a-bug-report).
- **Suggest a feature** — [open a feature request](https://github.com/ELSA-UNDP/elsar/issues/new?template=feature_request.yml).
- **Improve documentation** — fixes to roxygen docs, vignettes, or this file are very welcome and are often the easiest first contribution.
- **Fix a bug or add a feature** — please open (or comment on) an issue first, so we can agree on the approach before you invest time. For anything beyond a typo, a quick "I'd like to work on this" comment avoids duplicated effort.

## Development setup

You'll need a working geospatial R toolchain. In R:

```r
# install.packages("devtools")
devtools::install_deps(dependencies = TRUE)  # from the package root
devtools::load_all()                          # load the in-development package
```

System libraries (GDAL, GEOS, PROJ, sqlite3, udunits, libxml2) come with `sf`
and `terra`. The Tier B and Tier C workflows have further requirements — a
Chromium browser and `libarchive` for protected areas, and a conda/Python
environment plus a Google Earth Engine account for the `download_*` functions.
See [SUPPORT.md](SUPPORT.md#what-we-support). You do **not** need any of those to
work on the core package.

## Making a change

1. **Fork** the repo and create a branch off `main` with a descriptive name
   (e.g. `fix/planning-units-crs-check`, `docs/setup-vignette`).
2. Make your change. Keep the diff focused — one logical change per pull request
   is much easier to review than a mixed bag.
3. **Document and test** (see conventions below).
4. Run the checks locally:

   ```r
   devtools::document()   # regenerate man/ and NAMESPACE if you touched roxygen
   devtools::test()       # run the test suite
   devtools::check()      # R CMD check — aim for no new ERRORs/WARNINGs/NOTEs
   ```
5. **Open a pull request** against `main`, fill in the PR template, and link the
   issue it addresses (e.g. "Closes #123").

## Conventions

Matching the surrounding code makes review faster. elsar follows these:

### Style
- **snake_case** for function and argument names, and the
  [tidyverse style guide](https://style.tidyverse.org/) generally.
- **British English** in prose and identifiers (`normalise`, `harmonise`,
  `colour`) — `DESCRIPTION` declares `Language: en-GB`.
- Reference other packages' functions with `pkg::fun()`. Do **not** qualify
  elsar's own internal functions with `elsar::` — call them bare so
  `devtools::load_all()` works during development.

### Naming
The package uses consistent prefixes — please keep to them:

| Prefix | Purpose | Example |
|---|---|---|
| `make_*` | Build a layer (usually returns a `SpatRaster`) | `make_protect_zone()` |
| `get_*` | Fetch or derive data | `get_coverage()` |
| `download_*` | Pull a layer from Earth Engine | `download_lulc_data()` |
| `elsar_plot_*` | Plotting | `elsar_plot_static_raster()` |

Argument names should match existing functions for the same concept
(`iso3`, `pus`, `output_path`, `human_pressure`, …). If you're adding an argument
that already exists elsewhere, reuse its name rather than inventing a synonym.

### Documentation
- All exported functions use [roxygen2](https://roxygen2.r-lib.org/) with
  markdown. Every exported function needs `@param` for every argument, a
  `@return`, and — wherever it can run without credentials or large downloads —
  a runnable `@examples` block. Wrap examples that need GEE, a network, or big
  data in `\dontrun{}`.
- Run `devtools::document()` and commit the regenerated `man/*.Rd` and `NAMESPACE`
  in the same PR. Don't hand-edit files under `man/`.
- User-visible changes get a bullet in `NEWS.md` under the top (development)
  heading.

### Tests
- Tests live in `tests/testthat/` and use [testthat](https://testthat.r-lib.org/)
  edition 3.
- New behaviour needs a test; a bug fix should come with a test that fails
  before the fix and passes after.
- Tests must not require credentials, network access, or large downloads to run.
  Use the bundled example data, and `skip_if_not_installed()` / `skip_on_cran()`
  for anything that depends on an optional (`Suggests`) package or an external
  service.

## Translations

The package website is published in several languages from `translations/`. If
your change edits `README.md`, a vignette, or `_pkgdown.yml`, the English source
is what you change here — the translated copies are updated separately, and CI
will flag when they've drifted. You don't need to update translations yourself.

## Code of conduct

By contributing, you agree to abide by our
[Code of Conduct](CODE_OF_CONDUCT.md). Please be respectful and constructive.

## Licence

elsar is released under [CC BY 4.0](LICENSE.md). By contributing, you agree that
your contributions will be licensed under the same terms.

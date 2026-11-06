---
name: check-package
description: Document, test and R CMD check the elsar package, and interpret the results against the known baseline. Use after changing R/, roxygen comments, DESCRIPTION, tests or _pkgdown.yml, before committing, or when asked to "check" or "test" the package.
---

# Check the package

Run these in order and stop at the first real failure.

## 1. Documentation

```bash
Rscript -e 'devtools::document()'
```

Any `✖ Could not resolve link to topic` on the first run after adding a
function usually clears on a second run. If it persists, the link is wrong.
Commit the regenerated `man/` and `NAMESPACE` with the change.

## 2. Tests

```bash
Rscript -e 'devtools::test()'                        # ~2 minutes
Rscript -e 'devtools::test(filter = "<file stem>")'  # one file while iterating
```

Report the `[ FAIL | WARN | SKIP | PASS ]` line.

**Known baseline:** 1 warning (`make_boundary()` "repeating attributes for all
sub-geometries" in `test_correctness_fixes.R`) and 1 skip (GEE download,
needs `GEE_PROJECT`). Anything beyond that is new and must be explained.

When a test fails, read the expectation before changing it. Change an
expectation only when the old value was wrong, and say so.

## 3. R CMD check

```bash
Rscript -e 'devtools::check(vignettes = FALSE, args = "--no-manual", error_on = "never")'
```

Report every ERROR, WARNING and NOTE. Common causes here:

- **Non-ASCII characters in R code**: use `"²"` escapes.
- **"no visible binding for global variable"**: use `.data$col` in dplyr, or
  add the name to `R/globals.R`.
- **"'::' or ':::' import not declared"**: add the package to `Imports` (or
  `Suggests` plus `requireNamespace()` for optional use).
- **Example errors**: examples must run on bundled data (`inst/extdata`,
  `data/`) or be wrapped in `\dontrun{}` (network or credentials) or
  `\donttest{}` (slow).

## 4. Site (only if exports or `_pkgdown.yml` changed)

```bash
Rscript -e 'pkgdown::check_pkgdown()'
```

Every exported topic must appear in a `reference` section of `_pkgdown.yml`.

## Report

One short paragraph: what ran, the result lines, and anything new compared
with the baseline, with its cause.

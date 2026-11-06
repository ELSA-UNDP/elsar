---
name: add-layer
description: Add a new make_* function that builds one ELSA feature or zone layer on the planning-unit grid (e.g. a new global dataset), following elsar's input, output, naming, testing and documentation conventions. Use when asked to add, port or wrap a new data layer.
---

# Add a layer function

Copy the shape of an existing, recently fixed function before inventing your
own: `R/make_kbas.R` (polygons and points), `R/make_protected_areas.R`
(empty inputs, `return_sf`) or `R/make_mangroves.R` (a simple global layer).

## 1. Contract

```r
make_<layer> <- function(<layer>_in, pus, iso3, ..., output_path = NULL)
```

- **Inputs:** the source data (`sf` or `SpatRaster`, or a path), `pus` (the
  planning-unit `SpatRaster`), and `iso3`. Reuse existing argument names for
  the same concept (`pus`, `iso3`, `output_path`, `buffer_points`,
  `area_column`, `threshold`); don't invent synonyms.
- **Validate first** with `assertthat::assert_that(..., msg = )`: types,
  `is_valid_iso3(iso3)`, and that `output_path` exists if given.
- **Filter global data with `iso3_base(iso3)`**, so sub-national codes like
  `"USA_CA"` work.
- **Geometry:** `st_transform()` to the `pus` CRS, then `st_make_valid()`,
  then dissolve (`dplyr::summarise()`) so overlaps are not double counted.
- **Rasterise** with `exactextractr::coverage_fraction(pus, x)[[1]]` (polygons)
  or `exact_rasterise()` (attribute-weighted), then finish with
  `make_normalised_raster(raster_in, pus, iso3, name_out, output_path)`.
  Call it bare, not as `elsar::make_normalised_raster`.
- **Empty is normal:** if nothing is left after filtering, return
  `terra::ifel(pus == 1, 0, NA)` with a `warning()` that says why (see
  `test_empty_protected_areas.R`).
- **Output:** a single-layer `SpatRaster` named `<description>`. When
  `output_path` is set, write `<description>_<iso3>.tif` (lowercase, see
  `vignettes/naming_conventions.Rmd`) and still return the raster.
- Report progress with `log_message()`. Keep R code ASCII.

## 2. Test (`tests/testthat/test_make_<layer>.R`)

Use a small synthetic `pus` grid in an equal-area CRS (`ESRI:54009`, 1 km
cells) and hand-made `sf` features, so expected values are exact. Cover:

- a normal case with a known result (values in 0-1, NA outside `pus`);
- the empty case (no features for the ISO3, or all filtered out);
- a sub-national `iso3`;
- points with and without an area attribute, if the function buffers points.

No network or credentials. If the source must be downloaded, test only the
processing on a fixture, and keep the download in a separate `get_*` or
`download_*` function.

## 3. Wire it in

1. Roxygen: title, description, `@param` for every argument, `@return`,
   `@export`, and an example (`\dontrun{}` if it needs a download).
2. `Rscript -e 'devtools::document()'`.
3. Add the function to the "Prepare Feature Data" section of `_pkgdown.yml`.
4. Add a NEWS bullet under the development heading.
5. If the layer belongs in the standard stack, say so in the PR. The
   `elsar_pipeline` repo has to opt in separately.
6. Run the `check-package` skill, then ask the `geospatial-reviewer` agent to
   review the diff.

# elsar per-function review, group 01: region, I/O and shared utilities

Code: `/home/scottca/gitrepos/elsar-audit` at 54522b7, which is develop/0.4.1 plus the audit doc. None of these files differ on
`feature/elsa-results-analysis`. Downstream: `/home/scottca/gitrepos/elsar_pipeline` on branch `feat/sidecar-provenance`.
"Audit" means `notes/proposals/2026-11_audit_and_roadmap.md`.

Every finding marked [verified] was reproduced with a scratch script. The scripts are in
`(scratch, not committed)/scripts/` (t1–t5.R).
Environment: terra 1.9.34, GDAL 3.8.4, GEOS 3.12.1, PROJ 9.3.1.

---

## Region

### `make_boundary()`  (R/make_boundary.R:33)
**Does:** filters, optionally dissolves or limits to the mainland, transforms to WGS84, then optionally to a custom Mollweide CRS. **Used by:** elsar_pipeline has 1 call (`R/02_planning_units.R:229`). It passes `boundary_in, iso3, iso3_column, filter_by_iso3, dissolve` and never `limit_to_mainland`, `col_name` or `input_type`. Inside elsar it is only used in examples, the vignette and tests.
**Verdict:** fix. It is the entry point of every run, and two argument combinations give the wrong region.
**Issues:**
- **`limit_to_mainland = TRUE` + `dissolve = TRUE` keeps one province, not the mainland.** The mainland step (l.113) runs before dissolve (l.131). It casts each feature to POLYGON and keeps the largest, so a multi-province region shrinks to its largest province. [verified] Two adjacent 1°×1° provinces plus an islet gave 12,364 km² instead of about 24,700 km² (t4.R).
- **With `dissolve = FALSE`, a multi-feature input stays multi-row**, and `make_planning_units()` then crashes on it (see that card). [verified] 3 features in gave `nrow 3` out. Country files with several rows per ISO3 (disputed areas, admin-1 sources) hit this on the national path.
- `input_type` is redundant: it must agree with `class(boundary_in)`, and the class can be inferred.
- `custom_projection` is a logical, so the caller cannot pass their own CRS (a national equal-area CRS, or EPSG:54009).
- `st_make_valid()` runs only when `dissolve = TRUE`, so invalid boundaries go straight to rasterisation.
- The `iso3` check is `is.string`. Other functions use `is_valid_iso3()`.
- `limit_to_mainland` uses `st_cast("POLYGON")` on the attributed sf, which warns "repeating attributes" for each multi-part feature (code).
- The antimeridian bug is audit §2 #8.
**Improvements:**
- Order the steps filter → dissolve → mainland, and make the boundary single-feature by default (`dissolve = TRUE`, or always union before returning). Add a test with two provinces and an island. S
- Drop `input_type` and dispatch on class (deprecate the argument). S
- Accept `crs = NULL | "auto_mollweide" | <crs>` in place of the logical `custom_projection`. M
- Always `st_make_valid()` once, after filtering. S
- Drop the WKT side-file write (`output_path`). Return the CRS as an attribute or leave it to `elsar_region()` (audit §6.1). S

### `make_planning_units()`  (R/make_planning_units.R:36; local helpers `round_clean` l.74, `get_increment` l.87)
**Does:** rasterises the projected boundary into a 1/NA grid, auto-sizing cells to stay under `pu_threshold`. **Used by:** elsar_pipeline has 2 calls (`R/02_planning_units.R:295,322`). They pass `boundary_proj, pu_threshold | pu_size, limit_to_mainland, iso3`. Not called inside elsar (vignette and tests only).
**Verdict:** fix. A pipeline option silently does nothing, multi-feature boundaries crash, and the grid can miss the east and north edges.
**Issues:**
- **`limit_to_mainland` is accepted and ignored** ("Reserved for future use", never referenced). The pipeline passes `config$planning_units$limit_to_mainland` here, and never to `make_boundary()`. Its 10 country configs document it as "Exclude islands from PU grid" (e.g. `config/usa_ca_config.yaml:18`, `defaults.yaml:51`). Setting it to `true` has no effect. [code]
- **Multi-feature boundary crashes.** `st_area(boundary_proj)` returns a vector, so `round_clean()`'s `if (x < 50)` gives "the condition has length > 1". [verified] 2-feature boundary (t4.R).
- **The grid does not cover the boundary's east and north edges.** `terra::rast(resolution = pu_size, extent = ext(boundary))` keeps xmin/ymin and truncates xmax/ymax to whole cells. [verified] A circle of radius 1,200 km gave an extent ending at 1,198,900, not 1,200,000, so up to one cell of boundary is dropped on two sides. The origin is also arbitrary (not snapped to a multiple of `pu_size`).
- `touches = TRUE` is hard-coded. It is defensible (cover every boundary cell), but PU area exceeds boundary area. The pipeline's cache check tolerates 10% for this (`pus_boundary_mismatch`).
- Rounding steps (`round_clean`) and search steps (`get_increment`) use different bands. Example: 450 m rounds in 50 m steps but searches in 25 m steps, so sizes like 425 m appear. Cosmetic.
- `iso3` is required even when nothing is written (audit §4).
- Uses `plyr::round_any` for one ceiling (audit §3 Imports diet). `units::drop_units` is used once.
**Improvements:**
- Snap the extent outward: `ext <- terra::align(terra::ext(b), pu_size, snap = "out")`, or `xmin = floor(xmin/pu_size)*pu_size` etc. Add a test that every boundary vertex falls in a non-NA cell. S
- Union the boundary or sum `st_area` before estimating; error clearly on an empty boundary. S
- Either implement `limit_to_mainland` (delegate to the boundary step) or deprecate it with a warning when `TRUE`. Tell the pipeline to pass it to `make_boundary()`. S
- Replace the search loop with a closed form. The count is about area/size², so `size = ceiling_to_step(sqrt(area/threshold))`, then one verification rasterisation and at most one step down. Today's loop is cheap (3 iterations, 0.2 s in the test), so this is for clarity, not speed. S
- `plyr::round_any(x, s, ceiling)` → `ceiling(x/s)*s`. S
- `iso3` optional; drop the file write once `elsar_write()` exists. S

### `make_custom_mollweide_projection()` / `make_custom_projection()`  (R/make_custom_mollweide_projection.R:24 / :85)
**Does:** builds a WKT1 Mollweide string centred on the boundary bbox. **Used by:** elsar_pipeline has 0 calls (it gets the CRS through `make_boundary()`). Inside elsar: `make_boundary()`. The alias `make_custom_projection` has no callers; the translated vignettes still use it (audit §3).
**Verdict:** fix (audit §2 #8: antimeridian and false_northing) and simplify. Remove the deprecated alias at 0.6 or 1.0 through lifecycle.
**Issues:**
- Antimeridian and `false_northing = ymid` are known (audit §2 #8). [re-verified] proj4 `+lon_0=2.55 +y_0=2.55`: the false northing is the centre latitude in *metres*.
- `iso3_column` is a dead parameter on both functions (never used).
- Returns a `glue` object with embedded newlines, not a character CRS. [verified] `class() = "glue" "character"`. It works with `st_crs()`, but it is an odd return type for an exported function.
- The alias uses `.Deprecated()`, not lifecycle (audit §4 deprecation mix).
**Improvements:**
- Replace the 8-line WKT template with `sf::st_crs(sprintf("+proj=moll +lon_0=%.6f +datum=WGS84 +units=m", lon0))$wkt`. Compute `lon0` with a shared antimeridian-safe helper (also needed by `gee_helpers.R:962`). Return `sf::st_crs` or plain character. S
- Remove `iso3_column` (deprecate the argument). S
- Make it internal once `elsar_region()` lands. Keep it exported until then, because users call it directly in the README chain. S
- Test: Fiji/Kiribati boundary → `lon_0` near ±180 and width < 3,000 km. S

---

## I/O

### `elsar_load_data()`  (R/elsar_load_data.R:66; internal `combine_layers` l.236)
**Does:** one entry point for rasters, vector files, multi-layer GPKG/GDB, folders of shapefiles and PostGIS tables, with optional ISO3 and spatial filters. **Used by:** elsar_pipeline has 2 direct calls (`02_planning_units.R:176`, `utils_pipeline.R:584`) and about 38 calls through `safe_load_raster/vector`. Arguments used: `file_name, file_path, file_lyr, iso3, iso3_column, wkt_filter, db_info`. `pg_connection` and `drop3d` are never used. Inside elsar: `get_all_features()` (7 calls).
**Verdict:** refactor. The behaviour depends on which path is taken, and the PostGIS path skips the processing the file path applies.
**Issues:**
- **The PostGIS branch returns raw `st_read()` output.** It ignores `wkt_filter`, `drop3d` and geometry repair, so the same layer comes back different from Postgres than from a GPKG. [code] l.155 returns before any of that processing.
- **`iso3` without `iso3_column` silently skips the filter** (both paths). [verified] `elsar_load_data("x.gpkg", td, iso3 = "AAA")` returned all 3 rows, including "BBB".
- `db_info` and `pg_connection` are duplicate arguments.
- PostGIS is selected by the magic value `file_name = "postgres"` together with `file_path = NULL`. Passing a `file_path` makes it fall into `get_file_type(".../postgres")`, which fails with "Unsupported file extension".
- "`file_name = NULL` loads all files of a type in a folder" holds only for shapefiles. A folder of rasters errors in `get_file_type()`.
- The return type varies (sf or SpatRaster, or several layers bound into one sf). Binding all layers of a GPKG with mismatched schemas into one sf is rarely what a caller wants.
- `wkt_filter` is a misnomer: it takes an object and uses its *extent*. I tested whether the 4-corner bbox transformed to EPSG:4326 clips curved Mollweide extents (the audit §5 GEE issue). It does not for a Mollweide grid: parallels are straight, and the side edges lie outside the data. [verified] 0% of a 30–100°E × 50–70°N region fell outside the filter. No issue, noted so it is not re-raised.
**Improvements:**
- Split into `elsar_read_raster()` and `elsar_read_vector(src, layer, where = list(col = value), bbox = NULL)`, where `src` is a path or a DBI connection, with one shared post-processing step (zm drop, make_valid, filter). Keep `elsar_load_data()` as a dispatcher. M
- Error when `iso3` is given without `iso3_column`, or default `iso3_column` from a small known-column list (`iso3`, `ISO3`, `iso3cd`, `GID_0`), with a message. S
- Deprecate `pg_connection` in favour of `db_info`, or accept a live DBI connection. S
- Raster folders: `file_name = NULL` + rasters → `terra::rast(list.files(..., "\\.tif$"))`, or error clearly. S
- Rename `wkt_filter` → `extent_filter` (soft-deprecate). S
- Tests: PostGIS path parity is mockable with `local_mocked_bindings(st_read = ...)`. M

### `filter_sf()`  (R/utils.R:499)
**Does:** reads one vector layer with an optional OGR-SQL ISO3 filter and a spatial filter, then drops Z/M and repairs geometry. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `elsar_load_data()` (3 calls) and `make_protected_areas.R:225`.
**Verdict:** make internal, and fix the SQL handling.
**Issues:**
- **A wrong `iso3_column` gives 0 rows silently on GeoPackage.** SQLite reads an unknown `"col"` as a string literal. [verified] `iso3_column = "isoX"` → 0 rows, no error.
- **Every filtered read warns** "argument layer is ignored when query is specified", because both `layer` and `query` are passed (l.555–556). [verified]
- The ISO3 literal is not escaped. [verified] `iso3 = "O'X"` gives a GDAL SQL syntax error, which is swallowed into a `NULL` return. It is low risk, but the PostGIS path already quotes properly.
- Matching is case-sensitive and uses the full code. [verified] `"aaa"` → 0 rows. A sub-national `"USA_CA"` must be reduced with `iso3_base()` by every caller; `elsar_load_data()` does not do this.
- `file_type` is a dead parameter (documented, never read).
- `st_layers()` is called twice when a geometry filter is given.
- `st_make_valid()` always runs on the full result. On WDPCA- or GAUL-sized layers this is the slowest step, and under `sf_use_s2(TRUE)` on lon/lat data it applies s2 repair semantics (UNVERIFIED cost).
- The "Applying a spatial filter" log line only appears when there is no query.
**Improvements:**
- Validate `iso3_column` against the field names first (read 1 feature, as the pipeline's `vector_field_names()` does). Drop `layer` when `query` is set. Escape `'` → `''`. Uppercase both sides (`WHERE upper("col") = 'USA'`). S
- Remove `file_type`; read `st_layers()` once. S
- Make it internal (`@keywords internal`, deprecate the export). S
- Make repair optional (`repair = TRUE`) so callers that union afterwards can skip it. S

### `get_file_type()`  (R/utils.R:457)
**Does:** maps a path to a format string, scanning folders for `.shp`. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `elsar_load_data()` only.
**Verdict:** make internal; widen it or replace it with GDAL driver detection.
**Issues:**
- [verified] `x.GDB` (uppercase) and `y.gdb/` (trailing slash) both give "Cannot determine file type from folder contents". The `.gdb` test is case-sensitive and anchored on the raw string, while the extension branch lowercases.
- [verified] `.vrt`, `.fgb`, `.zip`, `.parquet` are rejected. VRT especially is common for tiled global rasters. GDAL can open all of these.
- A folder holding a GPKG or rasters errors; only `.shp` is scanned.
**Improvements:** normalise the path (`sub("/+$", "", path)`, `tolower`). Classify raster vs vector by trying `terra::describe()` / `sf::st_layers()`, or use GDAL's driver detection. Return `"raster"`/`"vector"`/`"postgres"` rather than an extension, since the extension is only used for that split. Make it internal. S

### `extract_filename_filetype()`  (R/utils.R:163)
**Does:** finds a file in a folder by regex and splits it into name and extension. **Used by:** elsar_pipeline has 0 calls. Inside elsar: only the stub `get_all_features()` (7 calls; audit §4 says to deprecate it).
**Verdict:** deprecate together with `get_all_features()`.
**Issues:** [verified] With `kba.gpkg, kba_points.shp, kba_points.dbf, nokba.tif`, `extract_filename_filetype("kba", dir)` returned `filetype "shp"`, `filename "kba_points."`:
- the pattern is an unanchored regex (it also matched `nokba.tif`);
- the dot position comes from the first match only and is applied to every match;
- the filename keeps the trailing dot;
- `.shp` silently wins over the exact `kba.gpkg`.
**Improvements:** deprecate. If anything is kept, use `tools::file_path_sans_ext()` / `file_ext()` on `list.files(pattern = paste0("^", data_name, "\\."))`. S

### `save_raster()`  (R/utils.R:713)
**Does:** writes a ZSTD COG with predictor, nodata and overview settings derived from `datatype`. **Used by:** elsar_pipeline has 4 calls (`06_output.R:110,179,197,370`), with `datatype` only ever `"INT1U"` or `"FLT4S"`. Inside elsar: 36 calls across 18 builders.
**Verdict:** fix. Any datatype other than INT1U or FLT4S is mishandled.
**Issues:**
- **Every non-`FLT4S` type is treated as 8-bit integer.**
  - [verified] `FLT8S` fails: "PREDICTOR=2 is only supported with 64 bit samples starting with libtiff > 4.3.0".
  - [verified] `INT2U` turns the valid value 255 into NA (NAflag 255).
  - `INT1S` gets NAflag 255, which is out of range [code].
  - Float types other than FLT4S get NEAREST overviews.
- **INT1U data with a real value 255 becomes NA.** [verified] 50/100 cells were lost. Multi-class layers that use 255 (some land-cover legends) are affected.
- **Five GDAL warnings on every call:** "driver MEM does not support creation option COMPRESS / PREDICTOR / NUM_THREADS / OVERVIEWS / OVERVIEW_RESAMPLING". [verified] This also happens with a file-backed source raster. With 36 internal call sites, users see hundreds of spurious warnings per country.
- It returns `log_message()`'s invisible `NULL`, not the path. The write is not atomic and always uses `overwrite = TRUE` (an interrupted write leaves a half file that the next run's caching treats as present; cf. audit §5 for GEE).
- Categories survive. [verified] `is.factor()` is TRUE after reload.
**Improvements:**
- Lookup table per datatype: predictor (2 for INT*, 3 for FLT*), NAflag (`NaN` for FLT*, the type's max for unsigned and min for signed), overview resampling. `match.arg(datatype, c("INT1U","INT2U","INT2S","INT4S","FLT4S","FLT8S"))`. S
- Error, or warn and promote to INT2U, when INT1U data contain 255. Cheap: `terra::global(x == 255, "max")`. S
- Silence the MEM-driver warnings. Pass the creation options only to the COG step (terra `gdal=` vs `wopt`; check with terra ≥ 1.8), or `withCallingHandlers()` muffling exactly that GDAL message 6. S
- Write to `paste0(filename, ".part")`, then `file.rename()`. `invisible(filename)`. S
- Create parent dirs or assert they exist. Tests: round trip per datatype, 255 guard, NaN for float. S

### `get_binary_layers()`  (R/utils.R:861)
**Does:** splits a stack into {0,1}-only layers and the rest, so the pipeline can write an INT1U stack and a FLT4S stack. **Used by:** elsar_pipeline has 3 calls (`06_output.R:72,159,348`). It reads `[[1]]`/`[[2]]` and `names([[1]])`, and catches errors with a fallback to all-float. Not used inside elsar.
**Verdict:** fix. Pipeline output datatypes depend on it.
**Issues:**
- The all-NA mislabel is known (audit §2 #12).
- **Near-zero continuous layers are classed as binary and then truncated.** `terra::freq(digits = 4)` rounds before the test. [verified] A layer of 0s plus one `4e-5` was classed binary. The pipeline then writes it as INT1U, which zeroes it. Sparse proportion layers (e.g. a tiny mangrove fraction) are exactly this case.
- Performance: `freq()` on float layers builds a table of every distinct value, potentially millions of rows per layer on an 850k-PU stack. Only "is any value outside {0,1}" is needed.
- It returns an unnamed `list(int, float)` with `NULL` for an empty side, and the pipeline matches layers back by *name*. Duplicate layer names would misclassify (UNVERIFIED in practice).
**Improvements:**
- Per layer: `mm <- terra::minmax(x, compute = TRUE)`; binary iff `mm` ⊆ [0,1] and `terra::global(x > 0 & x < 1, "max", na.rm = TRUE) == 0`. That needs no table and no rounding. Treat all-NA layers explicitly (audit). S
- Return `list(binary = <idx or SpatRaster>, continuous = ...)`, named. Add an `index`-returning helper (`is_binary_layer(x)` → logical per layer) so callers don't round-trip through names. S
- Tests: tiny-value layer, all-NA layer, all-1 layer, duplicate names. S

### `get_wad_data()`  (R/data.R:13)
**Does:** loads the bundled 1.9 MB `inst/extdata/wad_subset.tif`. **Used by:** elsar_pipeline has 0 calls. Inside elsar: README, `vignettes/elsaR.Rmd` (as background in about 8 chunks), the `elsar_plot_*` examples and `test_get_wad_data.R`.
**Verdict:** deprecate. Replace it with a generic synthetic-data accessor, in line with the maintainers' decision to replace bundled real data.
**Issues:** a function per dataset doesn't scale. The data has no documented source, version or licence (cf. audit §4/§7 for `boundary_dat`).
**Improvements:** `elsar_example(name = c("boundary", "pus", "background", ...))` returns synthetic objects, or a path to them (as `system.file`). Keep `get_wad_data()` as a soft-deprecated wrapper until 1.0. S

---

## Raster and vector helpers

### `exact_rasterise()`  (R/utils.R:241; helper `.terra_fun_name` l.196)
**Does:** attribute-weighted, coverage-fraction rasterisation of polygons onto the PU grid, aggregated with `fun`, then `make_normalised_raster()`. **Used by:** elsar_pipeline has **0 calls**. It was replaced by an inline `terra::rasterize` because of the file-handle problem (comment at `04_features_nondefault.R:647`). Inside elsar there are 3 calls:
- `make_threatened_ecosystems_protection.R:169` (default `fun = mean`);
- `make_urban_greening_opportunities.R:158` (`fun = mean`);
- `make_underrepresented_ecosystems.R:117` (`fun = "max"`).
**Verdict:** refactor, rewriting it on `exactextractr::exact_extract(include_cell = TRUE)`. It is wrong for `mean`, the two paths disagree, and the per-feature path is about 70× slower than necessary.
**Issues:**
- **`fun = mean` (the default) divides by the total number of features.** `coverage_fraction()` returns 0, not NA, outside each feature, so `app(stack, mean)` averages in a zero from every non-overlapping feature. [verified] Three features with attribute 1: exact path 0.333 inside a single feature and 0.667 in an overlap; fast path 1 (t2.R). After the default rescale the 1/N factor cancels, but overlaps then behave like `sum`. With `rescaled = FALSE` the values are off by a factor of N.
- **The two code paths give different numbers.** The exact path weights by coverage, fills 0 and has the dilution above. The fast path (>1000 features) is unweighted with NA background. The output therefore changes discontinuously at `max_exact_features`. [verified] Different values at the same cell (t2.R).
- **Performance:** one full-grid coverage raster per feature, stacked. [verified] 200 circles on a 300×300 grid: 20.7 s. A single `exact_extract(include_cell = TRUE)` + `tapply` + `setValues` took 0.31 s, with max |diff| 3e-8 (t3.R).
- `.terra_fun_name()` detects `mean` by function identity, which depends on what `mean` is at the call site. With terra attached, `mean` is an S4 generic. [verified] `.terra_fun_name(base::mean)` returned NA while `.terra_fun_name(mean)` returned "mean" in the same session. t1.R observed the fallback warning on a `fun = mean` call that should have taken the fast path.
- `iso3` is mandatory only to forward to `make_normalised_raster()` (audit §4).
- The function logs once per feature ("x% complete").
- It calls `elsar::make_normalised_raster` through `elsar::` from inside the package.
**Improvements:**
- Rewrite (M):
  ```r
  ee <- exactextractr::exact_extract(pus, features, include_cell = TRUE, include_cols = attribute)
  d  <- dplyr::bind_rows(ee)
  agg <- d |> dplyr::summarise(v = <fun>(...), .by = cell)
  out <- terra::setValues(terra::rast(pus), replace(rep(NA, ncell), agg$cell, agg$v))
  ```
  - `mean` becomes a coverage-weighted mean, `sum(cov*a)/sum(cov)`, with documented semantics.
  - It needs no file handles and no feature threshold, and it scales with covered cells.
  - Remove `max_exact_features`, `cores` and `.terra_fun_name`.
- Make `fun` a `match.arg(c("max","sum","mean","min"))` string. S
- Separate rasterising from normalising: return the raw raster, and let callers call `make_normalised_raster()` (`rescaled`/`invert` belong there). This matches audit §6.4 `vector_coverage()`. M
- After the rewrite the pipeline could go back to calling it, removing its inline copy. Tests: disjoint and overlapping features × each `fun`; parity with a hand calculation. S

### `convert_points_polygon()`  (R/utils.R:85)
**Does:** buffers POINT/MULTIPOINT features into circles whose area equals an attribute, optionally appending the polygons. **Used by:** elsar_pipeline has 0 calls. Inside elsar there are 5 calls:
- `make_kbas.R:120` (1e4);
- `make_wetlands_ramsar.R:142` (1e4);
- `make_protected_areas.R:231` (default 1e6);
- `make_indigenous_managed_lands.R:53,84` (1e4).
**Verdict:** fix now, then merge into `vector_coverage()` (audit §6.4).
**Issues:**
- Known items: KBA multiplier (audit §2 #5) and polygons with area 0/NA dropped (audit §2 #12).
- **MULTIPOINT features get one full-area circle per point.** [verified] `REP_AREA = 1` km² on a 2-point multipoint gave 1.995 km². The area should be split across parts, or the points cast first and the area divided.
- The docs contradict the default. The description says "assuming the area is in hectares", `@param area_multiplier` cites `1e4` for hectares, but the default is `1e6` (km², correct for WDPCA `REP_AREA`). Four of five callers override it with `1e4`.
- The `@return` says "If no valid features are found, returns NULL", but the code returns an empty `sf`.
- Control flow uses `exists("points_buffered", inherits = FALSE)` instead of a variable initialised to `NULL`.
- The caller `make_protected_areas` logs "geodesic buffers". The buffer is planar in the equal-area CRS, so the area is right and the shape slightly distorted. The log should not say geodesic.
**Improvements:**
- Replace `area_multiplier` with `area_units = c("km2","ha","m2")` (soft-deprecate the multiplier). This removes the class of bug behind audit #5. S
- `st_cast(..., "POINT")` with `area / n_parts`, or buffer the multipoint with radius `sqrt(A/(n·π))`. S
- Fix the docs. Initialise `points_buffered <- NULL`. Tests: point, multipoint, polygon-only, mixed, units. S

### `crop_global_raster()`  (R/utils.R:368)
**Does:** crops a raster to the PU extent expressed in the raster's CRS. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `make_normalised_raster.R:171`, only when `input_raster_conditional_expression` and `crop_global_input` are both set.
**Verdict:** fix. A failure silently turns into an all-zero layer.
**Issues:**
- **The error fallback returns zeros on the PU grid**, not an error and not the input CRS: `terra::ifel(pus == 1, 0, NA)` (l.394). This contradicts `@return` ("same CRS as raster_in"). [verified] With non-overlapping input: `same.crs(out, raster_in) = FALSE`, values 0. In `make_normalised_raster()` the conditional expression then runs on that zero raster and the layer comes out empty with only a log line.
- **The whole PU raster is reprojected to read off its extent.** [verified] 2000×2000 PUs: 0.66 s vs 0.017 s for `terra::project(terra::ext(pus), from = crs(pus), to = crs(raster_in))`. That is about 40× here, and the gap grows with PU count. The extents agree to within one source cell.
- There is no `snap = "out"`, so the edge cells can be clipped by half a cell.
- `threads` is only used for the needless raster projection.
**Improvements:** `e <- terra::project(terra::ext(pus), from = terra::crs(pus), to = terra::crs(raster_in))`, then `terra::crop(raster_in, e, snap = "out", extend = TRUE)`. Remove the `tryCatch` and let it error. Drop `threads` (deprecate). Make it internal (no external users). S

### `rescale_raster()`  (R/utils.R:47)
**Does:** min-max rescale to [new_min, new_max]. **Used by:** elsar_pipeline has 0 calls. Inside elsar there are 8 calls:
- `make_normalised_raster`;
- `forest_integrity` ×3;
- `flood`, `managed_forests`, `urban_greening`;
- `utils-plotting`.
**Verdict:** fix (audit §2 #2, constant → NaN). The rest is minor.
**Issues:**
- Multi-layer input works per layer. [verified] `c(1:4, 11:14)` → 0, 1/3, 2/3, 1 per layer, so that is not an issue.
- Min and max are two full passes (`global(min)` and `global(max)`).
- The defaults are evaluated as promises in the signature *and* re-checked for `NULL`, which duplicates the same expression.
- Explicit min/max outside the data range is not clamped, and the docs don't say so.
**Improvements:**
- `mm <- terra::minmax(x, compute = TRUE)` once, with defaults `NULL` only.
- Handle range 0 (audit).
- Add `clamp = FALSE`.

All S.

### `get_coverage()`  (R/utils.R:189)
**Does:** `sum(zone)/sum(pu)*100`. **Used by:** elsar_pipeline has 1 call (`06_output.R:653`, reporting zone % coverage). Inside elsar: `get_min_lockin_target`, `calculate_restore_and_urban_budgets`, `calibrate_weights.R:412`.
**Verdict:** keep, with docs fixes.
**Issues:**
- It counts cells, not area. That is fine on the equal-area PU grid, but the docs say "total area".
- Returns a percentage, while targets elsewhere are fractions (audit §4 units).
- With a multi-layer `zone_layer` it silently returns a vector.
**Improvements:** document that it counts cells and returns a per-layer vector. Add `as = c("percent","fraction")` when the units glossary lands. S

### `split_bbox_into_tiles()`  (R/utils.R:605)
**Does:** splits a bbox into an `ncols × nrows` grid of sf polygons. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `conditionally_subdivide_bbox()` only.
**Verdict:** make internal, or replace with `sf::st_make_grid`.
**Issues:** [verified] `sf::st_make_grid(x, n = c(ncols, nrows))` produces the same 6 tiles with identical areas. The 25-line double loop, with `append()` growing a list, duplicates it.
**Improvements:** `sf::st_sf(geometry = sf::st_make_grid(bbox_sf, n = c(ncols, nrows)))`, unexported. S

### `conditionally_subdivide_bbox()`  (R/utils.R:663)
**Does:** tiles a bbox into roughly 5° squares when it spans more than 10°. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `get_iucn_ecosystems.R:124`, always with an EPSG:4326 input.
**Verdict:** make internal, and fix.
**Issues:**
- **It hangs on projected input.** The spans are read as degrees with no CRS check. [verified] A 200 km square in Mollweide asks for 40,000 × 40,000 tiles, and the call was still running after 180 s; it was killed (t4.R). The only internal caller passes WGS84, but the function is exported and documented generically.
- The return type differs by branch: tiles (`geometry` only) vs the *original input*, with its attributes and real geometry, not its bbox.
- The antimeridian is not handled: a bbox spanning 179°E to −179°E gives a 358° span → 72 tiles over the wrong side of the globe.
**Improvements:** assert `sf::st_is_longlat()`, or convert thresholds into CRS units. Always return a tile `sf` (one tile when below the threshold). Make it internal. S

### `median_from_rast()`  (R/utils.R:803; helper `sidecar_fix_hint` l.758)
**Does:** reads a GDAL PAM `.aux.xml` histogram and returns the bucket midpoint at 50% cumulative count. **Used by:** elsar_pipeline has 0 calls (it prepares the side-car for it, `01_config.R:1141-1164`). Inside elsar: `make_threatened_ecosystems_protection.R:88`.
**Verdict:** fix the docs, then make it internal.
**Issues:**
- Its use (compared against a rescaled EII) is covered by audit §2 #12.
- The docs claim an "exact median". It is a histogram approximation with resolution (max − min)/BucketCount (256 by default for `gdalinfo -hist`).
- It reads the first `<HistItem>` only, so on multi-band files it silently uses band 1, and on a multi-layer `SpatRaster` it uses the first source.
- It ignores the `<Approximate>`/`<IncludeOutOfRange>` flags.
- [verified] An in-memory raster gives "Side-car XML not found for " with an empty path and the hint `gdalinfo -hist ''`.
**Improvements:**
- Fix the docs.
- `stopifnot(terra::nlyr(r) == 1)`.
- Error early when `terra::sources(r) == ""`, with a fallback of `stats::median(terra::spatSample(r, 1e6))` for in-memory or small rasters.
- Select the `HistItem` for the band.
- Make it internal.

All S.

### `elsar_extend()`  (R/utils-plotting.R:685)
**Does:** crops a raster to the extent of another, enlarged by a ratio or by an absolute distance. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `elsar_plot_background` (`utils-plotting.R:485`).
**Verdict:** make internal (a plotting helper) and simplify.
**Issues:**
- The name says "extend" but the function crops.
- `extend_by` switches meaning at 1: ≤ 1 is a ratio, > 1 is in CRS units. So `1` means 100%, and 1.5 means 1.5 m. That is a surprising API.
- There is no CRS check. Its caller first `terra::project()`s the *whole* background raster into the PU CRS and crops afterwards (`utils-plotting.R:476`), reprojecting e.g. a global layer when a few percent of it is needed.
- The defaults are `NULL` for required arguments.
**Improvements:** `terra::crop(x, terra::ext(main) + pad)`, where `pad` is computed once. Separate `ratio`/`distance` arguments. In the caller, crop in the source CRS first (reusing the fixed `crop_global_raster()`), then project. Unexport through deprecation. S

### `elsar_continuous_to_categorical()`  (R/utils-plotting.R:782)
**Does:** converts a raster to an xy data.frame and bins one column into equal-width or manual classes, with interval labels. **Used by:** elsar_pipeline has 0 calls. Inside elsar: `elsar_plot_static_raster.R:149`, `utils-plotting.R:510`.
**Verdict:** make internal, and fix.
**Issues:**
- [verified] A constant raster errors with "'breaks' are not unique".
- `graphics::hist(x, breaks = seq(...), plot = FALSE)$breaks` just returns the `seq()` it was given: a full histogram pass for nothing.
- `number_categories` is required even when `manual_breaks` is supplied.
- Manual breaks that don't cover the range silently give NA categories.
- The return value is an unnamed `list(df, breaks)` or a df, depending on `hist_breaks_out`.
- The name says it returns categories, but it returns a data.frame, not a raster.
**Improvements:**
- `breaks <- manual_breaks %||% seq(min, max, length.out = n + 1)`, with a range-0 guard (one class).
- Always return `list(data = , breaks = )`.
- Warn on out-of-range values.
- Make it internal.

All S.

---

## Shared utilities

### `log_message()` / `log_msg()`  (R/utils.R:420 / :435)
**Does:** prints a timestamped, glue-interpolated `message()`. It optionally echoes and returns a value. **Used by:** elsar_pipeline has 0 calls (it has its own `log_pipeline()`). Inside elsar there are 332 calls. `log_msg` has 0 callers anywhere.
**Verdict:** refactor (audit §4 covers verbosity and cli). Remove `log_msg` at 0.6.
**Issues:**
- New: "Warning:" messages are emitted through `log_message()`, not `warning()`:
  - `make_custom_mollweide_projection.R` (missing CRS assumed WGS84);
  - `crop_global_raster` (crop failed → zero raster);
  - `make_wetlands_ramsar.R` ×2;
  - `gee_helpers.R` ×2.

  Callers can neither catch nor test these, and the pipeline's `tryCatch` cannot see them.
- `glue(msg, .envir = parent.frame())` re-parses `msg`. A literal `{` in a pre-built string, such as a JSON or regex fragment or a path from `paste0()` (2 call sites use `log_message(paste0(...))`), errors or interpolates wrongly. It is low risk today: neither current paste0 call contains braces.
- The `variable =` echo-and-return feature has no internal users (UNVERIFIED beyond grep).
**Improvements:**
- `cli::cli_inform()` gated by `getOption("elsar.verbose", TRUE)`, with class `elsar_message` so callers can `suppressMessages(classes = )`.
- Convert the "Warning:" lines to `warning()` (classed).
- Drop `variable`.
- `log_msg` → `lifecycle::deprecate_warn()` now, removed at 0.6.

S–M (the conversion is mechanical but touches 332 lines).

### `is_valid_iso3()` / `iso3_base()`  (R/utils.R:16 / :31; internal)
**Does:** validates an ISO3-style region code / returns its first 3 characters for matching global datasets. **Used by:** elsar_pipeline has its **own, stricter** `is_valid_iso3()` (`utils_pipeline.R:946`: `^[A-Z]{3}(_[A-Z0-9]+)?$`). Inside elsar:
- `is_valid_iso3` has 11 callers: `exact_rasterise`, `make_kbas`, `make_threatened_ecosystems_restoration`, `calculate_restore_and_urban_budgets` and 7 in `gee_helpers`.
- `iso3_base` has 4 callers: `make_kbas`, `make_wetlands_ramsar`, `make_protected_areas`, `make_urban_greening_opportunities`.
**Verdict:** keep and fix. Unify the grammar with the pipeline, and normalise case once.
**Issues:**
- **The two validators disagree.** [verified] elsar accepts `"usa"`, `"USA-CA"` and `"USA_ca_x"`. The pipeline rejects all three and has a test asserting `USA-CA` is invalid (`tests/testthat/test-subnational.R:10`).
- `iso3_base("usa")` → `"usa"`, which matches nothing in KBA, Ramsar, WDPCA or GHSL. These are compared case-sensitively against uppercase attributes, so a lowercase code validates and then silently filters everything out (code; cf. `filter_sf` case test above).
- `make_boundary()`, `make_planning_units()` and `make_custom_mollweide_projection()` validate with `is.string` instead, so three grammars are in use.
- Neither function has a direct test in elsar (`iso3_base` is tested; `is_valid_iso3` only in the pipeline).
**Improvements:**
- One `normalise_iso3(x)` that validates (`^[A-Za-z]{3}([_-][A-Za-z0-9]+)*$` or the pipeline's stricter form; pick one), uppercases the 3-letter base and returns it. Use it at every entry point. Have `iso3_base()` uppercase too.
- Export it (or a `elsar_iso3()` helper) so the pipeline can drop its copy.
- Tests.

S.

### `.terra_fun_name()`  (R/utils.R:196; internal)
**Does:** maps a `fun` function or string to a `terra::rasterize` fun name. **Used by:** `exact_rasterise()` only.
**Verdict:** delete with the `exact_rasterise()` rewrite. Until then, compare names rather than identity.
**Issues:** identity matching against `match.fun()` depends on the S4 `mean` generic that terra installs. See `exact_rasterise`. [verified]
**Improvements:** accept strings only (`match.arg`). S

### `get_target()`  (R/utils-elsa.R:113)
**Does:** converts a % target into an absolute PU-cell count (floor 1e-4). **Used by:** elsar_pipeline has 0 calls. Inside elsar: `calibrate_weights.R:425`.
**Verdict:** make internal.
**Issues:** the scalar check `target >= 0 && target <= 100` errors for vector targets with an unhelpful message. It uses `ifelse` on a scalar. It is %-based, while the units glossary is pending (audit §4).
**Improvements:** unexport; vectorise or `assert is.number`. S

### `get_min_lockin_target()`  (R/utils-elsa.R:17)
**Does:** raises zone targets to at least the coverage already locked in. **Used by:** elsar_pipeline has 0 calls; inside elsar, none.
**Verdict:** deprecate, or fix and make it internal.
**Issues:**
- [verified] The documented "list of rasters" input errors: "unable to find an inherited method for function 'global' for signature x = "list"".
- 40 lines of copy-paste per zone (`ifelse` ×4) where `pmax(targets, coverage)` would do.
- The log line ignores zone 4.
- Hard-coded `zone_N_target` names.
**Improvements:** `pmax(unlist(input[paste0("zone_", seq_len(nlyr), "_target")]), get_coverage(lockin, pu))`. Accept `SpatRaster` only. Deprecate the export. S

### `calculate_restore_and_urban_budgets()`  (R/utils-elsa.R:164)
**Does:** budget % = 30% × coverage of the restore and urban zones, read from fixed file names. **Used by:** none (pipeline 0, elsar 0).
**Verdict:** deprecate. Audit §4 notes that it reads file names nothing writes.
**Issues:** `iso3` is validated and logged, but otherwise unused.
**Improvements:** if the logic is wanted, take rasters rather than file names: `budget_from_zone(zone, pus, fraction = 0.3)`. S

### Internal helpers that need no change
- **`combine_layers()`** (R/elsar_load_data.R:236): good, recent and tested. When the `elsar_read_vector()` split happens, give it an explicit `bind = FALSE` option.
- **`sidecar_fix_hint()`** (R/utils.R:758): only needs the empty-path guard described under `median_from_rast()`.

---

## Cross-cutting

1. **Two implementations of one job, with different numbers.**
   - `exact_rasterise()` has an exact and a fast path; the pipeline has a third copy inline.
   - `make_boundary(limit_to_mainland)` and `make_planning_units(limit_to_mainland)` (dead): the pipeline wired the dead one.
   - `is_valid_iso3()` exists in elsar and in the pipeline with different grammars.

   Each pair has drifted. Consolidate on one implementation, then let the pipeline call it. This is the same lesson as the audit's `align_to_pus` / `vector_coverage` (§6.4).
2. **Silent fallbacks instead of errors.**
   - `crop_global_raster()` → zero raster;
   - `filter_sf()` → `NULL` on SQL error, or 0 rows on a wrong column;
   - `elsar_load_data()` ignores `iso3` without `iso3_column`;
   - "Warning:" sent through `log_message()`;
   - `get_binary_layers()` rounds tiny values to binary.

   Every one of these turns bad input into a plausible-looking but empty or wrong layer. Prefer errors, or classed warnings that the pipeline can collect.
3. **Dead and misleading parameters.** `make_planning_units(limit_to_mainland)`, `make_custom_mollweide_projection(iso3_column)` (×2), `filter_sf(file_type)`, `crop_global_raster(threads)`, `exact_rasterise(iso3, cores, max_exact_features)`, `make_boundary(input_type)`, `elsar_load_data(pg_connection)`. Remove them through `lifecycle::deprecate_soft()`; this fits the 0.5 glossary pass.
4. **Hand-written versions of existing primitives.** `split_bbox_into_tiles` duplicates `st_make_grid`, `crop_global_raster` uses project(raster) instead of project(ext), `exact_rasterise` stacks per feature instead of one `exact_extract(include_cell)`, `get_binary_layers` uses `freq()` instead of `minmax`, and `elsar_continuous_to_categorical` calls `hist()` only to get its breaks back. The library calls are shorter, faster (40–70× measured for two of them) and remove whole bug classes.
5. **Datatype and units handled by convention, not tables.** `save_raster()` treats all non-FLT4S types as INT1U, and `convert_points_polygon()` uses a bare multiplier (audit #5 is one such bug). A datatype table and an `area_units` enum close both.
6. **Export surface.** 14 of the 27 functions here have no external callers: `filter_sf`, `get_file_type`, `extract_filename_filetype`, `split_bbox_into_tiles`, `conditionally_subdivide_bbox`, `crop_global_raster`, `median_from_rast`, `elsar_extend`, `elsar_continuous_to_categorical`, `get_target`, `get_min_lockin_target`, `calculate_restore_and_urban_budgets`, `log_msg`, `make_custom_projection`. Unexport or deprecate them in 0.6, as audit §4 proposes. Keep exported the ones the pipeline uses: `make_boundary`, `make_planning_units`, `elsar_load_data`, `save_raster`, `get_binary_layers` and `get_coverage`. `exact_rasterise` only stays if the pipeline goes back to calling it after the rewrite.
7. **Tests.** None of these has a test: `exact_rasterise`, `save_raster`, `get_binary_layers`, `convert_points_polygon`, `crop_global_raster`, `filter_sf` (direct), `get_file_type`, the bbox tilers, `elsar_extend`, `elsar_continuous_to_categorical`, `log_message`, `is_valid_iso3` and the `utils-elsa.R` trio. Every [verified] finding above is already a ready-made regression test (scripts t1–t5.R).

### Verdict counts (27 cards, 29 verdicts: the cards that pair a function with its deprecated alias, `make_custom_mollweide_projection` and `log_message`, get one verdict per function)
| Verdict | n | Functions |
|---|---|---|
| keep | 2 | get_coverage, internal helpers (combine_layers/sidecar_fix_hint) |
| fix | 10 | make_boundary, make_planning_units, make_custom_mollweide_projection, save_raster, get_binary_layers, convert_points_polygon, crop_global_raster, rescale_raster, is_valid_iso3/iso3_base, median_from_rast (+ internal) |
| refactor | 3 | elsar_load_data, exact_rasterise, log_message |
| make internal | 7 | filter_sf, get_file_type, split_bbox_into_tiles, conditionally_subdivide_bbox, elsar_extend, elsar_continuous_to_categorical, get_target |
| deprecate / remove | 6 | make_custom_projection, log_msg, extract_filename_filetype, get_wad_data (replace with synthetic accessor), get_min_lockin_target, calculate_restore_and_urban_budgets |
| delete with rewrite | 1 | .terra_fun_name |

Several "make internal" items also need small fixes (`conditionally_subdivide_bbox`, `elsar_continuous_to_categorical`, `filter_sf`). They are counted once, under their export decision.

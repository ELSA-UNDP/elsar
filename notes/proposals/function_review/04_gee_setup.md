# 04 - Earth Engine, downloads and setup: per-function review

**Scope:** every function in `R/gee_helpers.R` (24), `R/elsar_setup_gee.R` (3)
and `R/elsar_check_setup.R` (7), in `elsar-audit` at 54522b7 (develop = 0.4.1).
**Baseline:** `notes/proposals/2026-11_audit_and_roadmap.md`. Its findings are
cited as **A#n** (§2 table rows), **A§5** (Earth Engine section) and **A§3/A§4**,
and are not re-reported here.
**Downstream:** `elsar_pipeline` at 1fd6f43. I grepped R/, app/, run_*.R,
config/, check_dependencies.R and Dockerfile.
**Evidence tags:** [verified] = ran R on synthetic data or mocks
(`gee_probe.R` in this folder); [code] = read from the code;
UNVERIFIED = depends on Earth Engine runtime behaviour.

## Downstream usage at a glance

The pipeline calls only **3** of these functions directly:

| elsar function | Pipeline call site | How |
|---|---|---|
| `download_lulc_proportions()` | `R/03_features_default.R:308` | `boundary_layer, iso3, gee_project, output_dir = paths$input, lulc_product, class_names, pus, wait_time = config$pipeline$wait_time, force_download = config$runtime$force_gee`. Called only when `find_lulc_proportions_files()` finds no local files. |
| `check_and_download_required_layers()` | `R/03_features_default.R:353` | `skip_lulc = use_proportions` (default TRUE), so in practice it only downloads Pasturelands. `interactive = !auto_mode`. |
| `download_lulc_class_proportion()` | `R/03_features_default.R:798` | Inside `download_class_proportion()`, which **nothing calls**. It is dead code, and it would also pass `pus = NULL`, which elsar rejects. |

The pipeline uses nothing else here: no `elsar_setup_gee()` and no
`elsar_check_setup()`. Instead it has its own versions:
- Docker builds env `ee` itself: Python 3.11, `openssl=3.0`,
  `RETICULATE_PYTHON`.
- `R/diagnostics.R` has its own EE check and a task lister
  (`check_gee_tasks()`, `has_pending_gee_exports()`).
- `utils_pipeline.R:1122` has its own cache lookup
  (`find_lulc_proportions_files()`).
- It runs its own Drive preflight.

**Unit mismatch on `wait_time` [code]:**
- Pipeline configs say `wait_time: 3  # Seconds between GEE export status
  checks`, and `01_config.R:451` validates it as "0-600 seconds".
- elsar reads it as the **maximum wait in minutes**.
- So country runs give each export a 3-4 minute timeout. A value of `0`
  passes both validators and times out immediately [verified: `wait_time = 0`
  reaches EE init].

---

## R/gee_helpers.R

### `elsar_gee_env()`  (R/gee_helpers.R:15)
**Does:** returns the constant conda env name `"elsar_ee"`. **Used by:** internal (create/initialize/check_setup); pipeline: unused (uses env `ee`).
**Verdict:** keep - a single source of truth for the name is right.
**Issues:**
- The list of fallback env names is not centralised. It is written out three
  times and the copies differ:
  - L442: no `"earth-engine"`;
  - L487;
  - `elsar_check_setup.R:262`.
- So `LD_LIBRARY_PATH` is never set for an env named `earth-engine`, although
  that env is accepted for import [code].
**Improvements / target shape:** add `ee_env_candidates()` next to it, returning the ordered vector, used by all three (S). Make it overridable via `getOption("elsar.gee_env")` so the Docker env `ee` is first-class rather than a fallback (S).

### `require_gee_deps()`  (R/gee_helpers.R:29)
**Does:** errors with an install hint if `reticulate`/`googledrive` are missing. **Used by:** internal, but only `initialize_earthengine()` and the dead `download_from_drive()` call it; pipeline: unused.
**Verdict:** refactor - fine on its own, but it is bypassed.
**Issues:**
- Five other places re-implement the check inline, with a worse message and no
  setup hint: L830-835, L1732, L2120-2125, `elsar_setup_gee.R:68` and the
  setup tier.
- `download_gee_layer()` therefore says "Package 'googledrive' is required"
  without pointing to `elsar_setup_gee()` [code].
**Improvements / target shape:** replace with `rlang::check_installed(c("reticulate","googledrive"), reason = "for Earth Engine downloads; then run elsar_setup_gee()")` and call it once, inside `elsar_gee_session()` (S).

### `earthengine_cred_paths()`  (R/gee_helpers.R:60)
**Does:** lists candidate paths of the Python EE credentials file. **Used by:** internal (`initialize_earthengine`, `authenticate_gee`, `check_tier_earth_engine`).
**Verdict:** fix.
**Issues:**
- **Wrong on Windows [code; runtime UNVERIFIED].**
  - The Python client stores credentials at `os.path.expanduser("~/.config/earthengine/credentials")`, and on Windows Python's `~` is `%USERPROFILE%`.
  - R's `path.expand("~")` on Windows is the *Documents* folder (`R_USER`).
  - `rappdirs::user_config_dir()` gives `AppData/Local/...`.
  - So neither candidate is the real file. The check reports "no credentials", and `initialize_earthengine()` calls `ee$Authenticate()` (a browser flow) on **every** download on Windows.
- A file that exists is treated as valid credentials. Expired or revoked
  refresh tokens are only discovered when `ee$Initialize()` fails, and that
  error carries no guidance [code].
**Improvements / target shape:** ask Python itself: `ee$oauth$get_credentials_path()` once the env is active, and keep a `Sys.getenv("USERPROFILE")` fallback for the no-Python case (S). Under §5's auth module, replace "file exists" with "`ee$Initialize()` succeeds", wrapped to give classed errors (`elsar_gee_auth_error`) (S).

### `find_conda_exe()`  (R/gee_helpers.R:76)
**Does:** finds `mamba`/`conda` inside a conda base, preferring mamba. **Used by:** internal (`create_gee_conda_env`); tested.
**Verdict:** keep (inside the env module).
**Issues:**
- On Windows it can return `condabin/*.bat`, which is then run through
  `system()`.
- `system()` on Windows does not go through `cmd.exe`, so a `.bat` may not
  run (UNVERIFIED).
**Improvements / target shape:** use `system2()` (or `processx::run`) and, on Windows, wrap `.bat` in `cmd /c` (S). Alternatively drop it and use `reticulate::conda_create(envname, packages = ..., forge = TRUE, conda = <exe>)` (S).

### `create_gee_conda_env()`  (R/gee_helpers.R:112)
**Does:** creates or reuses the persistent env with `earthengine-api`, then `reticulate::use_python()`s it. **Used by:** `elsar_setup_gee()`, `initialize_earthengine()`; pipeline: unused (the Dockerfile builds its own env).
**Verdict:** fix.
**Issues:**
- **When an env is reused, nothing is checked [code].**
  - If the env directory has a Python, the env is reused even when
    `earthengine-api` is missing (for example after an interrupted
    `conda create` that got Python but not the package).
  - The failure only surfaces later as `import("ee")` failing.
  - The partial-env repair at L134-141 only handles "no Python at all".
- **`python_version` is silently ignored on reuse [code].**
  `elsar_setup_gee(python_version = "3.11")` against an existing 3.12 env
  does nothing and says "Reusing".
- The fixed `Sys.sleep(2)` stands in for real synchronisation.
- The package is unpinned: A§5.
**Improvements / target shape:**
- `ensure_gee_env(env = ee_env_name(), python = "3.12", ee_api = ">=1.0,<2", rebuild = FALSE)`. On reuse it checks `ee` importability with `reticulate::py_module_available()` in a subprocess (`callr`, or `system2(python, "-c 'import ee'")`) and rebuilds or repairs (`conda install`) on failure (S-M).
- Warn when the requested Python differs from the env's (S).
- Return the python path, not the name (S).

### `find_conda_base()`  (R/gee_helpers.R:197)
**Does:** searches fixed paths, then reticulate's conda and miniconda, for a conda base. **Used by:** internal (5 call sites, check_setup).
**Verdict:** fix (small): the search order is wrong.
**Issues:**
- `reticulate::conda_binary()` is the most reliable answer: it honours
  `RETICULATE_CONDA`, PATH and micromamba. But it is consulted *after* nine
  hard-coded guesses, so if a stale `~/anaconda3` exists it wins over the
  conda the user actually uses [code].
- The candidate `~/.conda` usually holds envs, not a base, but it has an
  `envs/` folder, so it can be returned as a base without any conda executable
  [code]. `find_conda_exe()` then returns NULL and creation errors with
  "Could not find a conda or mamba executable".
**Improvements / target shape:** order: `Sys.getenv("ELSAR_CONDA")` > `reticulate::conda_binary()` > fixed list; accept a candidate only if `find_conda_exe()` succeeds (S).

### `get_crs_wkt()`  (R/gee_helpers.R:263)
**Does:** converts an object's CRS to WKT1 by shelling out to `gdalsrsinfo`, falling back to WKT2. **Used by:** internal, 2 sites:
- L1002: the result is assigned to `pu_crs` and **never used** (dead);
- L1831: in `download_lulc_class_proportion()`.

**Verdict:** refactor.
**Issues:**
- The `gdalsrsinfo` dependency is A§5. But A§5's proposed fix ("send
  `EPSG:xxxx`") **does not work for elsar's own grids**:
  `make_custom_mollweide_projection()` builds a custom WKT1 `PROJCS`
  (`R/make_custom_mollweide_projection.R:62`), which has no EPSG/ESRI code.
- That WKT1 is lost because sf/terra store WKT2, which is why the CLI round
  trip exists. Neither sf nor terra exposes a WKT1 writer [verified: no
  `wkt1` option or function in either namespace].
- Output from `gdalsrsinfo` starts with a blank line [verified]. That is
  harmless, but it shows the output is never validated.
**Improvements / target shape:**
- `ee_crs(x)` returns `"EPSG:<code>"` when `sf::st_crs(x)$epsg` (or an ESRI
  authority code) is available.
- For an elsar Mollweide it regenerates WKT1 from `lon_0` with the same
  template as the projection builder, shared through a helper
  `mollweide_wkt1(lon_0)`.
- Only for anything else does it try `gdalsrsinfo`, and then it errors with a
  classed condition instead of silently sending WKT2 (S-M).

### `find_env_python()`  (R/gee_helpers.R:320)
**Does:** returns the python executable path in `<base>/envs/<name>`. **Used by:** internal.
**Verdict:** keep.
**Issues:** none new.
**Improvements / target shape:** move into the env module unchanged (S).

### `safe_gee_getinfo()`  (R/gee_helpers.R:354)
**Does:** `getInfo()` with a Windows C-long overflow fallback via `serialize()`. **Used by:** **nobody** [verified: 0 call sites].
**Verdict:** delete (or actually use it).
**Issues:**
- It is dead. All seven `$getInfo()` calls (year searches L887, L1769, L1781,
  L2167, L2179; regions L989, L1884, L2291) call the raw method, so the
  Windows protection it was written for does not exist.
- Its fallback is also wrong: `serialize()` returns the expression graph, not
  the computed value. So `fromJSON(serialize())` would return a JSON AST where
  a number was expected [code].
**Improvements / target shape:** delete. The regions no longer need `getInfo()` (pass the `ee.Geometry` directly to `region=`). The year search becomes a single `aggregate_array("system:time_start")` call with `reticulate::py_to_r` and `convert = FALSE` handling (S).

### `safe_task_status()`  (R/gee_helpers.R:382)
**Does:** reads `state`/`description` from `task$status()` and tolerates the overflow. **Used by:** internal, once (L941, the duplicate-task check in `download_gee_layer`).
**Verdict:** refactor into the export engine's task poller.
**Issues:**
- It is the only task-status code, yet no download function *polls* its own
  task. All three wait on Drive file names instead (A#13).
- Neither proportion function uses it to avoid submitting duplicate exports
  (see `download_lulc_proportions`).
**Improvements / target shape:** `ee_wait_task(task, timeout, poll = 15)`. It returns COMPLETED / FAILED (with `error_message`) / CANCELLED / TIMEOUT, and it is the only thing that decides whether to download (M, part of A#13).

### `initialize_earthengine()`  (R/gee_helpers.R:426)
**Does:** picks or creates a Python env with `ee`, authenticates if no credential file exists, then calls `ee$Initialize(project)`. **Used by:** the three export functions, each call (so once per class or layer); pipeline: unused (it has its own `ee$Initialize()` without a project in `diagnostics.R`).
**Verdict:** refactor into `elsar_gee_session()` (the A§5 redesign).
**Issues:**
- **If Python is already initialised without `ee`, a ~400 MB env is built
  that cannot be used [code].**
  - L456-461 detect this and log "Restart R with RETICULATE_PYTHON set".
  - The code then carries on: `use_python()` cannot switch the interpreter
    after initialisation, so every candidate fails.
  - It then calls `create_gee_conda_env()`, which builds the env.
  - `import("ee")` at L527 then fails anyway.
  - It should stop at L460.
- `temp_env` is always `NULL`. The return contract `list(ee, temp_env)`
  exists only for `cleanup_earthengine()`, which is a no-op.
- Each `download_*` call re-runs the full discovery and `Initialize()`. The
  pipeline makes up to three calls per run. This does not change results, but
  it makes the "cached runs still need the network" problem (A§5) worse.
- The `LD_LIBRARY_PATH` mutation and the interactive-only auth are A§5.
**Improvements / target shape:**
- `elsar_gee_session(project, credentials = NULL, python = NULL, quiet = FALSE)`.
- It is memoised in a package environment and returns an `elsar_gee_session`
  S3 object (`ee`, `project`, `python`, `api_version`, `sink`).
- Every download takes `session = elsar_gee_session()`, so tests can inject a
  fake.
- It fails fast with a classed error when Python is already bound to a non-ee
  interpreter (M).

### `cleanup_earthengine()`  (R/gee_helpers.R:555)
**Does:** removes `env_info$temp_env` if set. That never happens, so it is a no-op. **Used by:** 13 internal call sites, all no-ops.
**Verdict:** delete.
**Issues:**
- 13 calls to a function that cannot do anything.
- The real cleanup (temp tiles, Drive files) is scattered beside each of
  them, and on some error paths it is skipped. The `download_gee_layer()`
  download/merge section (L1078-1089) has no `on.exit()`, so a failed
  `drive_download` leaves `$HOME/gee_download_*` behind. That directory is
  then the stale-tile source for A#13 [code].
**Improvements / target shape:** delete; use one `withr::local_tempdir()` / `on.exit(unlink(...), add = TRUE)` per export (S).

### `download_from_drive()`  (R/gee_helpers.R:579)
**Does:** downloads all `.tif` files in a Drive folder whose names start with a prefix. **Used by:** **nobody** [verified: 0 call sites]; documented with an example.
**Verdict:** delete (and reuse its error handling in the Drive sink).
**Issues:**
- It is dead. Yet it is the only Drive code with per-file error handling and
  a `success` flag, while the three live functions download inline without
  either.
- Its prefix regex has the same unanchored-suffix problem as A#13.
**Improvements / target shape:** internal `drive_sink_fetch(task_names, dest)`, used by the export engine (S). It:
- matches exact names, `<name>.tif` or `<name>-\d{10}-\d{10}.tif` (the EE tile suffix);
- searches inside a per-run folder;
- reports per-file failures.

### `merge_tiles()`  (R/gee_helpers.R:670)
**Does:** VRT-merges every `.tif` in a directory and writes a ZSTD COG with the given datatype. **Used by:** internal (2 sites); pipeline: unused.
**Verdict:** fix.
**Issues:**
- **Takes a directory, not a file list.** That is why stale tiles get merged
  (A#13).
- **Tiles with a different CRS are dropped with only a GDAL warning
  [verified].** A stray EPSG:3857 tile was skipped ("vrt did not use 1 of the
  2 files") and the output was silently smaller.
- **No range check against `datatype` [verified].** Float tile values 0.4,
  0.6, 1.5, 2.5 written as `INT1U` gave `0 0 1 2`. Today callers pass the
  right type, but anything generic (`download_gee_layer(datatype=)`) can
  truncate silently.
- `OVERVIEWS=NONE` with the comment "Let terra handle overviews": no
  overviews are ever built, so it is a COG in name only.
- The write is not atomic (A§5).
**Improvements / target shape:**
- `merge_tiles(files, output_file, datatype, nodata)`;
- error if the CRS or resolution of the tiles differs;
- write to `paste0(output_file, ".part")`, then `file.rename()`;
- set `NAflag` from the product registry;
- `OVERVIEWS=AUTO` (S).

### `download_gee_layer()`  (R/gee_helpers.R:808)
**Does:** generic engine: initialises EE and Drive, finds the year, checks the local cache, then Drive, starts a Drive export (optionally "aggregated to PUs"), waits for files, downloads, merges, and with `aggregate_to_pus` reprojects locally. **Used by:** the four product wrappers; pipeline: indirectly, only via `check_and_download_required_layers()` → pasture (and LULC when `use_proportions: false`).
**Verdict:** refactor. It becomes the public generic over the §5 export engine.
**Issues:**
- **`aggregate_to_pus = TRUE` does no aggregation in Earth Engine [code;
  output UNVERIFIED].**
  - There is no `reduceResolution()` anywhere in this function.
  - `aggregation_reducer` only chooses the *local* `terra::project()` method
    (L1094-1101).
  - The export is the composite at `scale = pu_res` in EPSG:4326, so EE fills
    each PU-sized pixel from its pyramid (default policy: mean) or by
    nearest-neighbour sampling.
  - For categorical ESRI codes a mean pyramid yields non-existent classes
    (2 and 5 average to 3/4), which are then cast to `INT1U`.
  - This is separate from A#9, which covers only the two proportion
    functions.
- **The first run and a cached run behave differently [code].**
  - A fresh run returns the raster with `attr(, "pre_aggregated") = TRUE`.
  - The cache hit at L901-906 returns a bare `terra::rast()`.
  - `make_normalised_raster()` branches on that attribute
    (`R/make_normalised_raster.R:132`): skip resampling vs full resampling,
    plus the mode warning at L150.
  - So the same inputs take two code paths depending on whether the file
    already existed. The attribute also does not survive a
    `writeRaster`/`rast` round trip [verified].
- **`year` search ignores the region [code].** L887 filters by date only,
  while the proportion functions use `filterBounds`. A year with data
  somewhere in the world but not in the region is chosen and exports empty or
  0 (A§5 masked→0).
- **Docs are wrong about the return value.** It promises "or NULL if export
  timed out", but it `stop()`s. All four wrappers' `if (!is.null(result))`
  are dead.
- `wait_time` must be a whole number: 2.5 is rejected and 0 is accepted
  [verified].
- `scale` is in metres and is taken from `terra::res(pus)` without checking
  the PU grid is projected. Geographic PUs give `scale ≈ 0.008` [code].
- `googledrive_folder` creates `<folder>_<iso3>` but the search ignores the
  folder. It is used only by the broken test, and the docs warn against it.
  Remove it.
- Dead locals: `pu_crs`, `pu_ext`, `source_projection`.
- Known and cited: defaults to `here::here()` (A§4 outputs); `$HOME` temp dir,
  network on cache hit, unpinned years (A§5); two-corner region (A§5); cache
  key and stale tiles (A#13); antimeridian (A#8).
- Testability: one test, which calls a non-existent function (A§3). There is
  no seam to mock, because EE and Drive are reached through module globals
  rather than an injected session.
**Improvements / target shape (after the §5 split):**
```r
download_gee_layer(region, product = NULL, asset_id = NULL, band = NULL,
                   year = NULL, composite = NULL,
                   grid = c("native", "pus"), reducer = NULL,
                   output_dir, overwrite = FALSE,
                   session = elsar_gee_session(), timeout = 30)
```
- `region` is the `elsar_region()` from A§6, or `pus`/boundary during the
  transition.
- `product` looks up asset, band, composite, legend, nodata, native scale and
  valid years in the registry. Raw `asset_id` stays possible for ad-hoc
  layers.
- `grid = "pus"` runs the single correct pipeline from A#9 for *every*
  product. Categorical products require `reducer = "mode"`, and the
  `pyramidingPolicy` question goes away because `reduceResolution` reads
  native pixels.
- It returns a file-backed `SpatRaster` with a JSON sidecar. Downstream code
  reads provenance from the sidecar, not from `attr()` (the attribute
  contract is removed).
- `year = NULL` means "latest available *in region*", is recorded in the
  sidecar, and an explicit year is never replaced.
- `timeout` is in minutes and may be fractional.
- Effort: **L** overall. The pieces are A#9/A#13/A§5 items; the
  signature/registry part is M.

### `download_esri_lulc_data()`  (R/gee_helpers.R:1145)
**Does:** `download_gee_layer()` with the ESRI 10 m asset, mosaic, `mode`, `INT1U`, then tags `lulc_product = "esri_10m"`. **Used by:** `download_lulc_data()`; pipeline: indirectly (only with `use_proportions: false`).
**Verdict:** merge into `download_lulc(product = "esri_10m")`, then deprecate.
**Issues:**
- **There is no `year` argument.** It is reachable only as `year_override`
  through `...`, so `download_lulc_data(product = "esri_10m", year = 2019)`
  silently downloads the latest year [verified: `year` never reaches the
  wrapper].
- The help page says "Downloads the most recent year available", but the
  search starts at *last calendar year*, so on 2 January the newest product
  year may be skipped.
- Validation duplicates `download_gee_layer()` and `download_lulc_data()`:
  the same three asserts in three layers.
**Improvements / target shape:** one registry row (`esri_10m`: asset, band `b1`, composite `mosaic`, legend from `get_lulc_classes()`, nodata 0, years 2017-) (S). Keep a `lifecycle::deprecate_soft()` shim for one minor version (S).

### `download_global_pasture_data()`  (R/gee_helpers.R:1222)
**Does:** downloads the Global Pasture Watch cultivated or natural grassland probability at 30 m as `FLT4S`. **Used by:** `check_and_download_required_layers()`; pipeline: **yes, indirectly**. It is the only GEE layer that call actually fetches in default runs.
**Verdict:** merge into `download_gee_layer(product = "gpw_cultivated" | "gpw_natural")`, then deprecate.
**Issues:**
- **No grid or aggregation path, even though the pipeline asks for one
  [code].** `check_and_download_required_layers()` never passes
  `pus`/`aggregate_to_pus` to it.
  - So Pasturelands is always exported at 30 m float over the whole country
    bbox, into `$HOME`, and resampled locally.
  - For a large country that is roughly 10^10 cells (UNVERIFIED for the
    actual tile count).
  - A PU-grid mean export would be about 1000x smaller and correct for a
    probability layer.
- The asset is a probability in 0-100 (UNVERIFIED: catalogue scaling). It is
  stored as `FLT4S`, with no scaling or nodata recorded, while consumers
  expect 0-1. Check the consumer in the manage zone.
- `lulc_product` is not tagged, which is fine, but nothing records the
  version (`v1`).
**Improvements / target shape:** registry rows `gpw_cultivated` and `gpw_natural` with `scale_factor`, `nodata` and `years`; `grid = "pus", reducer = "mean"` by default when a region is supplied (S once the engine exists).

### `download_dynamic_world_data()`  (R/gee_helpers.R:1337)
**Does:** annual `mode()` composite of the DW `label` band. **Used by:** `download_lulc_data()`; pipeline: indirectly when `lulc.product: dynamic_world` and `use_proportions: false`.
**Verdict:** merge into `download_lulc(product = "dynamic_world")`.
**Issues:**
- **The year appears twice in the file name [code].**
  - `file_prefix = "dynamic_world_lulc_{year}"` and the engine then appends
    `_{year}_{iso3}`, giving `dynamic_world_lulc_2025_2025_GHA.tif`.
  - It still matches the pipeline's discovery regex by luck.
- The default year is the previous calendar year, passed as `year_override`.
  So unlike ESRI there is no fallback, and a year with sparse coverage early
  in the archive (2015-2016) is used as is.
- A one-year 10 m `mode()` over a whole country is the most expensive export
  in the file. With `wait_time = 3` (pipeline units bug, see top) it will
  essentially always time out (UNVERIFIED).
- Masked → 0 = Water is A§5.
**Improvements / target shape:** registry row with `composite = "mode"`, `band = "label"`, years 2015- (S). Optionally add `composite = "probability_mean"`, which averages the per-class probability bands. That gives true class proportions without a categorical mode step, and is a better DW product for `download_lulc_proportions()` (M).

### `download_esa_worldcover_data()`  (R/gee_helpers.R:1452)
**Does:** downloads the ESA WorldCover v200 `Map` band (2021). **Used by:** `download_lulc_data()`; pipeline: indirectly (`lulc.product: esa_worldcover`).
**Verdict:** merge into `download_lulc(product = "esa_worldcover")`.
**Issues:**
- `year` is silently ignored. That is documented here, but `download_lulc_data()`
  passes nothing and does not warn when a user sets `year = 2020` (v100
  exists for 2020).
- Nodata in WorldCover is 0. Exporting a masked mosaic gives 0, which is not
  in the legend, and the zone builders do not handle it (UNVERIFIED; related
  to A§5 and A#10).
**Improvements / target shape:** registry rows `esa_worldcover` (v200, 2021) and optionally `esa_worldcover_v100` (2020). Explicitly setting a year that is not in `years` errors (S).

### `load_local_lulc_data()`  (R/gee_helpers.R:1545)
**Does:** reads a local LULC raster, warns if it does not cover the boundary bbox, and tags `lulc_product = "local"`. **Used by:** `download_lulc_data(product = "local")`; pipeline: unused (it reads local LULC with `safe_load_raster`).
**Verdict:** refactor and rename `read_lulc()`, then move it to the LULC module (not Earth Engine).
**Issues:**
- **It validates almost nothing [verified].**
  - A raster **without a CRS** is accepted, and the bbox check silently runs
    against an untransformed boundary.
  - A **continuous float** raster is accepted as categorical LULC.
- The bbox check compares bboxes, so a raster covering the bbox but not the
  polygon passes. A raster that partially covers the bbox warns, while the
  extent outside the polygon is irrelevant.
- The `"local"` tag means "you must pass class values". A#10 says nothing
  reads tags.
**Improvements / target shape:**
- `read_lulc(file, product = "local", legend = NULL, region = NULL)`.
- It errors without a CRS and errors on non-integer values.
- It checks that the values are a subset of the legend when `product` is
  known (A#10), and checks coverage against the polygon with
  `terra::extract`/`global` on the masked area.
- It returns a SpatRaster with a `levels()` table, i.e. a real categorical
  RAT instead of an `attr()` (S-M).
- Easy to test with synthetic rasters.

### `download_lulc_class_proportion()`  (R/gee_helpers.R:1663)
**Does:** for one class, builds a 0/1 mask in EE, reduces it to a PU-scale mean and exports it in the PU CRS through `crsTransform`, then trashes the Drive copy. **Used by:** pipeline only from the dead `download_class_proportion()` (0 live calls). `make_normalised_raster()` names it in a warning message.
**Verdict:** merge into `download_lulc_proportions()`, then deprecate.
**Issues:**
- **`...` is accepted and documented ("passed to `download_gee_layer()`"),
  but never used [verified: `...` absent from the body].** Misspelt or extra
  arguments vanish silently.
- **`boundary_layer` is required and validated, but never used [code].** The
  region comes from the PU corners.
- **It breaks out of the wait on the *first* matching Drive file [code].** It
  does not wait for the count to stabilise, so a multi-tile export is merged
  from whatever tiles had arrived.
- **Its file name differs from the sibling function's
  (`{product}_{class}_prop_{year}_{iso3}`).** So the same layer is cached
  twice under two names. Neither name matches the pipeline's discovery regex
  for this one.
- The year fallback loop starts at the *current* year, while
  `download_gee_layer()` starts at last year.
- No duplicate-task check, unlike `download_gee_layer()`.
- Dead local: `native_scale`.
- This is the only function exporting in the PU CRS directly. That is the
  better design, which the merged function should keep.
- Known: aggregation order, A#9; unanchored `drive_find(pattern=)` and
  `drive_trash`, A#13; network on cache hit, A§5.
**Improvements / target shape:** becomes `download_lulc_proportions(classes = "agriculture")`. The shim maps `class_name` / `class_values` to `classes` / a custom legend (S).

### `download_lulc_proportions()`  (R/gee_helpers.R:2018)
**Does:** for several classes, builds a mask per class, runs `reduceResolution(mean, bestEffort)` → `reproject` (WGS84 at PU scale), exports one file per class, then downloads and bilinear-reprojects to the PUs locally. **Used by:** pipeline **1 live call** (`03_features_default.R:308`). This is the main production GEE path.
**Verdict:** refactor. This is the function to keep as the public proportions API.
**Issues:**
- **`force_download = TRUE` does not force anything [code].**
  - It skips the Drive check and starts a new export.
  - But the wait loop (L2335-2342) immediately finds the *old* Drive file
    with the same name and downloads it.
  - In the pipeline it never even gets that far: `find_lulc_proportions_files()`
    returns the existing local files *before* calling elsar, whatever
    `force_gee` is set to.
  - So `--force-gee` has no effect.
- **Only the first tile of each export is downloaded [code].**
  `drive_download(matching[1, ])` is taken without waiting for, or merging,
  further tiles. A PU-scale float export is usually one file, but a large
  country, fine PUs or a 0.1 km grid can split, and the result is then
  silently truncated (UNVERIFIED at what size EE splits).
- **The pre-network cache check is defeated by the year fallback [code].**
  - The local check (L2096-2117) uses `year = last calendar year`.
  - When that year is unavailable (ESRI lags about a year), the files are
    written under the fallback year.
  - So every rerun misses the cache, starts conda, Python, auth and EE, and
    only then hits the in-loop check (L2237).
  - That is the specific mechanism behind A§5's "cached runs still need the
    network", and why the pipeline wrote its own lookup.
- **The naming puts `iso3` before the class
  (`…_{year}_{iso3}_{class}`) [code].** That breaks:
  - the naming-conventions vignette (A§4);
  - the pipeline's `has_pending_gee_exports()`, whose regex `_{ISO3}($|\.)`
    can never match these task descriptions. So its "exports already running"
    guard is blind exactly for this function.
- **It never checks for running tasks.** A rerun after a timeout submits a
  second identical export. Two same-named Drive files then exist, and
  `[1, ]` picks one arbitrarily (this compounds A#13).
- `boundary_layer` is required but unused (as in its sibling).
- The doc says `wait_time` defaults to "5" (inherited), but it is 30.
- It returns a list of in-memory rasters on a fresh run but file-backed
  rasters on a cache hit, with different `attr()`s (see `download_gee_layer`).
- Known: `bestEffort` and two-stage resampling, A#9; regex, A#13; region and
  masked pixels, A§5.
- Downstream: the pipeline's `check_gee_aggregation_safety()` computes an
  "intermediate resolution for multi-step aggregation", but elsar has no
  argument to receive it. The value is only logged, and the config comment
  "Automatically disabled if aggregation ratio exceeds GEE limits" is false.
**Improvements / target shape:**
```r
download_lulc_proportions(region, product = "esri_10m",
                          classes = c("agriculture", "built_area"),
                          year = NULL, output_dir, overwrite = FALSE,
                          session = elsar_gee_session(), timeout = 30)
```
- It returns **one multi-layer SpatRaster** (a layer per class, named by
  class), written as one COG plus a sidecar, instead of a list of files.
  That fixes the naming, halves the number of exports, and makes "all classes
  cached" a single file check.
- Classes are stacked in EE with `ee.Image.cat()` and exported once on the PU
  grid in the PU CRS (`crs = ee_crs(pus)`, `crsTransform`), so there is no
  local bilinear reproject.
- `overwrite = TRUE` deletes the Drive and local copies before export.
- File stem: `lulc_prop_{product}_{year}_{iso3}.tif`, from the layer registry.
- The pipeline then drops `find_lulc_proportions_files()`,
  `check_gee_aggregation_safety()` and `has_pending_gee_exports()`.
- Effort: M on top of the A#9/A#13 work.

### `download_lulc_data()`  (R/gee_helpers.R:2465)
**Does:** dispatches on `product` to the ESRI, DW or ESA wrapper, or to the local loader, and re-tags `lulc_product`. **Used by:** `check_and_download_required_layers()`; pipeline: indirectly.
**Verdict:** refactor into `download_lulc()`. Its dispatch becomes a registry lookup.
**Issues:**
- `year` is forwarded only to Dynamic World. ESRI silently ignores it
  [verified], and so does ESA.
- `product = "local"` requires `boundary_layer` (an `sf`) although it is only
  an optional coverage check. And `gee_project` is unused.
- Mixing "download" and "read local" behind one verb is confusing. The local
  path belongs in `read_lulc()`.
- Three layers of identical argument validation (dispatcher, wrapper, engine).
**Improvements / target shape:** `download_lulc(region, product = "esri_10m", year = NULL, grid = c("native","pus"), output_dir, overwrite = FALSE, session = elsar_gee_session(), timeout = 30)` is a thin call to `download_gee_layer(product = product, reducer = "mode", ...)` that returns a categorical SpatRaster with a legend RAT (S once the engine exists).

### `check_and_download_required_layers()`  (R/gee_helpers.R:2611)
**Does:** given the pipeline's `data_info` sheet, decides whether LULC and/or Pasturelands are needed (by hard-coded `data_name` strings), looks for local files by regex, and prompts or auto-downloads. **Used by:** pipeline (1 call, with `skip_lulc = use_proportions`).
**Verdict:** deprecate. It is pipeline orchestration, and its replacement is the layer registry and `elsar_build()` (A§6).
**Issues:**
- **Non-interactive mode turns every download failure into a warning and
  returns normally [verified].** With mocked downloads that fail,
  `check_and_download_required_layers(..., interactive = FALSE)` emits two
  warnings and returns `NULL`. The pipeline's `strict` branch around this
  call (03_features_default.R:367-389) can therefore **never fire** for
  Pasturelands timeouts. A strict `--auto` run continues without the layer.
- **The local-file regex gives false positives [verified].** A file
  `dynamic_world_agriculture_prop_2025_GHA.tif` (a class-proportion output)
  counts as "LULC present", so the categorical download is skipped.
- It is coupled to the pipeline's sheet vocabulary ("Urban Greening
  Opportunities", "Restore Zone", ...) inside a package function, and the
  required-layer logic differs from the pipeline's own (the pipeline gates
  Pasturelands on "Manage Zone"; elsar gates it on the name "Pasturelands").
- The double `readline` prompt ("Do you have GEE access…") adds nothing; the
  error would say so anyway.
- It calls `elsar::download_*` from inside elsar (namespace-qualified
  self-calls). That is harmless, but unusual.
- `interactive = TRUE` is the default, and `base::interactive()` still gates
  the prompt. So its two meanings of "interactive" are folded together:
  passing `TRUE` in a batch job auto-downloads.
**Improvements / target shape:** during deprecation, rethrow errors when `interactive = FALSE` (or add `on_error = c("warn","stop")`) so the pipeline's strict mode works now (S). Long term, the registry says which builder needs which GEE product, and `elsar_build()` fetches it (L, part of A§6).

---

## R/elsar_setup_gee.R

### `elsar_setup_gee()`  (R/elsar_setup_gee.R:55)
**Does:** one-call bootstrap: find or install conda, create or reuse `elsar_ee`, and optionally authenticate EE and Drive. **Used by:** users; pipeline: unused (Dockerfile and `setup_windows.ps1` do this by hand, with env `ee`, Python 3.11 and `openssl=3.0`).
**Verdict:** fix (small), keep public.
**Issues:**
- It inherits the reuse-without-checking problem from `create_gee_conda_env()`.
  A rerun cannot repair a broken env (no `rebuild` argument).
- It does not record or print which env `initialize_earthengine()` will
  *actually* use. `RETICULATE_PYTHON` and the other candidate names win over
  `elsar_ee`, so setup can report success for one env while downloads use
  another.
- `authenticated` is `TRUE` even when it skipped because a credentials file
  existed (it may be stale; see `earthengine_cred_paths`).
- The pipeline's Docker image pins `openssl=3.0` to avoid the very OpenSSL
  mismatch that `initialize_earthengine()` patches with `LD_LIBRARY_PATH`.
  elsar's env does not pin it.
- Tests check the arguments only; that is reasonable, since side effects are
  heavy.
**Improvements / target shape:** `elsar_setup_gee(python_version = "3.12", install_conda = interactive(), authenticate = interactive(), drive = TRUE, project = NULL, rebuild = FALSE)`. It:
- takes a pinned `earthengine-api` spec and `openssl` from an internal
  constant;
- ends by running `elsar_check_setup("earth_engine")`;
- returns `python` and `api_version` (S).
- Rename `authenticate_drive` → `drive` and `gee_project` → `project` (with a soft-deprecated alias) to match `elsar_gee_session(project=)` (S).
- Add `credentials = "path/to/service-account.json"` once A§5's
  service-account path exists (M).

### `resolve_conda()`  (R/elsar_setup_gee.R:105)
**Does:** returns the conda base, or installs miniconda if allowed. **Used by:** `elsar_setup_gee()`; tested with mocks.
**Verdict:** keep (env module).
**Issues:**
- `reticulate::install_miniconda()` installs Anaconda-channel miniconda. For
  institutional users this raises Anaconda licence/ToS questions; the pipeline
  Dockerfile deliberately uses Miniforge (comment at Dockerfile:66) [code].
**Improvements / target shape:** prefer Miniforge (`reticulate::install_miniconda()` cannot do this, so either document it or download Miniforge with a checksum), or at least state the channel in the message (S).

### `authenticate_gee()`  (R/elsar_setup_gee.R:150)
**Does:** `ee$Authenticate()` if no credentials file exists, optional `Initialize(project)`, and `drive_auth()` if no in-session token. **Used by:** `elsar_setup_gee()`.
**Verdict:** fix → becomes the auth module behind `elsar_gee_session()`.
**Issues:**
- `googledrive::drive_has_token()` only reports a token *loaded in this
  session* (`inherits(.auth$cred, "Token2.0")`, verified in googledrive
  2.1.2). In a fresh session it is always `FALSE`, so this always calls
  `drive_auth()`. With several cached Google accounts, `drive_auth()` prompts
  for an account choice each time.
- Credentials existence is not validity (see `earthengine_cred_paths`).
- Interactive-only and full Drive scope: A§5.
**Improvements / target shape:** `gee_auth(project, credentials = NULL, drive = TRUE, email = getOption("gargle_oauth_email"))`. It:
- uses `ee$ServiceAccountCredentials` when `credentials` is a key file;
- requests `scopes = "https://www.googleapis.com/auth/drive.file"` (enough
  for files this app created);
- probes with `ee$Initialize()` (S-M).

---

## R/elsar_check_setup.R

### `elsar_check_setup()`  (R/elsar_check_setup.R:45)
**Does:** runs the per-tier probes and prints or returns a `tier/item/status/detail/fix` data frame. **Used by:** users; pipeline: unused (`R/diagnostics.R::run_diagnostics()` re-implements the EE part, with `ee$Initialize()` and no project).
**Verdict:** fix (the EE tier only), keep public.
**Issues:**
- See `check_tier_earth_engine` and `safe_try` for the substance.
- The API is good: it never errors, returns a data frame and has an
  actionable `fix` column. It is the right model for a future
  `elsar_check_stack()`.
**Improvements / target shape:** add `tiers = "earth_engine", deep = FALSE`. `deep = TRUE` imports `ee` in the selected env and runs `ee$Initialize(project)` when a project is given (still read-only) (S). Then the pipeline can call it instead of its own diagnostics (S on the pipeline side).

### `setup_record()`  (R/elsar_check_setup.R:78)
**Does:** builds a one-row record data frame. **Used by:** internal.
**Verdict:** keep.
**Issues:** none.
**Improvements / target shape:** none.

### `safe_try()`  (R/elsar_check_setup.R:90)
**Does:** `tryCatch` returning a default on error **or warning**. **Used by:** internal (setup checks).
**Verdict:** fix.
**Issues:**
- Turning *warnings* into failures throws away real results. For example,
  `reticulate::conda_list()` and `chromote::find_chrome()` warn in benign
  cases, and the probe then reports "missing" [code; specific warnings
  UNVERIFIED].
- It has no roxygen block (no `@noRd`).
**Improvements / target shape:** catch errors only; wrap known-noisy calls in `suppressWarnings()` explicitly (S).

### `check_tier_core()`  (R/elsar_check_setup.R:97)
**Does:** reports R, sf (+ GDAL/GEOS/PROJ), terra (+ GDAL) and elsar versions. **Used by:** internal; tested.
**Verdict:** keep.
**Issues:**
- "R ok" is unconditional. Once `Depends: R (>= 4.1)` is fixed (A§3), compare
  against it.
- It does not flag sf and terra being built against different GDAL/PROJ
  versions, a common source of CRS drift.
**Improvements / target shape:** warn if `sf` and `terra` report different GDAL or PROJ major.minor versions (S).

### `check_tier_protected_areas()`  (R/elsar_check_setup.R:156)
**Does:** checks wdpar, a Chromium browser via chromote, and the archive package. **Used by:** internal.
**Verdict:** keep (out of EE scope; reviewed only for consistency).
**Issues:** none new.
**Improvements / target shape:** none.

### `check_tier_earth_engine()`  (R/elsar_check_setup.R:219)
**Does:** checks reticulate, conda, an EE-named env directory, googledrive and its token, the credential file, and `gdalsrsinfo`. **Used by:** internal; tested only for shape.
**Verdict:** fix.
**Issues:**
- **"Google Drive auth" is always "no Drive token cached yet" in a fresh
  session [code + verified googledrive internals]**, because it uses
  `drive_has_token()` (in-memory only). The report is misleading on every
  correctly set-up machine.
- **"Earth Engine conda env: ok" means only that a directory exists.** There
  is no check that `ee` is installed in it, which is exactly the partial-env
  failure from `create_gee_conda_env()`.
- It ignores `RETICULATE_PYTHON`, the route the pipeline and Docker actually
  use. A machine where `RETICULATE_PYTHON` points at an env without `ee`
  reports "ok" (because `elsar_ee` exists), while downloads will use the
  broken interpreter.
- The credential-path check inherits the Windows path bug.
- `gdalsrsinfo`: "warn". This becomes unnecessary once `ee_crs()` exists, so
  drop it then.
**Improvements / target shape:**
- Report the *resolved* interpreter (the same function the session uses)
  and, for that interpreter, whether `ee` imports (`system2(python, c("-c", "import ee; print(ee.__version__)"))`, which makes no change to the R session).
- Check the gargle cache for Drive tokens: `gargle::gargle_oauth_sitrep()`
  or list `gargle::gargle_oauth_cache()` (file names only, never contents).
- Effort: S.

### `print_setup_report()`  (R/elsar_check_setup.R:362)
**Does:** prints the report with `[ ok ]`/`[MISS]` tags and fix lines. **Used by:** internal.
**Verdict:** keep.
**Issues:** none. When the package moves to cli (A§4), switch to `cli::cli_bullets` without changing the returned data frame.
**Improvements / target shape:** cli port (S, with A§4).

---

## Cross-cutting

### Patterns across the 24 gee_helpers functions
1. **Three export engines, not one.** `download_gee_layer`,
   `download_lulc_class_proportion` and `download_lulc_proportions` each have
   their own:
   - year search (three variants: date-only from last year, bounds from this
     year, bounds from this year);
   - bbox code (two variants);
   - wait loop (three variants: tile-count stabilisation, first-match,
     first-match per class);
   - Drive matching (three regexes) and cleanup;
   - file-naming scheme (three schemes);
   - output contract (`attr()` sets on first runs and cache hits differ).

   Almost every new finding above is a divergence between these copies.
2. **Provenance via `attr()` on S4 rasters is unreliable** and also drives
   behaviour (`make_normalised_raster()`). It is lost on every file
   round-trip [verified], so results depend on cache state. Replace it with a
   JSON sidecar plus a categorical RAT.
3. **Dead code:**
   - `safe_gee_getinfo`, `download_from_drive`, `cleanup_earthengine`
     (13 no-op calls);
   - the `temp_env` contract;
   - `googledrive_folder`;
   - `pu_crs`, `native_scale`, `native_scale_m`, `source_projection`;
   - wrappers' `is.null(result)` branches.
4. **Arguments accepted and ignored:**
   - `...` in `download_lulc_class_proportion`;
   - `boundary_layer` in both proportion functions;
   - `year` for ESRI and ESA via `download_lulc_data`;
   - `python_version` on reuse;
   - `force_download`.
5. **Testability.** Nothing can be mocked at a single seam today. With
   `session` injected and the export engine split as
   `ee_build_image()` → `ee_export()` → `sink_fetch()` → `merge_tiles()`:
   - everything except `ee_export` and `sink_fetch` is pure R on synthetic
     rasters;
   - those two can be faked with `local_mocked_bindings`.

   That covers A§3's "mock EE and Drive" target.
6. **Downstream drift.** The pipeline compensates for elsar with:
   - its own cache lookup, task check, Drive preflight and diagnostics;
   - an aggregation "safety" calculator whose output nothing consumes.

   Its `wait_time` is in seconds against elsar's minutes. All of these go
   away with the target API below.

### Proposed public Earth Engine API (replaces 12 exports with 9)

| Function | Purpose | Signature |
|---|---|---|
| `elsar_setup_gee()` | One-off: conda and a pinned `earthengine-api` env, optional auth, then self-check | `elsar_setup_gee(python_version = "3.12", install_conda = interactive(), authenticate = interactive(), drive = TRUE, project = NULL, rebuild = FALSE)` |
| `elsar_check_setup()` | Read-only diagnosis of all tiers | `elsar_check_setup(tiers = c("core", "protected_areas", "earth_engine"), quiet = FALSE, deep = FALSE, project = NULL)` |
| `elsar_gee_session()` | Explicit, memoised EE session: env, auth (user or service account), project, sink | `elsar_gee_session(project, credentials = NULL, sink = c("drive", "gcs", "direct"), bucket = NULL, python = NULL, refresh = FALSE)` |
| `elsar_gee_products()` | Product registry: id, asset, band, composite, legend, nodata, scale factor, native scale, years, version, citation, licence | `elsar_gee_products(type = NULL)` → tibble |
| `download_gee_layer()` | Generic export onto the native or PU grid, with a provenance-aware cache | `download_gee_layer(region, product = NULL, asset_id = NULL, band = NULL, year = NULL, composite = NULL, grid = c("native", "pus"), reducer = NULL, output_dir, overwrite = FALSE, session = elsar_gee_session(), timeout = 30)` |
| `download_lulc()` | Categorical LULC (ESRI, DW, ESA) with a legend RAT | `download_lulc(region, product = "esri_10m", year = NULL, grid = c("native", "pus"), output_dir, overwrite = FALSE, session = elsar_gee_session(), timeout = 30)` |
| `download_lulc_proportions()` | Per-class proportion stack on the PU grid (one multi-layer COG) | `download_lulc_proportions(region, product = "esri_10m", classes = c("agriculture", "built_area"), year = NULL, output_dir, overwrite = FALSE, session = elsar_gee_session(), timeout = 30)` |
| `read_lulc()` | Validate and read a local categorical LULC raster | `read_lulc(file, product = "local", legend = NULL, region = NULL)` |
| `elsar_gee_tasks()` | List or cancel this project's running elsar exports (replaces the pipeline's `check_gee_tasks`) | `elsar_gee_tasks(session = elsar_gee_session(), pattern = NULL, cancel = FALSE)` |

Notes:
- `region` is `elsar_region()` (A§6). Until that exists, it accepts a PU
  `SpatRaster` (grid and extent) with an optional boundary `sf` for clipping.
- `iso3` disappears from the download signatures. It is only a file-name
  component, and comes from the region (A§4).
- Pasture is reached through `download_gee_layer(region, product = "gpw_cultivated", grid = "pus", reducer = "mean")`.
- `timeout` is in minutes and may be fractional.
- Internal modules (A§5):
  - `env/`: `ee_env_candidates`, `find_conda_*`, `ensure_gee_env`;
  - `auth/`: `gee_auth`, credential paths;
  - `region/`: `ee_region` (densified, antimeridian-aware), `ee_crs`;
  - `export/`: `ee_build_image`, `ee_export`, `ee_wait_task`;
  - `sinks/`: `drive_sink_fetch`, `gcs_sink_fetch`, `direct_fetch`;
  - `merge/`: `merge_tiles(files, …)`;
  - `cache/`: key hash plus sidecar.

### Mapping old -> new

| Old | New | How |
|---|---|---|
| `download_gee_layer()` | `download_gee_layer()` (new signature) | Soft-deprecate `boundary_layer`/`iso3`/`gee_project`/`file_prefix`/`scale`/`datatype`/`wait_time`/`year_override`/`aggregate_to_pus`/`aggregation_reducer`, mapped to `region`/`session`/`product`/`timeout`/`year`/`grid`/`reducer`; drop `googledrive_folder` |
| `download_esri_lulc_data()` | `download_lulc(product = "esri_10m")` | deprecate_soft shim |
| `download_dynamic_world_data()` | `download_lulc(product = "dynamic_world")` | shim |
| `download_esa_worldcover_data()` | `download_lulc(product = "esa_worldcover")` | shim |
| `download_lulc_data()` | `download_lulc()`; `product = "local"` → `read_lulc()` | shim |
| `download_global_pasture_data()` | `download_gee_layer(product = "gpw_cultivated" / "gpw_natural")` | shim |
| `download_lulc_class_proportion()` | `download_lulc_proportions(classes = <one>)` | shim (`class_values` → custom legend) |
| `download_lulc_proportions()` | `download_lulc_proportions()` (new signature, returns a stack) | shim returns `as.list(stack)` for one version |
| `load_local_lulc_data()` | `read_lulc()` | shim |
| `check_and_download_required_layers()` | layer registry + `elsar_build()` (A§6) | deprecate in 0.6, remove in 1.0. Interim fix: errors propagate when `interactive = FALSE` |
| `elsar_setup_gee()` | `elsar_setup_gee()` | rename `authenticate_drive` → `drive`, `gee_project` → `project` (aliases) |
| `elsar_check_setup()` | `elsar_check_setup()` | add `deep`, `project` |
| `initialize_earthengine()` (internal) | `elsar_gee_session()` | - |
| `cleanup_earthengine()`, `safe_gee_getinfo()`, `download_from_drive()` (internal) | delete; Drive logic → `drive_sink_fetch()` | - |
| `safe_task_status()` | `ee_wait_task()` | - |
| `get_crs_wkt()` | `ee_crs()` | - |
| `merge_tiles()` | `merge_tiles(files, …)` | - |
| `create_gee_conda_env()`, `find_conda_base()`, `find_conda_exe()`, `find_env_python()`, `elsar_gee_env()`, `resolve_conda()` | env module (`ensure_gee_env()` etc.) | - |
| `require_gee_deps()` | `rlang::check_installed()` inside `elsar_gee_session()` | - |
| `earthengine_cred_paths()`, `authenticate_gee()` | auth module (`gee_auth()`) | - |
| pipeline `check_gee_tasks()` / `has_pending_gee_exports()` | `elsar_gee_tasks()` | pipeline change |
| pipeline `find_lulc_proportions_files()`, `check_gee_aggregation_safety()` | removed; cache and grid logic live in elsar | pipeline change |

### Quick wins that need no redesign (candidates for 0.4.2/0.5, each S)
1. `check_and_download_required_layers()`: rethrow in non-interactive mode,
   so pipeline strict mode works.
2. Pipeline: treat `wait_time` as minutes, or elsar adds `timeout_secs`.
   Fix the config comments and validator.
3. `download_lulc_data()`: forward `year` to ESRI, and error for ESA.
4. `download_lulc_class_proportion()`: remove `...` or forward it; mark
   `boundary_layer` unused.
5. `download_lulc_proportions()`:
   - make `force_download` delete the old Drive file before exporting;
   - download and merge all tiles;
   - check for running tasks before exporting.
6. `download_gee_layer()`:
   - set `pre_aggregated`/`aggregation_reducer` on cache hits too, or better,
     stop branching on them;
   - add `filterBounds` to the year search;
   - fix `@return`.
7. `initialize_earthengine()`: stop at "Python initialised without ee"
   instead of building an env.
8. `check_tier_earth_engine()`:
   - check the gargle cache instead of `drive_has_token()`;
   - check that `ee` imports in the resolved interpreter;
   - fix the Windows credential path.
9. Delete `safe_gee_getinfo()`, `download_from_drive()` and
   `cleanup_earthengine()`.
10. DW file name: drop the duplicate `{year}`.

### Verdict counts (34 functions)

| Verdict | Count | Functions |
|---|---|---|
| keep | 8 | `elsar_gee_env`, `find_conda_exe`, `find_env_python`, `resolve_conda`, `setup_record`, `check_tier_core`, `check_tier_protected_areas`, `print_setup_report` |
| fix | 9 | `earthengine_cred_paths`, `create_gee_conda_env`, `merge_tiles`, `elsar_setup_gee`, `authenticate_gee`, `elsar_check_setup`, `safe_try`, `check_tier_earth_engine`, `find_conda_base` |
| refactor | 8 | `require_gee_deps`, `get_crs_wkt`, `safe_task_status`, `initialize_earthengine`, `download_gee_layer`, `load_local_lulc_data` (→ `read_lulc`), `download_lulc_proportions`, `download_lulc_data` (→ `download_lulc`) |
| merge | 5 | `download_esri_lulc_data`, `download_dynamic_world_data`, `download_esa_worldcover_data` → `download_lulc`; `download_global_pasture_data` → `download_gee_layer(product=)`; `download_lulc_class_proportion` → `download_lulc_proportions` |
| deprecate | 1 | `check_and_download_required_layers` |
| delete | 3 | `safe_gee_getinfo`, `cleanup_earthengine`, `download_from_drive` |
| make internal | 0 | (all internals already internal; the exported building blocks become shims instead) |

Totals: keep 8, fix 9, refactor 8, merge 5, deprecate 1, delete 3 = 34.

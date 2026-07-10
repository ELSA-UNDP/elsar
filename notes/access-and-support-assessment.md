# elsar: access, uptake, and supportability assessment

Working document, prepared 2026-07-08 against `elsar` 0.4.0 (commit `538ea9d`).
Audience: ELSA team, deciding how to open the package up and what support to commit to.

---

## Executive summary

**Recommendation: do not publicise the package to a wider audience until the four silent-failure bugs in §1 are fixed.** They are cheap to fix. Left in place, they convert every new non-specialist user into a support ticket that requires forensic investigation — because the package produces a plausible-looking wrong answer rather than an error.

Once those are fixed, the access problem has a clean structure. There are really *three* packages inside `elsar`, with very different setup costs:

| Tier | Surface | External requirements | Can we support it? |
|---|---|---|---|
| **A. Core** | boundary, planning units, normalise, zone builders from local rasters, all plotting | GDAL/GEOS/PROJ only | **Yes, fully** |
| **B. Protected areas** | `make_protected_areas(from_wdpca = TRUE)` | + Chrome/Chromium, libarchive, Protected Planet uptime | **Best-effort** |
| **C. Earth Engine** | the 8 `download_*` functions (`gee_helpers.R`, 2,588 lines) | + conda, Python 3.12, GEE account & cloud project, Google Drive OAuth | **Not today.** See §4. |

That tiering *is* the answer to "what can and cannot we accommodate." It is defensible, easy to explain, and it maps onto a dependency change we should make anyway (§2.1).

---

## 1. Correctness issues that block wider release

These four were reproduced by running R against the installed package. They are not style problems; each silently produces a wrong scientific result.

### 1.1 `make_planning_units()` accepts a lat/long boundary and returns a one-cell raster

`pu_size` is metres. `boundary_proj` is documented as "must be in a projected CRS." Nothing checks it. Passing Nepal in EPSG:4326:

```
Iteration 1: 1 PUs at resolution 3900 m
...
Iteration 19: 1 PUs at resolution 15 m
Final PU layer: 1 PUs at 15 m resolution.
```

Returns a **1-cell `SpatRaster`**. No error, no warning. The log actively misreports — "15 m" is 15 *degrees*. The auto-sizing loop's success condition (`1 <= threshold_soft`) is satisfied on every iteration, so it shrinks 19 times, hits `min_pu_size`, and declares success.

Everything downstream — `make_normalised_raster()`, every zone builder, `get_coverage()` — then runs happily on a one-pixel country.

The trap is baited by our own README, which chains `make_boundary()` → `make_planning_units()`. That works *only* because `make_boundary()`'s `custom_projection = TRUE` default happens to reproject. A user who sets `custom_projection = FALSE`, or who brings their own boundary from `sf::st_read()` — the single most likely thing a GIS analyst does — gets a one-pixel country and no indication anything is wrong.

**Fix:** assert `!sf::st_is_longlat(boundary_proj)` at entry. Three lines.

### 1.2 `make_boundary(limit_to_mainland = TRUE)` always errors

[`R/make_boundary.R:77`](R/make_boundary.R#L77) uses `.data` — the rlang data *pronoun* — where a data frame is meant:

```r
dplyr::slice(which.max(as.numeric(sf::st_area(.data))))
#> Error: no applicable method for 'st_area' applied to an object
#>        of class "rlang_data_pronoun"
```

An exported, documented argument of two functions that cannot be set to `TRUE`. Two further latent bugs sit in the same block: the mainland filter runs *before* the `iso3` filter (so it would pick the largest polygon of the whole input dataset, then filter — yielding 0 rows), and the `st_transform()` to WGS84 sits only in the `else` branch. In `make_planning_units()` the argument is entirely dead: *"Reserved for future use."*

### 1.3 A wrong `iso3` returns a 0-row `sf`, silently

```r
make_boundary(boundary_dat, iso3 = "NEP", iso3_column = "iso3cd", custom_projection = FALSE)
#> returned class: sf/data.frame  nrow: 0
```

(`"NEP"` is a plausible typo for Nepal's `"NPL"`.) With the `custom_projection = TRUE` default it instead surfaces as a GDAL error about `PARAMETER["central_meridian",NA]` — the emptiness leaking into a WKT string. There is no ISO3 lookup in the package; `is_valid_iso3()` is a *format* check (`^[A-Za-z]{3}...$`), so `"XYZ"` passes, and it's called from only 4 of ~30 functions taking `iso3`.

**Fix:** `nrow(nb) > 0` check with a message naming the available codes.

### 1.4 The PostgreSQL path is dead code

[`R/elsar_load_data.R:122`](R/elsar_load_data.R#L122):

```r
con <- RPostgres::dbConnect(RPostgres::Postgres(), !!!db_info)
```

`dbConnect` is a plain S4 generic, not an rlang-quoting function. `!!!x` parses as `!(!(!x))`:

```r
f <- function(...) list(...); f(!!!list(host = "h", port = 5432L))
#> Error: invalid argument type
```

Every `file_name = "postgres"` call errors before reaching the database. Also in this function: `RPostgres` is in `Suggests` but called with `::` and no `requireNamespace()` guard; the connection is never closed (`dbDisconnect` appears nowhere in the package); and `file_lyr`/`iso3_column`/`iso3` are interpolated into SQL via `glue()` with no identifier quoting.

Separately, `filter_sf()` swallows every read error and returns `NULL`. If *all* layers fail, `elsar_load_data()` returns a `0×0 tibble` — not an `sf` — while logging *"Loaded 0 features from 0 layer(s)."*

### 1.5 One hazard to flag before any non-specialist touches the GEE path

[`R/gee_helpers.R:1778`](R/gee_helpers.R#L1778) polls for the export with `drive_find(pattern = file_name, type = "tif")` — **unscoped by folder, across the user's entire Drive** — then downloads every match and `drive_trash()`es it. The pattern (`esa_worldcover_2021_NPL`) is specific enough that accidental matches are unlikely rather than impossible, but the function trashes files it did not create, and a `googledrive_folder` argument already exists elsewhere in the file and is simply not used here. `download_from_drive()` a few hundred lines up does scope correctly with `drive_ls(path = drive_folder)`.

This is a "fix before onboarding anyone who isn't us" item, not an emergency.

---

## 2. Access blockers

### 2.1 You cannot install the pure-R core without the Earth Engine and Chrome dependencies

`DESCRIPTION` puts `wdpar`, `reticulate`, `googledrive`, and `rappdirs` in **`Imports`** — hard dependencies. Consequences:

- `wdpar` → `chromote` (needs **Chrome/Chromium**) and `archive` (needs **libarchive** headers *to build*). Every elsar user must have these even if they never touch protected areas.
- `reticulate` and `googledrive` are pulled in for a code path most users will never run.

The tell that this is wrong: **the code already guards all four with `requireNamespace()`**, as though they were `Suggests`. Demoting them is nearly free and is the single highest-leverage access change available.

Meanwhile the dependency table is inverted at the other end — `RPostgres`, `scales`, and `png` are in `Suggests` and called unguarded at runtime. `DBI` is in `Suggests` and used nowhere at all.

### 2.2 There is no setup infrastructure of any kind

Verified by grep across the repo: no `zzz.R`, no `.onLoad`/`.onAttach`, no `SystemRequirements:` field, no setup vignette, no exported setup helper. The README's installation section is one line (`remotes::install_github(...)`) and never uses the words *conda*, *Python*, *Earth Engine*, *Drive*, *postgres*, *GDAL*, or *Chrome*.

What a new user actually needs, and whether we help:

| Requirement | Package helps? | Documented? |
|---|---|---|
| GDAL / GEOS / PROJ / sqlite3 / udunits / libxml2 | No | No — no `SystemRequirements` |
| `gdalsrsinfo` CLI binary | Warns, then silently degrades to WKT2 (may fail in GEE) | No |
| conda/mamba + Python 3.12 | Auto-creates env *if conda exists*; `stop()`s otherwise | Only in the error string |
| GEE account + cloud project | Validated, with a good `@param` | **Yes** — the one well-documented item |
| GEE auth | Fires interactive `ee$Authenticate()`; no service account | No |
| Google Drive account + quota | Fires interactive `drive_auth()` | No |
| Chrome/Chromium + libarchive | No | No |
| WDPA archive (100s of MB) | No | No |
| Postgres credentials | Code path is broken (§1.4); no env-var support | `@param` only |

Two further setup landmines: the conda search (`find_conda_base()`) checks a hardcoded path list and requires `<base>/envs` to pre-exist — it never consults `CONDA_PREFIX`, `Sys.which("conda")`, or `reticulate::conda_binary()`, so it misses micromamba, Homebrew miniconda, and GitHub Actions' `/usr/share/miniconda`. And when no pre-named env (`ee_compat`, `ee`, `gee`, `earthengine`, `earth-engine` — a convention documented nowhere) is found, it creates a PID-named temp env and removes it on exit, so the user **re-downloads Python 3.12 + earthengine-api every R session**.

### 2.3 Authentication is interactive-only

Both Google flows are bare interactive calls:

```r
if (!googledrive::drive_has_token()) googledrive::drive_auth()   # gee_helpers.R:744, :2009
if (!file.exists(cred_path)) ee$Authenticate()                    # gee_helpers.R:409
```

No service-account support anywhere — no `ServiceAccountCredentials`, no `EARTHENGINE_TOKEN`, no `GOOGLE_APPLICATION_CREDENTIALS`. **Nothing on the GEE path can run headless**: not in CI, not on a server, not in a scheduled pipeline, not in a container a country office is handed.

The Earth Engine credential probe also looks likely to be wrong off Linux. It checks `rappdirs::user_config_dir("earthengine")`, which resolves to `~/Library/Application Support/earthengine` on macOS and `%APPDATA%/earthengine/earthengine` on Windows — but the Python `earthengine-api` writes to `~/.config/earthengine/credentials` on all platforms. If that holds (unverified — `ee` isn't installed here), `ee$Authenticate()` re-fires on every call on Mac and Windows. Worth a five-minute check.

---

## 3. Support-surface gaps

Everything a would-be user or contributor looks for is absent:

- **Missing:** `CONTRIBUTING.md`, `SUPPORT.md`, `CODE_OF_CONDUCT.md`, `SECURITY.md`, `.github/ISSUE_TEMPLATE/`, PR template, `CITATION`/`CITATION.cff`.
- **Missing from `DESCRIPTION`:** `BugReports:`. pkgdown therefore renders no "Report a bug" link. **We have not told anyone where to ask for help.**

That last point deserves emphasis: the current de facto support channel is *emailing Scott*. Any uptake plan that doesn't replace that channel will simply increase the volume of email.

Corroborating weaknesses:

- **Tests cover ~8% of exports** (6 of 71; 22 assertions across 7 files). Zero tests for any zone builder, any plotting function, `elsar_load_data`, or `get_coverage`. The one GEE test calls `elsar_download_gee_layer()` — **a function that does not exist** — and is permanently green because two `skip()`s fire first.
- **58 of 64 example blocks are `\dontrun{}`**, so `R CMD check --as-cran` validates almost no example code. One (`make_planning_units`) uses ISO3 `"ZMB"` against a bundled dataset containing only BGD/IND/NPL/PAK — unrunnable by construction, masked by `\dontrun`.
- **The getting-started vignette teaches the deprecated API** (`make_custom_projection()` at `elsaR.Rmd:73`), its warning suppressed by the chunk options. `_pkgdown.yml:92` likewise indexes the deprecated alias and omits the replacement.
- `vignettes/elsaR.Rmd` opens with empty `# Overview` and `## Introduction` headings, and `library(prioritizr)` is loaded but never used (a heavy Suggests dep pulled in purely to be loaded).
- `vignettes/naming_conventions.Rmd` documents a `_t1/_t2/_t3` tier suffix and lowercase ISO3 that **no function in the package emits**.
- `build-multilingual.R` depends on undeclared `fs`, and an interrupted run strands `.bak` files with translated content clobbering the English `_pkgdown.yml`, `README.md`, and vignettes. `*.bak` is not in `.gitignore`.

**Healthy, and worth saying:** `@param` and `@return` coverage is 100% on non-internal topics. `pkgdown::check_pkgdown()` passes clean. `docs/` is properly untracked and CI-deployed. The R-CMD-check matrix (3 OSes × release + oldrel-1) is sensible. `naming_conventions.Rmd` and `data-info.Rmd` are genuinely good reference docs. The working tree is clean.

---

## 4. What we can and cannot support

Proposed commitment, to go in `SUPPORT.md`:

### Tier A — Core (supported)
Boundary, planning units, normalisation, zone builders from user-supplied rasters, coverage metrics, plotting.
Depends only on GDAL/GEOS/PROJ. Runs offline. Works on the bundled example data.
**We commit to:** bug reports triaged, reproducible failures fixed, examples that run.

### Tier B — Protected areas (best-effort)
`make_protected_areas(from_wdpca = TRUE)`.
Depends on Chrome/Chromium + libarchive on the user's machine, and on Protected Planet's availability and download-link markup — `wdpar` drives a headless browser to scrape it.
**We commit to:** documenting the system requirements, and forwarding upstream breakage to `wdpar`. **We cannot commit to** availability, because we do not control either dependency. Recommend the documented fallback: supply your own PA layer via `from_wdpca = FALSE`.

### Tier C — Earth Engine (user-facing and supported; needs onboarding work first)
The 8 `download_*` functions.
Requires a conda install, a Python 3.12 env, a GEE account with an approved Cloud project, and a Google Drive OAuth grant with free quota. Export waits default to 5 minutes, which for a 10 m LULC export over a large country is a timeout rather than a completion budget.

**Decision (confirmed by the team, 2026-07-09): Earth Engine is a first-class, user-facing part of the package. The user could be anyone — a country officer or an external researcher — and each brings and maintains their own GEE account and Cloud project.** That settles the gating question in the previous draft and it raises the stakes: the GEE onboarding path is no longer something we can route around by publishing outputs. It is now on the critical path for a share of *every* audience, at *every* skill level, so it has to become the most robust and self-explaining part of the setup, not the most fragile.

What that means concretely for the support boundary:

- **We own, and commit to:** a working install path, a self-diagnosing setup (`elsar_check_setup()`, §5), a one-call environment bootstrap (`elsar_setup_gee()`, §5), actionable error messages, and clear step-by-step docs for account → project → auth. When a non-specialist gets stuck, the package itself should tell them the next command to run.
- **We document but cannot own:** Google's GEE access approval, Cloud-project billing/quota, and Drive quota. These belong to the user's own Google account. A ticket that reduces to "Google hasn't approved my Earth Engine access" is one we can *guide* but not *close* — and the docs should set that expectation up front so it isn't a surprise.

The good news in the confirmed scenario: because each user runs on their own laptop against their own project, the **interactive** OAuth flows (`ee$Authenticate()`, `drive_auth()`) are actually acceptable for the common case — a browser popup is normal, not a blocker. The blocker for a non-specialist is everything *before* auth: getting a conda/Python environment onto their machine at all. That reframes the priority order below — the conda bootstrap, not service-account auth, is the headline unlock. Service-account/token auth still matters, but for a narrower set: our own CI (which currently tests zero of the GEE path), and the minority of users running on a shared server or scheduler where a browser popup isn't available.

Two ways to change that answer, in order of leverage:
1. **Add service-account auth** (§5, option 4). This converts "each user must obtain and maintain two Google OAuth grants" into "UNDP provisions one credential file." It moves Tier C from unsupportable to supportable, and is the single most consequential change on this list.
2. **Publish the derived layers.** Most users want the *outputs* of the GEE functions, not the functions. If the ELSA team runs the downloads centrally and publishes the resulting COGs, the vast majority of users never need Tier C at all.

Option 2 is worth serious thought — it may be that the GEE path should be an *internal data-production tool*, documented as such, and not part of the user-facing API at all. That would shrink the supported surface by 2,588 lines and eliminate the hardest onboarding cliff outright.

---

## 5. Ways to improve access and uptake

Six options, roughly ascending in cost. My recommendation follows.

### Option 1 — Fix the install cliff *(days; highest leverage per hour)*
- Demote `wdpar`, `reticulate`, `googledrive`, `rappdirs` to `Suggests`. The guards are already written.
- Promote `scales`, `png`, `RPostgres` to `Imports` or guard them. Drop `DBI`.
- Add `SystemRequirements:` and a `BugReports:` field.
- Add `elsar_check_setup()` — an exported diagnostic that reports, per tier, what's present and what's missing, with the exact command to fix each. This one function would deflect a large share of anticipated support email.

Effect: `install.packages("elsar")` works for Tier A users with no conda, no Chrome, no libarchive.

### Option 2 — Binary distribution via r-universe *(hours to set up)*
Publish to `https://elsa-undp.r-universe.dev`. Users then run `install.packages("elsar", repos = ...)` and get a **pre-compiled binary** on Windows and macOS — no compiler, no `remotes`, no source build of `sf`/`terra`. For a non-specialist on a locked-down work laptop this is often the difference between installing and not installing. It also gives us free `R CMD check` across platforms and a real version badge.

Prerequisite: Option 1, or the binaries still drag in Chrome/libarchive.

### Option 3 — Documentation restructure *(1–2 weeks)*
The current docs answer "what does this function do." They never answer "how do I run my country." Proposed articles:
- **Setup** — one page per tier, with `elsar_check_setup()` output as the acceptance test.
- **Run one country end-to-end** — a real, complete, credential-free worked example. This is the doc that determines uptake; nothing else comes close.
- **Bring your own data** — boundary sources (GADM vs Natural Earth vs UN geodata, and their differing ISO3 column names, which currently bite users because our defaults assume UN's `iso3cd`), raster requirements, CRS expectations.
- **Glossary** — planning unit, HII, Mollweide, tier, lock-in. Currently a non-specialist meets `pus` as a mandatory argument with no definition anywhere.

Also: fix the vignette to stop teaching the deprecated function, and reconcile `naming_conventions.Rmd` with what the code actually writes.

### Option 4 — GEE onboarding: bootstrap the environment *(1–2 weeks; now on the critical path)*
With the decision confirmed, this is no longer optional — it's what makes Tier C reachable for a non-specialist. Two parts, in priority order:

**4a. A one-call environment bootstrap — the headline unlock.** Export `elsar_setup_gee()` that: detects conda, and if absent offers `reticulate::install_miniconda()` rather than dying with `stop("Could not find conda installation")`; creates the earthengine env **once, with a stable name** (not a PID-named temp env re-downloaded every session); installs `earthengine-api`; and walks the user through the interactive `ee$Authenticate()` and `drive_auth()` grants. Fix `find_conda_base()` to consult `reticulate::conda_binary()`/`CONDA_PREFIX` so it finds micromamba and Homebrew installs. The current failure — *"install miniconda or anaconda"* with no further help — is the single steepest step for someone who has never heard of conda. A guided installer flattens it.

**4b. Service-account / token auth — for the narrower headless case.** Support `EARTHENGINE_TOKEN` and a service-account JSON for both Earth Engine and Drive, exported as `elsar_gee_auth()`. In the confirmed scenario, interactive auth is *fine* for the laptop user, so this is not the main onboarding path — but it's what lets **our CI test the GEE path** (currently zero coverage), and it serves the minority running on shared servers, schedulers, or containers where a browser popup isn't available. Also fix the likely macOS/Windows credential-path bug (§2.3) so returning users aren't re-prompted every call.

### Option 5 — Container image *(1 week, then low maintenance)*
A `rocker/geospatial`-based image with GDAL, conda + `earthengine-api`, Chromium, and elsar preinstalled. Publish to GHCR. Pair with a Binder or GitHub Codespaces config so a user clicks a link and has a working environment in 60 seconds, then authenticates to **their own** GEE project interactively in the browser.

Given the confirmed decision, this rises in priority: it is the cleanest way to get the *least* technical users — who will never install conda successfully — onto the full Tier C workflow, while still respecting "each user brings their own GEE account and project." It composes with 4b for the headless variant but does **not** require it for the interactive laptop-in-a-browser case.

### Option 6 — A guided front-end *(months)*
A Shiny app or `targets` pipeline template wrapping the standard country workflow. Only worth doing if the target audience genuinely cannot write R — and only *after* Options 1–5, because a GUI over a package that silently returns one-cell rasters is a liability, not an asset.

### Complement (not a substitute) — publish the derived layers
Even with GEE user-facing, many users will want the *outputs* of the `download_*` functions, not to run them. Publishing the derived COGs (e.g. via the r-universe data repo or a UNDP data portal) gives the risk-averse or low-bandwidth user a no-GEE path to the same layers, and reduces load on the onboarding funnel. This is now a *complement* to Tier C rather than a way to retire it.

### Recommended sequence

1. **Phase 0 (now, before any outreach):** the four bugs in §1, plus the Drive-scoping fix in §1.5. Nothing else matters until a wrong CRS is an error rather than a wrong answer.
2. **Phase 1:** Option 1 + Option 2 + `SUPPORT.md`/`CONTRIBUTING.md`/issue templates with a mandatory reprex + `sessionInfo()`. Minimum viable "we are open for business" posture. `elsar_check_setup()` lands here and must be tier-aware — it has to diagnose the GEE toolchain, not just the core.
3. **Phase 2:** Option 3 **and** Option 4 together — no longer conditional. Because Tier C is user-facing for everyone, the onboarding docs and the `elsar_setup_gee()` bootstrap are the same deliverable and ship as a pair. This is the phase that actually determines GEE uptake.
4. **Phase 3:** Option 5 (container/Codespaces) for the least-technical users and for workshops. Ship the derived-layer complement alongside. Revisit Option 6 only with evidence of demand.

Add a test-coverage workflow in Phase 1 and hold the line at, say, 40% before Phase 2. At 8% we currently have no regression safety net for any of the refactoring above — and 4b specifically exists so the GEE path can finally be tested in CI at all.

---

## Decision resolved — and its consequences

The gating question in the earlier draft ("is Earth Engine user-facing or an internal tool?") is **answered: user-facing, for anyone.** The non-specialist may be a country officer or an external researcher, and each is responsible for their own GEE account and Cloud project.

That is the more demanding of the two paths, and it commits us to three things the internal-tool reading would have let us skip:

1. **The GEE onboarding path must become the most robust part of setup, not the most fragile.** Today it is the reverse. Option 4a is the pivot: a non-specialist meets conda for the first time and must succeed, so the package has to install and configure the environment *for* them.
2. **`SUPPORT.md` has to draw the account/quota line explicitly.** We support the toolchain and the code; we cannot approve anyone's Earth Engine access or pay their Cloud/Drive quota. Saying so plainly, up front, is what keeps "Google won't approve me" tickets from reading as an elsar failure.
3. **The onboarding docs roughly double**, because they now have to carry a complete account → project → auth walkthrough aimed at someone who has never opened the Google Cloud console.

The one thing the decision does *not* change: everything in Tier A/B still stands on its own for users who never touch GEE, and the derived-layer complement above gives the GEE-averse a way to reach the same outputs. So "user-facing GEE" raises the ceiling of what we support without lowering the floor.

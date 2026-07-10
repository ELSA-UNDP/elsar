# Getting help with elsar

Thanks for using **elsar**. This page explains where to get help, what we can
and cannot support, and how to give us enough information to actually help you.

## Where to ask

| You want to… | Go to |
|---|---|
| Report a bug or unexpected result | [Open a bug report](https://github.com/ELSA-UNDP/elsar/issues/new?template=bug_report.yml) |
| Request a feature or change | [Open a feature request](https://github.com/ELSA-UNDP/elsar/issues/new?template=feature_request.yml) |
| Ask a "how do I…" question | [Search existing issues](https://github.com/ELSA-UNDP/elsar/issues?q=is%3Aissue) first, then open a bug report and pick the *Question* label |
| Read the documentation | [Package website](https://elsa-undp.github.io/elsar/) |

Please **search [existing issues](https://github.com/ELSA-UNDP/elsar/issues?q=is%3Aissue+is%3Aopen)** before opening a new one — your question may already be answered.

We prefer GitHub issues over email. A public issue is searchable, so the next
person with the same problem finds your answer, and anyone on the team can pick
it up rather than it waiting on one person's inbox.

## What we support

elsar has three layers with very different setup requirements. Knowing which
layer your problem is in helps us respond, and it sets realistic expectations
about what we can fix.

### Tier A — Core (fully supported)

Boundaries, planning units, raster normalisation, zone builders from
locally-supplied rasters, coverage metrics, and plotting.

- **Needs only** a working geospatial R stack (GDAL, GEOS, PROJ — installed with `sf` and `terra`).
- Runs offline, and works on the example data bundled in the package.
- **We commit to:** triaging every reproducible bug report and fixing genuine
  defects. If you can reproduce it on the bundled example data, we can fix it.

### Tier B — Protected areas (best-effort)

`make_protected_areas(from_wdpca = TRUE)`, which downloads protected-area data
from [Protected Planet](https://www.protectedplanet.net/) via the
[`wdpar`](https://prioritizr.github.io/wdpar/) package.

- **Also needs** a Chromium-based browser and the `libarchive` system library on
  your machine (these are requirements of `wdpar`, not of elsar directly).
- Depends on Protected Planet's availability and on the layout of their download
  pages, neither of which we control.
- **We commit to:** documenting the requirements and forwarding genuine upstream
  breakage to `wdpar`. **We cannot guarantee** availability. If this path is
  blocked, supply your own protected-area layer with `from_wdpca = FALSE`.

### Tier C — Earth Engine (supported, with a boundary)

The `download_*` functions, which pull global layers from
[Google Earth Engine](https://earthengine.google.com/) (GEE).

Earth Engine is a **first-class, user-facing** part of elsar. Anyone — a country
officer or an external researcher — can use it. **You bring and maintain your own
Google Earth Engine account and Cloud project.**

- **Also needs** a conda/Python environment, a GEE account with an approved Cloud
  project, and a Google Drive grant with free quota. See the
  [Earth Engine setup guide](https://elsa-undp.github.io/elsar/) and run
  `elsar_check_setup()` to see what is and isn't configured on your machine.
- **We support:** the elsar code, the setup tooling, the environment bootstrap,
  and clear step-by-step onboarding docs. If elsar itself misbehaves once your
  environment is set up, that's a bug — report it.
- **We cannot help with, because they belong to your own Google account:**
  - Earth Engine **access approval** (Google's process),
  - Cloud project **billing or quota**,
  - Google Drive **storage quota**.

  A problem that reduces to "Google hasn't approved my Earth Engine access" or
  "my Drive is full" is one we can point you at the right Google documentation
  for, but cannot resolve on your behalf.

## Before you open a bug report

A report we can act on almost always includes:

1. **A [reproducible example](https://reprex.tidyverse.org/) (reprex).** The
   smaller and more self-contained, the faster we can help. Where possible, use
   the bundled example data (`boundary_dat`, `get_wad_data()`) so we can run your
   code without your files or credentials.
2. **The full error message**, copied as text (not a screenshot).
3. **Your session info** — paste the output of `sessionInfo()` (or
   `sessioninfo::session_info()` if you have it). This tells us your OS, your R
   version, and your elsar / `sf` / `terra` / GDAL versions, which is often the
   whole answer.
4. **Which tier** (A/B/C above) the problem is in, and for Tier C, the output of
   `elsar_check_setup()`.

The bug-report template will prompt you for each of these.

## Response expectations

elsar is maintained by a small team alongside other work. We aim to acknowledge
issues within a couple of weeks, but we can't promise a fixed turnaround. Clear,
reproducible reports get resolved first — a good reprex genuinely moves your
issue to the front of the queue.

## Security

If you believe you've found a security-sensitive problem, please see
[SECURITY.md](SECURITY.md) rather than opening a public issue.

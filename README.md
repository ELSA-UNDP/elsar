# elsar <img src="man/figures/elsaR_hex_sticker.png" align="right" style="width:140px"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **elsar** package provides tools and data pipelines to support *Essential Life Support Areas (ELSA)* conservation planning. It is designed to assist in spatial prioritization and scenario development using globally standardized geospatial data layers.

## What does elsar do?

- **Prepare planning regions**: Create boundaries and planning units for any country
- **Process global datasets**: Load and harmonize spatial datasets (WDPA, KBA, LANDMark, ICCA, etc.)
- **Create zone layers**: Generate protect, restore, and manage zone rasters
- **Calculate metrics**: Compute spatial coverage over planning units
- **Visualize results**: Plot raster data with customizable styling

## Installation

```r
# Install the development version from GitHub
remotes::install_github("ELSA-UNDP/elsar")
```

## Quick Start

```r
library(elsar)

# Create a boundary for Nepal
boundary <- make_boundary(

  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  custom_projection = TRUE
)

# Create planning units
pus <- make_planning_units(
 boundary_proj = boundary,
  pu_threshold = 8.5e5,
  iso3 = "NPL"
)

# Load and normalize a feature layer
wad <- get_wad_data()
wad_norm <- make_normalised_raster(
  raster_in = wad,
  pus = pus,
  iso3 = "NPL"
)

# Plot the result
elsar_plot_static_raster(
  raster_in = wad_norm,
  legend_title = "WAD"
)
```

## Key Functions

| Category | Functions |
|----------|-----------|
| Planning Region | `make_boundary()`, `make_planning_units()` |
| Feature Data | `make_normalised_raster()`, `make_kbas()`, `make_protected_areas()`, `make_mangroves()`, `make_wetlands_ramsar()` |
| Zone Layers | `make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()` |
| Utilities | `get_coverage()`, `rescale_raster()`, `save_raster()` |
| Plotting | `elsar_plot_static_raster()`, `elsar_plot_feature()` |

## Documentation

- [Package website](https://elsa-undp.github.io/elsar/)
- [Getting started vignette](https://elsa-undp.github.io/elsar/articles/elsaR.html)

## License
CC BY 4.0

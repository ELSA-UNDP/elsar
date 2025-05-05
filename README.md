# elsaR R package  <img src="man/figures/elsaR_hex_sticker.png" align="right" style="width:140px"/>

The **elsaR** package provides tools and data pipelines to support *Essential Life Support Areas (ELSA)*. It is designed to assist in spatial prioritization and scenario development using globally standardized geospatial data layers for conservation and restoration planning.

---

## What does elsaR do?

- Loads and harmonizes global spatial datasets (e.g., WDPA, KBA, LANDMark, ICCA)
- Prepares binary and fractional raster layers for datastack creation
- Supports creation of key ELSA zone layers: protect, restore, manage
- Calculates spatial coverage metrics over planning units
- Provides tools to buffer and rasterise point-based spatial data
- Outputs results in ready-to-use formats (COG, SpatRaster, etc.)

---

## Installation

```r
# Install the development version from GitHub
remotes::install_github("ELSA-UNDP/elsar")
```

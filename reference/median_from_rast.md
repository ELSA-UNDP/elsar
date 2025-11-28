# Compute median from a SpatRaster using its GDAL PAM side-car histogram

Extracts the histogram stored in the GDAL PAM side-car XML file (e.g.
`raster.tif.aux.xml`) and computes the exact median value by finding the
bucket midpoint where the cumulative count reaches 50%.

## Usage

``` r
median_from_rast(r)
```

## Arguments

- r:

  A `SpatRaster` (from **terra**) whose underlying file has a GDAL PAM
  side-car XML (e.g. `raster.tif.aux.xml`).

## Value

A single numeric value: the median of the raster (bucket midpoint).

## Details

Before running this function, you must generate the histogram side‑car:
in a terminal, run:

    gdalinfo -hist /path/to/your.tif

This computes and stores the histogram in `/path/to/your.tif.aux.xml`.
Once the side-car exists, calling this function reads the stored
`<Histogram>` data without re-scanning pixel values and returns the
exact median bucket midpoint.

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)
r <- rast("eii_2023.tif")
# First, in the shell:
#   gdalinfo -hist eii_2023.tif
med <- median_from_rast(r)
} # }
```

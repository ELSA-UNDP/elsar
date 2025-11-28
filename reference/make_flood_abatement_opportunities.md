# Create a Flood Abatement Opportunities Raster

This function generates a raster that identifies flood abatement
opportunities by integrating the Global Flood Database (GFD) and NDVI
(Normalized Difference Vegetation Index). The function normalizes NDVI
and flood risk data, then calculates potential areas for flood abatement
based on their relationship.

## Usage

``` r
make_flood_abatement_opportunities(
  gfd_raster,
  ndvi_raster,
  pus,
  iso3,
  output_path = NULL,
  threads = TRUE
)
```

## Arguments

- gfd_raster:

  A `SpatRaster` representing flood risk data from the Global Flood
  Database.

- ndvi_raster:

  A `SpatRaster` representing NDVI data.

- pus:

  A `SpatRaster` defining the planning unit (PU) grid.

- iso3:

  A character string representing the ISO3 country code.

- output_path:

  A character string specifying the output directory for saving the
  raster (optional).

- threads:

  A logical value indicating whether to use multi-threaded processing
  where supported (default: `TRUE`).

## Value

A `SpatRaster` representing flood abatement opportunities, rescaled
between 0 and 1. If `output_path` is provided, the raster is saved as a
Cloud-Optimized GeoTIFF (COG).

## Examples

``` r
if (FALSE) { # \dontrun{
flood_abatement <- make_flood_abatement_opportunities(
  gfd_raster = terra::rast("gfd_flood_database.tif"),
  ndvi_raster = terra::rast("mod13q1_2022_ndvi_wgs84.tif"),
  pus = planning_units,
  iso3 = "NPL",
  output_path = "path/to/output"
)
} # }
```

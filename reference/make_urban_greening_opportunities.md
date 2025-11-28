# Create Urban Greening Opportunities Raster

This function generates an urban greening opportunities raster by
integrating NDVI (Normalized Difference Vegetation Index), land use/land
cover (LULC) classification or a pre-computed built areas raster, and
urban heat intensity from SDEI/GHS data. It identifies urban pixels with
high heat exposure and low greenness for targeted greening
interventions.

## Usage

``` r
make_urban_greening_opportunities(
  ndvi_raster,
  lulc_raster = NULL,
  built_areas_raster = NULL,
  sdei_statistics,
  threshold = 0.1,
  pus,
  iso3,
  return_urban_areas = FALSE,
  output_path = NULL,
  cores = 4
)
```

## Arguments

- ndvi_raster:

  A `SpatRaster` representing NDVI values.

- lulc_raster:

  Optional. A `SpatRaster` representing land use/land cover (LULC)
  classes. Required if `built_areas_raster` is not provided. Urban
  classes should be coded as `7`.

- built_areas_raster:

  Optional. A `SpatRaster` representing pre-classified binary built
  areas (1 = built, 0 = non-built). Skips internal classification from
  `lulc_raster`. Required if `lulc_raster` is not provided.

- sdei_statistics:

  An `sf` object with urban heat exposure data (e.g., WBGT statistics).

- threshold:

  Numeric. The fractional threshold (0–1) to classify planning units as
  urban after aggregation. Planning units with built area fraction above
  this threshold are considered urban. Default is `0.10`.

- pus:

  A `SpatRaster` defining the planning units.

- iso3:

  Character. ISO3 country code used to subset `sdei_statistics`.

- return_urban_areas:

  Logical. If `TRUE`, returns a two-layer stack with both greening
  opportunities and binary urban extent.

- output_path:

  Optional. Directory path to save the resulting raster(s).

- cores:

  Integer. Number of CPU cores to use (for future expansion, currently
  unused).

## Value

A normalized `SpatRaster` representing urban greening opportunities. If
`return_urban_areas = TRUE`, returns a raster stack with both the
greening opportunities raster and the binary built areas raster.

## Details

The built areas layer can be sourced either from a precomputed binary
raster (`built_areas_raster`) or derived dynamically from a LULC raster
(`lulc_raster`), which is classified on-the-fly to extract urban pixels.
When aggregating built areas to planning units, fractional values are
thresholded to generate a binary raster representing urban (1) and
non-urban (0) planning units.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- make_urban_greening_opportunities(
  ndvi_raster = ndvi,
  built_areas_raster = built_binary,
  sdei_statistics = sdei,
  pus = planning_units,
  iso3 = "BRA"
)
} # }
```

# Create Indigenous Managed Lands Raster (LANDMark + ICCA)

This function identifies Indigenous- and community-managed lands using
two datasets: the LANDMark dataset and the ICCA Registry. It buffers
point geometries if required, merges both sources, calculates fractional
coverage over planning units, applies a threshold, and returns a binary
raster indicating areas likely under Indigenous management.

## Usage

``` r
make_indigenous_managed_lands(
  sf_landmark = NULL,
  sf_icca = NULL,
  iso3,
  pus,
  buffer_points = TRUE,
  output_path = NULL
)
```

## Arguments

- sf_landmark:

  sf or NULL. LANDMark Indigenous lands dataset, subset to the country
  of interest.

- sf_icca:

  sf or NULL. ICCA Registry dataset, subset to the country of interest.

- iso3:

  Character. ISO3 country code for naming.

- pus:

  SpatRaster. Planning unit raster for alignment and coverage
  calculations.

- buffer_points:

  Logical. Buffer POINT/MULTIPOINT geometries into polygons (default =
  TRUE).

- output_path:

  Character or NULL. If provided, saves output raster as a COG to this
  directory.

## Value

A binary `SpatRaster` where:

- `1` = likely Indigenous-managed

- `0` = not Indigenous-managed

## Examples

``` r
if (FALSE) { # \dontrun{
indigenous_lands <- make_indigenous_managed_lands(
  sf_landmark = landmark_sf,
  sf_icca = icca_sf,
  iso3 = "COL",
  pus = planning_units,
  output_path = "outputs/"
)
terra::plot(indigenous_lands)
} # }
```

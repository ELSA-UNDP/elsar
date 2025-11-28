# Create a Standardised Raster of Key Biodiversity Areas (KBAs)

This function processes a Key Biodiversity Areas (KBA) vector dataset
and converts it into a normalised raster aligned with the input planning
units. It allows for filtering to include or exclude Alliance for Zero
Extinction (AZE) sites, as well as optionally excluding KBAs marked as
"Regional". By default, it includes all KBAs except regional-only sites.
It can also be used to return only AZE sites if `aze_only = TRUE`.

## Usage

``` r
make_kbas(
  kba_in,
  pus,
  iso3,
  include_aze_sites = FALSE,
  aze_only = FALSE,
  include_regional_kba = FALSE,
  buffer_points = TRUE,
  area_column = "repareakm2",
  nQuadSegs = 50,
  output_path = NULL
)
```

## Arguments

- kba_in:

  An `sf` object containing KBA vector features, including columns like
  `iso3`, `azestatus`, and `kbaclass`.

- pus:

  A `SpatRaster` object representing planning units (reference extent
  and resolution).

- iso3:

  A character string representing the 3-letter ISO country code (e.g.,
  "KEN").

- include_aze_sites:

  Logical. If `TRUE`, includes KBAs that are also AZE sites (default is
  `FALSE`).

- aze_only:

  Logical. If `TRUE`, returns only confirmed AZE sites (default is
  `FALSE`).

- include_regional_kba:

  Logical. If `FALSE`, filters out KBAs marked as "Regional" or "Global/
  Regional to be determined".

- buffer_points:

  Logical. If `TRUE`, circular buffers are generated around point
  geometries using the `area_column` attribute (default is `TRUE`).

- area_column:

  A string indicating the name of the column containing site area (in
  hectares), used when buffering point geometries.

- nQuadSegs:

  An integer specifying the number of segments used to approximate
  circular buffers (default: 50).

- output_path:

  Optional character. Directory path to save the output raster. If
  `NULL`, output is not written to file.

## Value

A `SpatRaster` object with normalised values representing KBA (or AZE)
coverage across planning units. If `output_path` is provided, the raster
is also written to disk.

## Examples

``` r
if (FALSE) { # \dontrun{
kba_raster <- make_kbas(
  kba_in = kba_sf,
  pus = planning_units,
  iso3 = "KEN",
  aze_only = TRUE,
  output_path = "outputs"
)
} # }
```

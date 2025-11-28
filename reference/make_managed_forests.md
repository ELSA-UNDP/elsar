# Create Managed Forests and Productive Managed Forests Raster Layers

This function generates a standardised raster of managed forests,
optionally including disturbed forests and a separate layer for
productive managed forests (based on NPP data). The input raster should
contain LULC classification values which are reclassified according to
defined managed forest class codes.

## Usage

``` r
make_managed_forests(
  raster_in,
  pus,
  iso3,
  forest_classes = c(31, 32, 40, 53),
  include_disturbed_forest = FALSE,
  make_productive = FALSE,
  raster_npp = NULL,
  name_out,
  output_path = NULL
)
```

## Arguments

- raster_in:

  A `SpatRaster` representing forest management classifications.

- pus:

  A `SpatRaster` used as a template for extent, resolution, and
  alignment.

- iso3:

  A 3-letter ISO country code string.

- forest_classes:

  Integer vector of class codes representing managed forests (default =
  c(31, 32, 40, 53)).

- include_disturbed_forest:

  Logical. If TRUE, includes disturbed forest class (code 20) in
  `forest_classes`.

- make_productive:

  Logical. If TRUE, also generates a productive managed forest raster.

- raster_npp:

  A `SpatRaster` of NPP data used for generating the productive forest
  layer. Required if `make_productive = TRUE`.

- name_out:

  Character. Name stem for output file(s) (optional, not currently
  used).

- output_path:

  Optional character. Directory to save output raster(s) if provided.

## Value

A `SpatRaster` with either one layer (managed forests) or two layers
(managed forests and productive managed forests).

## Details

If `make_productive = TRUE`, the function multiplies the managed forest
raster by a normalised raster of Net Primary Productivity (NPP) to
generate a layer representing productive managed forests.

## Examples

``` r
if (FALSE) { # \dontrun{
managed_forests <- make_managed_forests(
  raster_in = forest_lulc,
  pus = planning_units,
  iso3 = "NPL",
  make_productive = TRUE,
  raster_npp = npp_raster
)
} # }
```

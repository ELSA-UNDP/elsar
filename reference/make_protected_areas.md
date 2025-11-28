# Create a Binary or Fractional Raster of Protected Areas Aligned to Planning Units

Generates a raster of protected areas for a given country, aligned to
the planning units. Data can be downloaded from the [Protected
Planet](https://www.protectedplanet.net/) database via the wdpar
package, or provided as an `sf` object.

## Usage

``` r
make_protected_areas(
  pus,
  iso3,
  from_wdpa = TRUE,
  sf_in = NULL,
  download_path = NULL,
  status = c("Established", "Inscribed", "Designated"),
  pa_def = 1,
  include_mab_designation = FALSE,
  buffer_points = TRUE,
  area_column = "REP_AREA",
  area_calc_crs = "ESRI:54009",
  n_quad_segs = 50,
  binary = TRUE,
  threshold = 0.1,
  force_update = FALSE,
  output_path = NULL
)
```

## Arguments

- pus:

  SpatRaster. Planning units raster defining the output resolution and
  extent.

- iso3:

  Character. ISO3 country code (e.g., "CHL").

- from_wdpa:

  Logical. If `TRUE`, downloads protected area data using the wdpar
  package. If `FALSE`, uses `sf_in`. Default is `TRUE`.

- sf_in:

  sf object or NULL. Protected area geometries to use when

  `from_wdpa = FALSE`. Required if `from_wdpa = FALSE`.

- download_path:

  Character or NULL. Directory where WDPA data will be saved or loaded
  from. Required if `from_wdpa = TRUE`.

- status:

  Character vector. Which `STATUS` values to include from WDPA data.
  Valid values: "Designated", "Established", "Inscribed", "Proposed",
  "Adopted". Default is `c("Established", "Inscribed", "Designated")`.

- pa_def:

  Integer vector. Values for the `PA_DEF` column (1 = Protected Area, 0
  = OECM). Default is `1`.

- include_mab_designation:

  Logical. If `FALSE`, excludes UNESCO Man and

  Biosphere (MAB) reserves. Default is `FALSE`.

- buffer_points:

  Logical. If `TRUE`, creates geodesic buffers around point geometries
  using the area attribute. Default is `TRUE`.

- area_column:

  Character. Column name containing area values for buffering point
  geometries. Default is `"REP_AREA"`.

- area_calc_crs:

  Character. CRS to use for buffer calculations. Default is
  `"ESRI:54009"` (World Mollweide).

- n_quad_segs:

  Integer. Number of segments per quarter circle when creating buffers.
  Default is `50`.

- binary:

  Logical. If `TRUE`, output is binary (0/1) based on `threshold`. If
  `FALSE`, returns fractional coverage. Default is `TRUE`.

- threshold:

  Numeric. Coverage fraction threshold for binary classification.
  Planning units with coverage above this value are classified as
  protected. Only used when `binary = TRUE`. Default is `0.10`.

- force_update:

  Logical. If `TRUE`, forces re-download of WDPA data even if cached.
  Default is `FALSE`.

- output_path:

  Character or NULL. Directory to save the output raster. If NULL, the
  raster is returned but not saved.

## Value

If `sf_in` was provided (i.e., `from_wdpa = FALSE`), returns a
SpatRaster. If data was downloaded from WDPA (`from_wdpa = TRUE`),
returns a list with:

- raster:

  SpatRaster of protected areas

- sf:

  sf object of dissolved protected area polygons for reuse

## Details

All geometries are reprojected, dissolved to avoid double-counting
overlaps, and rasterized. The output can be binary (presence/absence
based on a coverage threshold) or fractional (proportion of each
planning unit covered).

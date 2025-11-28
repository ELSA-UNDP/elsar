# Create a Representation Gap Raster for Ecosystems

This function calculates the representation gap of ecosystems provided,
identifying those that fall short of a 30% protection target. The
resulting raster layer indicates the average degree of
underrepresentation across each planning unit. \#'

## Usage

``` r
make_underrepresented_ecosystems(
  ecosystems_sf,
  group_attribute,
  target_percent = 30,
  protected_areas_sf,
  pus,
  iso3 = NULL,
  output_path = NULL
)
```

## Arguments

- ecosystems_sf:

  `sf` object. Polygon features representing ecosystems, clipped to the
  analysis area.

- group_attribute:

  `character`. Column name in `ecosystems_sf` used to group features
  (e.g., "econame" or "get_id").

- target_percent:

  `numeric`. Target percentage of protection for each ecosystem group
  (default = 30).

- protected_areas_sf:

  `sf` or `SpatVector`. Protected areas dataset to be used in the gap
  calculation.

- pus:

  `SpatRaster`. Raster of planning units (e.g., from
  [`make_planning_units()`](https://elsa-undp.github.io/elsar/reference/make_planning_units.md)),
  used as the target grid for rasterization.

- iso3:

  `character`, optional. ISO3 country code used for naming output files
  (e.g., "BRA").

- output_path:

  `character`, optional. Directory path to save the output raster as a
  GeoTIFF.

## Value

A `SpatRaster` with one layer: `"underrepresented_ecosystems"`, showing
the maximum gap (0 to target_percent) per planning unit.

## Examples

``` r
if (FALSE) { # \dontrun{
gap_raster <- make_underrepresented_ecosystems(
  ecosystems_sf = my_ecosystem_polygons,
  group_attribute = "eco_type",
  target_percent = 30,
  protected_areas_sf = wdpa,
  pus = planning_units,
  iso3 = "KEN",
  output_path = "outputs"
)
} # }
```

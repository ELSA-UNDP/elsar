# Efficient Attribute-Weighted Rasterization Using Coverage Fraction

Rasterizes vector features to a raster grid defined by a planning unit
(`pus`) layer, assigning values based on a given attribute and the
actual coverage fraction of each feature over each raster cell. This
method is optimized for speed and precision and is especially useful
when features overlap or vary in size and shape.

## Usage

``` r
exact_rasterise(
  features,
  attribute,
  iso3,
  pus,
  invert = FALSE,
  rescaled = TRUE,
  fun = mean,
  cores = 4
)
```

## Arguments

- features:

  An `sf` or `SpatVector` object containing the vector features to
  rasterize.

- attribute:

  Character. The column name in `features` to use as a weight for raster
  values.

- iso3:

  Character. ISO3 country code, passed to
  [`make_normalised_raster()`](https://elsa-undp.github.io/elsar/reference/make_normalised_raster.md).

- pus:

  A `SpatRaster` object defining the resolution, extent, and CRS of the
  output raster.

- invert:

  Logical. If `TRUE`, inverts the resulting values during normalization
  (default: `FALSE`).

- rescaled:

  Logical. If `TRUE`, rescales the output to 0-1 using
  [`make_normalised_raster()`](https://elsa-undp.github.io/elsar/reference/make_normalised_raster.md)
  (default: `TRUE`).

- fun:

  Function. Aggregation function applied across overlapping rasterized
  features (default: `mean`).

- cores:

  Integer. Number of CPU cores to use for multi-core processing
  (default: 4).

## Value

A `SpatRaster` object representing the attribute-weighted rasterization
of the input features.

## Details

If multiple features are provided, each is rasterized independently and
aggregated across the stack using the specified function (`fun`),
typically `sum`, `mean`, or `max`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Rasterize polygons using a 'score' attribute
result <- exact_rasterise(features = my_polygons, attribute = "score", pus = my_raster, fun = sum)
} # }
```

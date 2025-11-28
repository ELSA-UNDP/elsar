# Create a Standardised Raster for Wetlands and Ramsar Sites

This function processes and combines Ramsar and global wetlands data to
produce a standardised raster layer aligned to planning units. It
supports handling of Ramsar point geometries by buffering them into
polygons if required, and allows filtering and normalisation based on
user-defined thresholds. Either Ramsar or Wetlands data can be provided,
or both.

## Usage

``` r
make_wetlands_ramsar(
  ramsar_in = NULL,
  wetlands_in = NULL,
  pus,
  iso3,
  buffer_points = TRUE,
  area_column = "Area (ha)",
  nQuadSegs = 50,
  wetland_threshold = 0.25,
  return_all = FALSE
)
```

## Arguments

- ramsar_in:

  An `sf` object containing Ramsar site geometries (optional).

- wetlands_in:

  A `SpatRaster` object representing global wetlands data (optional).

- pus:

  A `SpatRaster` object containing the planning units used for
  resolution and extent.

- iso3:

  A character string with the ISO3 country code (e.g., "KEN").

- buffer_points:

  Logical. Only relevant when "POINT" or "MULTIPOINT" geometries exist
  in `ramsar_in`. If `TRUE`, circular buffers are generated from the
  area attribute.

- area_column:

  A string indicating the name of the column containing site area (in
  hectares). Used when buffering point geometries.

- nQuadSegs:

  An integer specifying the number of segments used to approximate
  circular buffer (default: 50).

- wetland_threshold:

  Numeric. Threshold for binarising wetlands raster (default: 0.25).

- return_all:

  Logical. If `TRUE`, returns a raster stack containing the combined
  wetlands+ramsar raster, as well as the individual normalised input
  layers.

## Value

A `SpatRaster` object. If `return_all = TRUE`, returns a raster stack
with three layers: combined, ramsar only, and wetlands only.

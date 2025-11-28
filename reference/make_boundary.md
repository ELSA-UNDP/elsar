# Create a boundary of the planning region

Create a boundary of the planning region

## Usage

``` r
make_boundary(
  boundary_in,
  input_type = "sf",
  limit_to_mainland = FALSE,
  col_name = NULL,
  filter_out = 0,
  custom_projection = TRUE,
  iso3 = NULL,
  iso3_column = NULL,
  output_path = NULL
)
```

## Arguments

- boundary_in:

  A file containing the boundary information. Can be `sf` or
  `SpatRaster`

- input_type:

  A string that is either "sf" or "SpatRaster" (default is "sf").

- limit_to_mainland:

  Logical. Limits the extent of the data to mainland.

- col_name:

  A string of the column containing the actual extent of the planning
  region (not outside area). Can be `NULL`.

- filter_out:

  A value representing the outside area in the data (e.g. `0`)

- custom_projection:

  Logical. `TRUE`if custom projection for planning region is wanted.

- iso3:

  The iso3 country code (character) of the country of interest.

- iso3_column:

  Only relevant when `iso3` != NULL. A string of the name of where iso3
  information can be found in a dataset.

- output_path:

  An optional output path for the created file. Only needed when
  custom_projection = TRUE.

## Value

`sf` object of the boundary of the planning region

## Examples

``` r
boundary <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd"
)
```

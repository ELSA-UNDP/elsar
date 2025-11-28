# Create a custom projection based on the planning region

Create a custom projection based on the planning region

## Usage

``` r
make_custom_projection(
  boundary,
  output_path = NULL,
  iso3_column = "iso3cd",
  iso3 = NULL
)
```

## Arguments

- boundary:

  `sf` object of the boundary of the planning region. Should match iso3
  country code.

- output_path:

  An optional output path for the created file.

- iso3_column:

  A string of the name of where iso3 information can be found in a
  dataset.

- iso3:

  The iso3 country code (character) of the country of interest.

## Value

A `wkt` file centred on the planning region

## Examples

``` r
boundary <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd"
)

wkt <- make_custom_projection(boundary = boundary, iso3 = "NPL")
```

# Create a Map of the ELSA output to download with GIS layers

This function gives a quick way of getting a map immediately when
downloading the output GIS layers.

## Usage

``` r
elsa_plot_output_map(elsa_raster, ELSA_text, pu0)
```

## Arguments

- elsa_raster:

  A `SpatRaster`with one layer that contains different categories for
  the different zones.

- ELSA_text:

  A `tbl_df` containing the translations for the displayed text in the
  plot.

- pu0:

  A `SpatRaster` of all possible planning units in the planning region
  (one layer)

## Value

A ggplot object with a map of the prioritization output

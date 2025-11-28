# Calculate feature representation

This function calculates the feature representation of a single feature
per zone comapred to all possible PUs for this feature across all zones.
E.g. if 60 out of 100 possible PUs are selected for zone 1 and 50 out of
100 possible PUs are selected for zone 2: This DOES not mean that the
overall feature representation in the planning region is 60% (60/100) +
50% (50/100) (= 110%, so \> 100%) Instead we need to get the
representation in the zone versus all possible planning units in the
whole region. This means that if in our case zones do not share any
overlapping possible area prior to prioritisation, we would have 200
possible planning units overall for the feature in the planning region.
The representation across the whole planning region would then be:
60/200 + 50/200, so 110/200 (= 55%)

## Usage

``` r
calc_feat_rep(prob.all, elsa_result, elsa_raster, feat_df, ELSA_text)
```

## Arguments

- prob.all:

  A `prioritizr` problem. Needed as an input for the `prioritizr`
  function `eval_feature_representation_summary()`

- elsa_result:

  A `prioritizr` solution. Needed as an input for the `prioritizr`
  function `eval_feature_representation_summary()`

- elsa_raster:

  A `SpatRaster` containing label information of the the zones used to
  obtain solution. Has no zero values.

- feat_df:

  A `df` containing information on the features, including columns
  "label" and "theme"

- ELSA_text:

  A `tbl_df` containing the translations for the displayed text in the
  plot. Needs

## Value

A `list` with two `tbl_df`, one with the raw data and the second with
the data prepared for plotting.

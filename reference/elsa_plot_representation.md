# Create a Bar Plot of Feature Representation

This function generates a stacked bar plot displaying the representation
of each input feature, broken up into specified actions (e.g.,
"Protect," "Restore," "Manage," and optionally "Urban Greening").

## Usage

``` r
elsa_plot_representation(
  feature_rep_tabl,
  input,
  ELSA_text,
  pal.elsa = tibble::tibble(colour = c("#4daf4a", "#984ea3", "#377eb8", "#adaf4a",
    "#FFFFFF00"), category = c("Protect", "Restore", "Manage", "PR", "Do_Nothing"))
)
```

## Arguments

- feature_rep_tabl:

  A dataframe containing representation values for each feature. Columns
  should include numeric data for each feature and action.

- input:

  The input object of an R Shiny app containing budget information for
  each action. This is used to label the budget percentage of each
  action in the plot legend.

- ELSA_text:

  A `tbl_df` containing the translations for the displayed text in the
  plot.

- pal.elsa:

  A `tibble` with palette information for plot

## Value

A ggplot object with a stacked bar chart showing feature representation
percentages.

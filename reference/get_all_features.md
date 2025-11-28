# Wrapper function to get a raster file with all wanted features

Wrapper function to get a raster file with all wanted features

## Usage

``` r
get_all_features(feature_list, path_in, pus, iso3, output_path = NULL, ...)
```

## Arguments

- feature_list:

  A `list` with all features that are wanted for the downstream ELSA
  workflow.

- path_in:

  A common path where all the data can be found.

- pus:

  A `SpatRaster` file that contains the reference spatial extent, crs
  etc.in form of the planning units

- iso3:

  A string of the iso3 name of the data (country name)

- output_path:

  An optional output path for all created files.

- ...:

  Additional attributes needed for some functions if wanted.

## Value

A `SpatRaster` with the length of the feature_list + 1 since the
planning units are the first layer.

## Examples

``` r
if (FALSE) { # \dontrun{
feature_list <- c("fml", "cropsuit", "fi")

all_feats <- get_all_features(
  feature_list = feature_list,
  path_in = "<yourpath>",
  pus = pus,
  iso3 = "NPL"
  )
} # }
```

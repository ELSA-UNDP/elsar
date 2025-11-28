# Check and download GEE-sourced layers based on metadata requirements

This function checks for required layer names from metadata, ensures
they exist in the output path, and optionally prompts the user to
download them from GEE if missing. Used internally by other functions to
automatically download required datasets.

## Usage

``` r
check_and_download_required_layers(
  data_info,
  iso3,
  input_path,
  gee_project,
  boundary_proj,
  wait_time = 5,
  interactive = TRUE
)
```

## Arguments

- data_info:

  Data.frame containing `data_name` fields to match against
  requirements.

- iso3:

  Character. ISO3 country code for the target country.

- input_path:

  Character. Path where data files should exist or be downloaded to.

- gee_project:

  Character. Google Earth Engine project ID.

- boundary_proj:

  An `sf` object representing the country boundary.

- wait_time:

  Numeric. Wait time (in minutes) for Drive export. Default is 5.

- interactive:

  Logical. If TRUE (default), prompts user before downloading. If FALSE,
  automatically downloads missing layers without prompting. Set to FALSE
  for non-interactive sessions (e.g., batch jobs, CI/CD pipelines).

## Value

Invisible NULL. Files are downloaded as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive mode (default) - prompts user
check_and_download_required_layers(
  data_info = metadata_df,
  iso3 = "GHA",
  input_path = "/path/to/data",
  gee_project = "my-project",
  boundary_proj = ghana_boundary
)

# Non-interactive mode - auto-downloads without prompts
check_and_download_required_layers(
  data_info = metadata_df,
  iso3 = "GHA",
  input_path = "/path/to/data",
  gee_project = "my-project",
  boundary_proj = ghana_boundary,
  interactive = FALSE
)
} # }
```

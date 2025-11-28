# Initialize Google Earth Engine

Sets up Python environment and initializes the Earth Engine API. Handles
authentication and temporary conda environments as needed.

## Usage

``` r
initialize_earthengine(gee_project)
```

## Arguments

- gee_project:

  Character. Google Earth Engine cloud project ID. This is required and
  must be a valid GEE project you have access to. You can find your
  project ID in the [GEE Code
  Editor](https://code.earthengine.google.com/) or [Google Cloud
  Console](https://console.cloud.google.com/).

## Value

A list containing the `ee` module and temporary environment name (if
created)

## Examples

``` r
if (FALSE) { # \dontrun{
env_info <- initialize_earthengine("my-gee-project")
ee <- env_info$ee
} # }
```

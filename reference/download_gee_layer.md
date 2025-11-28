# Download and Process a GEE Raster Layer into a Cloud-Optimized GeoTIFF

Generic function for downloading any raster data from Google Earth
Engine. Handles authentication, export management, file downloading, and
processing into a local Cloud-Optimized GeoTIFF. Exports to Google Drive
root to avoid GEE folder duplication bugs.

## Usage

``` r
download_gee_layer(
  boundary_layer,
  iso3,
  gee_project,
  asset_id,
  file_prefix,
  output_dir = here::here(),
  scale = 10,
  datatype = "INT1U",
  googledrive_folder = NULL,
  wait_time = 5
)
```

## Arguments

- boundary_layer:

  An `sf` object defining the spatial boundary of interest.

- iso3:

  Character. Three-letter ISO country code for filename generation.

- gee_project:

  Character. Google Earth Engine cloud project ID. This is required and
  must be a valid GEE project you have access to. Find your project ID
  in the [GEE Code Editor](https://code.earthengine.google.com/).

- asset_id:

  Character. Earth Engine ImageCollection asset ID (e.g.,
  "projects/sat-io/open-datasets/...").

- file_prefix:

  Character. Prefix for export filename and GEE task description.

- output_dir:

  Character. Path to directory for saving the final raster file.
  Defaults to project root via
  [`here::here()`](https://here.r-lib.org/reference/here.html).

- scale:

  Numeric. Resolution of the exported image in meters. Default is 10.

- datatype:

  Character. Output datatype (GDAL style), e.g., "INT1U" or "FLT4S".
  Default is "INT1U".

- googledrive_folder:

  Character or NULL. Google Drive folder name for exports. Currently
  defaults to NULL (Drive root) to avoid a GEE folder duplication bug.

- wait_time:

  Numeric. Maximum time in minutes to wait for the GEE export to appear
  in Google Drive. Default is 5. Increase for large exports.

## Value

A `SpatRaster` object written to disk, or NULL if export timed out.

## Details

For most use cases, prefer the higher-level wrapper functions like
[`download_esri_lulc_data()`](https://elsa-undp.github.io/elsar/reference/download_esri_lulc_data.md)
or
[`download_global_pasture_data()`](https://elsa-undp.github.io/elsar/reference/download_global_pasture_data.md)
which have sensible defaults for common datasets.

## See also

[`download_esri_lulc_data()`](https://elsa-undp.github.io/elsar/reference/download_esri_lulc_data.md),
[`download_global_pasture_data()`](https://elsa-undp.github.io/elsar/reference/download_global_pasture_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Download ESRI LULC data for Ghana
lulc <- download_gee_layer(
  boundary_layer = ghana_boundary,
  iso3 = "GHA",
  gee_project = "my-gee-project",
  asset_id = "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS",
  file_prefix = "esri_10m_lulc",
  scale = 10,
  datatype = "INT1U"
)
} # }
```

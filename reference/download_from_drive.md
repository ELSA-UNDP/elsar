# Download files from Google Drive

Downloads all files matching a specified prefix from a Google Drive
folder. Files are filtered to only include .tif files matching the
prefix pattern.

## Usage

``` r
download_from_drive(drive_folder, file_prefix, local_path)
```

## Arguments

- drive_folder:

  Character or NULL. Google Drive folder name/path, or NULL for root.

- file_prefix:

  Character. Prefix to search for in filenames.

- local_path:

  Character. Local directory path where files should be downloaded.

## Value

Invisible list with `success` (logical) and `files` (character vector of
successfully downloaded file paths). Returns `success = FALSE` if any
download fails.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- download_from_drive("my_gee_exports", "esri_lulc_2024_GHA", "/tmp/downloads")
if (!result$success) warning("Some downloads failed")
} # }
```

# Extract Filename and Filetype from Directory

Extracts the base filename and file extension for a given `data_name` in
a specified folder.

## Usage

``` r
extract_filename_filetype(data_name, file_path)
```

## Arguments

- data_name:

  Character. Name pattern of the file to search.

- file_path:

  Character. Path to the folder.

## Value

A named list with elements: `filename` and `filetype`.

# Extract and Filter IUCN GET Ecosystem Vector Layers

This function reads, intersects, reprojects, and merges `.gpkg` vector
files representing IUCN Global Ecosystem Typology (GET) layers from a
specified directory. It allows filtering by filename prefixes (e.g.,
"T1.2", "TF1.4"), excludes specific ecosystem types (e.g., intensive
land-use biomes), and optionally removes features with
`minor occurrence` flags. The resulting layer is cropped to a country
boundary and reprojected to match a planning units raster.

## Usage

``` r
get_iucn_ecosystems(
  iucn_get_directory,
  iso3,
  pus,
  boundary_layer,
  include_minor_occurrence = TRUE,
  iucn_get_prefixes = NULL,
  excluded_prefixes = c("T7.1", "T7.2", "T7.3", "T7.4", "T7.5"),
  output_path = NULL
)
```

## Arguments

- iucn_get_directory:

  Character. Path to the directory containing IUCN GET `.gpkg` files.

- iso3:

  Character. ISO3 country code (used for logging and naming the output
  file).

- pus:

  A `SpatRaster` object. Used for defining spatial extent and target
  CRS.

- boundary_layer:

  An `sf` polygon layer (typically a country boundary) used to spatially
  crop features.

- include_minor_occurrence:

  Logical. If `FALSE`, filters out ecosystems marked as minor occurrence
  (default is `TRUE`).

- iucn_get_prefixes:

  Optional character vector of GET ecosystem IDs to include (e.g.,
  `c("F1.1", "TF1.2")`). If `NULL`, all layers are included.

- excluded_prefixes:

  Optional character vector of ecosystem prefixes to exclude (e.g.,
  `c("T7.1", "T7.2")` for intensive land-use biomes). Default exclude
  all T7 classes.

- output_path:

  Character or `NULL`. If specified, writes the resulting `sf` object as
  a GeoPackage to this directory.

## Value

An `sf` object of the merged, reprojected, valid IUCN GET ecosystem
features intersecting the target boundary.

## Details

The ecosystem layer ID (e.g., "F1.1") is extracted from the source
filename and added to each feature as the `get_id` column for tracking.

If `output_path` is specified, the final filtered and valid `sf` object
is saved as a GeoPackage.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define inputs
pus <- terra::rast("data/pus_raster.tif")
boundary <- sf::st_read("data/country_boundary.gpkg")

# Get all layers except intensive land-use (T7) and minor occurrences
ecosystems <- get_iucn_ecosystems(
  iucn_get_directory = "data/iucn_layers",
  iso3 = "KEN",
  boundary_layer = boundary,
  pus = pus,
  include_minor_occurrence = FALSE,
  excluded_prefixes = c("T7.1", "T7.2", "T7.3", "T7.4", "T7.5"),
  output_path = "outputs"
)
} # }
```

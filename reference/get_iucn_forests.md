# Extract and Rasterise IUCN Forest Ecosystems from Preloaded Layer

This function filters IUCN GET polygons (already loaded as `sf`) to
include only forest ecosystems, merges them, and calculates proportional
coverage across each planning unit. The result is normalized to a 0–1
scale using
[`elsar::make_normalised_raster()`](https://elsa-undp.github.io/elsar/reference/make_normalised_raster.md).

## Usage

``` r
get_iucn_forests(
  iucn_get_sf,
  iso3,
  pus,
  boundary_layer,
  include_minor_occurrence = TRUE,
  iucn_get_prefixes = c("MFT1.2", "T1.1", "T1.2", "T1.3", "T1.4", "T2.1", "T2.2", "T2.3",
    "T2.4", "T2.5", "T2.6", "TF1.1", "TF1.2"),
  excluded_prefixes = NULL,
  output_path = NULL
)
```

## Arguments

- iucn_get_sf:

  sf object. IUCN GET polygons already loaded and pre-clipped to the
  boundary.

- iso3:

  Character. ISO3 country code, used for naming and passed to
  [`make_normalised_raster()`](https://elsa-undp.github.io/elsar/reference/make_normalised_raster.md).

- pus:

  SpatRaster. Planning units raster over which forest coverage is
  computed.

- boundary_layer:

  sf object. Used to spatially clip features if necessary.

- include_minor_occurrence:

  Logical. If FALSE, removes minor occurrence ecosystems.

- iucn_get_prefixes:

  Character vector of layer prefixes to retain (default is
  forest-related).

- excluded_prefixes:

  Optional character vector of prefixes to exclude (e.g., `"T7.1"`).

- output_path:

  Optional character. Output directory to save raster.

## Value

A normalized `SpatRaster` showing fractional IUCN forest coverage across
PUs.

## Details

Optionally, the result can be written to a Cloud-Optimized GeoTIFF file.

## Examples

``` r
if (FALSE) { # \dontrun{
iucn_data <- get_iucn_ecosystems(...)
forest_layer <- get_iucn_forests_from_layer(
  iucn_get_sf = iucn_data,
  pus = pus,
  iso3 = "KEN",
  boundary_layer = boundary,
  excluded_prefixes = c("T7.1", "T7.2"),
  output_path = "outputs"
)
} # }
```

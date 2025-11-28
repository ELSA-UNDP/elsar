# Create the forest integrity index using flii and fsii data

Create the forest integrity index using flii and fsii data

## Usage

``` r
make_forest_integrity(
  raster_flii = NULL,
  raster_fsii = NULL,
  pus,
  iso3,
  output_path = NULL
)
```

## Arguments

- raster_flii:

  A `SpatRaster` file with the flii information.

- raster_fsii:

  A `SpatRaster` file with the fsii information.

- pus:

  A `SpatRaster` file that contains the reference spatial extent, crs
  etc.in form of the planning units

- iso3:

  A string of the iso3 name of the data (country name)

- output_path:

  An optional output path for the created file.

## Value

A `SpatRaster` file of the forest integrity index that has been aligned
and normalised

## Examples

``` r
if (FALSE) { # \dontrun{
forest_integrity1 <- make_forest_integrity(
  raster_flii = raster_flii,
  pus = pus
)

forest_integrity2 <- make_forest_integrity(
  raster_fsii = raster_fsii,
  pus = pus
)

forest_integrity3 <- make_forest_integrity(
  raster_flii = raster_flii,
  raster_fsii = raster_fsii,
  pus = pus
)

zero_fsii <- raster_fsii
zero_fsii[zero_fsii < 14] = 0

forest_integrity4 <- make_forest_integrity(
  raster_flii = raster_flii,
  raster_fsii = raster_fsii,
  pus = pus
)

} # }
```

# Load spatial or raster data from local files or Postgres

Automatically detects file type and loads spatial (`sf`) or raster
(`SpatRaster`) data. Supports local files (e.g., shapefiles,
GeoPackages, TIFFs) and PostGIS tables.

## Usage

``` r
elsar_load_data(
  file_name = NULL,
  file_path = NULL,
  file_lyr = NULL,
  wkt_filter = NULL,
  db_info = NULL,
  pg_connection = NULL,
  drop3d = TRUE,
  iso3_column = NULL,
  iso3 = NULL
)
```

## Arguments

- file_name:

  Character or NULL. File name with extension. Use NULL to load all
  files of a type in a folder. Set to `"postgres"` to load from a
  PostGIS database.

- file_path:

  Character. Folder or file path. Not required when loading from
  PostgreSQL.

- file_lyr:

  Character. Optional. Layer name for multi-layer files (e.g.,
  GeoPackage or GDB). When loading from PostgreSQL, this specifies the
  table name.

- wkt_filter:

  `sf`, `SpatRaster`, or `SpatVector`, from which WKT geometry can be
  derived to spatially filter vector data when reading in.

- db_info:

  Named list with PostgreSQL connection parameters. Required elements:

  host

  :   Database host (e.g., "localhost" or an IP address)

  dbname

  :   Name of the PostgreSQL database

  port

  :   Port number (typically 5432)

  user

  :   Database username

  password

  :   Database password

- pg_connection:

  Named list. Alternative parameter name for `db_info` (same structure).

- drop3d:

  Logical. Drop Z/M dimensions if TRUE. Default is TRUE.

- iso3_column:

  Character. Column name to filter by ISO3 code.

- iso3:

  Character. ISO3 code to filter vector data.

## Value

An sf object or SpatRaster depending on the file type.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load a single raster
r <- elsar_load_data(
  file_name = "lulc.tif",
  file_path = "data"
  )

# Load shapefile with ISO3 filter
shp <- elsar_load_data(
  file_name = "admin.shp",
  file_path = "data",
  iso3 = "KEN",
  iso3_column = "ISO3"
  )

# Load all shapefiles in a folder
shp_all <- elsar_load_data(
  file_name = NULL,
  file_path = "data/shapes"
  )

# Load PostGIS table
pg <- elsar_load_data(
  file_name = "postgres",
  file_lyr = "admin",
  iso3 = "KEN",
  iso3_column = "iso3",
  pg_connection = list(
      host = "localhost",
      dbname = "gis",
      port = 5432,
      user = "me",
      password = "pass"
      )
   )
} # }
```

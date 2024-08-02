#' Function to create planning units and custom projection
#'
#' This function creates planning units for a spatial prioritisation problem in a raster format. It also creates a custom projection (`wkt` file) centred on the planning region based on the Mollweide projection.
#'
#' @param boundary_src A string of the data name and file type
#' @param boundary_lyr The layer used within the data (character or numeric).
#' @param dataPath A string of the path where the data is saved. This is also where the custom projection is saved.
#' @param input_type A string that is either "sf", "postgres" or "raster" (default is "sf").
#' @param pu_size A way to define a custom planning unit size. Can be NULL to use default settings that generate planning units as small as possible whilst still being computationally efficient.
#' @param pu_threshold An integer value that gives a maximum number of PUs. The default (`8.5e5`) is set based on `prioritizr` processing time, network transfer time and solver time.
#' @param limit_to_mainland A logical that determines whether planning units should only be created for mainland area (`FALSE`; default) or not (`TRUE`)
#' @param iso3_column Only relevant when `ìnput_type` "postgres" is selected. A string of the name of where iso3 information can be found in a dataset.
#' @param iso3 The iso3 country code (character) of the country of interest.
#' @param outputPath An optional output path for the created file. If none is provided, `dataPath` will be used.
#'
#' @return A raster (`.tif`) file with the planning unit information for the chosen planning region.
#' @export
#'
#' @examples
#' \dontrun{
#' pus_nepal <- make_planning_units(
#'   boundary_src = "pu_nepal_450m.tif",
#'   boundary_lyr = NULL,
#'   dataPath = "localDataPath/nepal_reference",
#'   input_type = "raster",
#'   iso3 = "NPL"
#' )
#' }
make_planning_units <- function(boundary_src, # string of data name and file type
                                boundary_lyr = NULL, # layer within file
                                dataPath,
                                input_type = "sf", # sf, postgres, raster
                                pu_size = NULL,
                                pu_threshold = 8.5e5,
                                limit_to_mainland = FALSE,
                                iso3_column = "iso_sov1",
                                iso3,
                                outputPath = NULL) {
  # create path to data (same pathe will be needed later to save projection)
  boundary_src <- file.path(dataPath, boundary_src)

  if (input_type == "postgres") { # can't check this
    # PostgreSQL Connection only, if needed
    dn <- dbConnect("PostgreSQL", dbname = boundary_src)
    nb <- st_read(dsn = dn, query = glue::glue("SELECT * FROM {boudnary_lyr} WHERE type = 'Land' AND {iso3_column} = '{iso3}'"))
  } else if (input_type == "sf") {
    nb <- sf::read_sf(boundary_src, boundary_lyr)
  } else if (input_type == "raster") {
    nb <- terra::rast(boundary_src, lyrs = boundary_lyr) %>%
      terra::as.polygons() %>%
      sf::st_as_sf()
  }

  if (limit_to_mainland == TRUE) { # exclude any islands etc.
    nb <- nb %>%
      sf::st_cast("POLYGON") %>%
      dplyr::slice(which.max(as.numeric(sf::st_area(.)))) # get largest polygon that represents mainland
  }

  nb <- sf::st_transform(nb, crs = st_crs(4326))

  xmid <- mean(c(sf::st_bbox(nb)$xmin, sf::st_bbox(nb)$xmax))
  ymid <- mean(c(sf::st_bbox(nb)$ymin, sf::st_bbox(nb)$ymax))

  # Create WKT projection string
  wkt <- glue::glue('PROJCS["Mollweide_Custom_{iso3}",GEOGCS["GCS_unknown",
                  DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,
                  AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],
                  PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],
                  PROJECTION["Mollweide"],PARAMETER["central_meridian",{xmid}],
                  PARAMETER["false_easting",0],PARAMETER["false_northing",
                  {ymid}],UNIT["metre",1,AUTHORITY["EPSG","9001"]],
                  AXIS["Easting",EAST],AXIS["Northing",NORTH]]')
  # note: for marine it's not this straight forward if EEZ goes across date line
  writeLines(wkt, glue::glue("{dataPath}/{tolower(iso3)}_proj.wkt")) # save wkt

  # reproject to new projection
  nb_proj <- sf::st_transform(nb, crs = sf::st_crs(wkt))

  if (is.null(pu_size)) { # NO provided PU size
    pu_sum <- pu_threshold # if more than this: issues solving problems in real-ish time (related to: prioritizr processing time, network transfer time, gurobi solve time)

    pu_size <- units::drop_units(sf::st_area(nb_proj)) / 1e6 / 8e5 * 1e3 # convert to edge length of PU

    pu_size <- dplyr::case_when( # round PU numbers based on size
      pu_size < 100 ~ 100,
      between(pu_size, 100, 300) ~ plyr::round_any(pu_size, accuracy = 50, floor),
      pu_size > 300 ~ plyr::round_any(pu_size, accuracy = 100, floor)
    )

    while (pu_sum >= pu_threshold) { # ensure that the final # of PUs is < 850,000
      pu_size <- pu_size + dplyr::case_when( # for this, make the PUs larger (so decrease the number) within the planning region
        between(pu_size, 100, 300) ~ 50,
        pu_size > 300 ~ 100
      )

      rasterMask <- terra::rast( # create a raster with the current PU size, the new crs and the spatial extent of the data
        resolution = pu_size,
        crs = wkt,
        extent = terra::ext(nb_proj)
      )

      r1 <- terra::rasterize(terra::vect(nb_proj), # rasterize the data (sf object) based on the created raster
        rasterMask,
        touches = TRUE,
        update = TRUE,
        background = 0
      )

      pu_sum <- terra::global(r1, sum, na.rm = TRUE) # calculate the # of PUs to check if we're below the given threshold yet
      print(paste0("The current number of planning units is: ", as.integer(pu_sum)))
    }
  } else {
    rasterMask <- terra::rast( # create a raster with the current PU size, the new crs and the spatial extent of the data
      resolution = pu_size,
      crs = wkt,
      extent = terra::ext(nb_proj)
    )

    r1 <- terra::rasterize(terra::vect(nb_proj), # rasterize the data (sf object) based on the created raster
      rasterMask,
      touches = TRUE,
      update = TRUE,
      background = 0
    )
    pu_sum <- terra::global(r1, sum, na.rm = TRUE)

    if (pu_sum > pu_threshold) {
      message(paste0("Your current number of planning units (", pu_sum, ") is above our recommended threshold of ", pu_threshold, ". Consider increasing the input pu_size."))
    }
  }

  # save PU layer
  if (is.null(outputPath)) {
    outputPath <- dataPath
  }

  terra::writeRaster(r1,
    glue::glue("{outputPath}/planning_units_{iso3}.tif"),
    gdal = c("COMPRESS=DEFLATE", "OVERVIEWS=NONE"),
    NAflag = 255,
    overwrite = TRUE,
    filetype = "COG",
    datatype = "INT1U"
  )

  base_v <<- nb_proj
}

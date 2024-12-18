#' Function to create planning units and custom projection
#'
#' This function creates planning units for a spatial prioritisation problem in a raster format. It also creates a custom projection (`wkt` file) centred on the planning region based on the Mollweide projection.
#'
#' @param boundary_proj A `sf`object representing the boundary of the planning region in the preferred projection. Preferably generated with [make_boundary()]
#' @param pu_size A way to define a custom planning unit size. Can be NULL to use default settings that generate planning units as small as possible whilst still being computationally efficient.
#' @param pu_threshold An integer value that gives a maximum number of PUs. The default (`8.5e5`) is set based on `prioritizr` processing time, network transfer time and solver time.
#' @param limit_to_mainland A logical that determines whether planning units should only be created for mainland area (`FALSE`; default) or not (`TRUE`)
#' @param iso3 The iso3 country code (character) of the country of interest.
#' @param output_path An optional output path for the created file.
#'
#' @return A raster (`.tif`) file with the planning unit information for the chosen planning region.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(boundary_proj = boundary_proj,
#'                            pu_size = NULL,
#'                            pu_threshold = 8.5e5,
#'                            limit_to_mainland = FALSE)
make_planning_units <- function(boundary_proj,
                                pu_size = NULL,
                                pu_threshold = 8.5e5,
                                limit_to_mainland = FALSE,
                                iso3,
                                output_path = NULL) {
  if (is.null(pu_size)) { # NO provided PU size
    pu_sum <- pu_threshold # if more than this: issues solving problems in real-ish time (related to: prioritizr processing time, network transfer time, gurobi solve time)

    pu_size <- units::drop_units(sf::st_area(boundary_proj)) / 1e6 / 8e5 * 1e3 # convert to edge length of PU

    pu_size <- dplyr::case_when( # round PU numbers based on size
      pu_size < 100 ~ 100,
      dplyr::between(pu_size, 100, 300) ~ plyr::round_any(pu_size, accuracy = 50, floor),
      pu_size > 300 ~ plyr::round_any(pu_size, accuracy = 100, floor)
    )

    while (pu_sum >= pu_threshold) { # ensure that the final # of PUs is < 850,000
      pu_size <- pu_size + dplyr::case_when( # for this, make the PUs larger (so decrease the number) within the planning region
        dplyr::between(pu_size, 100, 300) ~ 50,
        pu_size > 300 ~ 100
      )

      rasterMask <- terra::rast( # create a raster with the current PU size, the new crs and the spatial extent of the data
        resolution = pu_size,
        crs = terra::crs(boundary_proj),
        extent = terra::ext(boundary_proj)
      )

      r1 <- terra::rasterize(terra::vect(boundary_proj), # rasterize the data (sf object) based on the created raster
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
      crs = terra::crs(boundary_proj),
      extent = terra::ext(boundary_proj)
    )

    r1 <- terra::rasterize(terra::vect(boundary_proj), # rasterize the data (sf object) based on the created raster
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

  if (!is.null(output_path)) {
    terra::writeRaster(r1,
    glue::glue("{output_path}/planning_units_{iso3}.tif"),
    gdal = c("COMPRESS=ZSTD", "NUM_THREADS=4", "OVERVIEWS=NONE"),
    NAflag = 255,
    overwrite = TRUE,
    filetype = "COG",
    datatype = "INT1U"
  )
  }

  return(r1)
}


# Riparian and coastal zones -------------------------------------------------------
#
# Context layers for land along rivers and coasts, as the share of each
# planning unit inside the zone (0-1). Buffers are dissolved before they are
# measured, so a cell where buffers of several river segments overlap counts
# once.

#' Make a riparian zone layer
#'
#' Buffers a river network and returns the share of each planning unit
#' (0-1) that lies within the buffer. Use it to identify riparian land, for
#' example to split Restore into riparian restoration with
#' `elsar_sub_actions()`, or as an ELSA input feature.
#'
#' Buffers of all river segments are dissolved before the cover is measured,
#' so cells where segments meet or run side by side are counted once, never
#' once per segment. Rivers are cropped to the planning units (plus the buffer
#' width) first, so a global dataset can be passed as is.
#'
#' Buffers are drawn around the river's centre line. For wide rivers, the
#' channel itself falls inside the zone; mask open water separately if it
#' matters.
#'
#' @param rivers_in An `sf` object of river lines, e.g. HydroRIVERS
#'   (<https://www.hydrosheds.org/products/hydrorivers>) or a national river
#'   network. Any CRS; it is transformed to that of `pus`.
#' @param pus A `SpatRaster` of planning units (1 inside, `NA` outside), in a
#'   projected CRS in metres.
#' @param iso3 Optional ISO3 country code, used in messages and in the output
#'   file name.
#' @param buffer_m Buffer width on each side of the river, in metres: a single
#'   number (default 1000), or the name of a column of `rivers_in` with a width
#'   for each segment (e.g. wider buffers for larger rivers).
#' @param filter_column,min_value Optional. Keep only segments whose
#'   `filter_column` is at least `min_value`, e.g. `filter_column = "ORD_STRA"`
#'   (Strahler order) and `min_value = 4` for HydroRIVERS, or
#'   `"UPLAND_SKM"` (upstream area, km2) to drop small streams.
#' @param name_out Name of the output layer and file (default
#'   `"riparian_zone"`).
#' @param output_path Optional directory to save the layer to, as
#'   `<name_out>_<iso3>.tif`.
#'
#' @return A single-layer `SpatRaster` on the planning-unit grid with the share
#'   of each planning unit inside the riparian zone (0-1), `NA` outside the
#'   planning units.
#' @seealso [make_coastal_zone()]
#' @export
#'
#' @examples
#' pus <- terra::rast(
#'   nrows = 10, ncols = 10, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000,
#'   crs = "ESRI:54009", vals = 1
#' )
#' river <- sf::st_sf(
#'   ORD_STRA = 5,
#'   geometry = sf::st_sfc(
#'     sf::st_linestring(rbind(c(-1000, 2000), c(11000, 8000))),
#'     crs = "ESRI:54009"
#'   )
#' )
#' riparian <- make_riparian_zone(river, pus, buffer_m = 500)
#' terra::plot(riparian)
make_riparian_zone <- function(rivers_in,
                               pus,
                               iso3 = NULL,
                               buffer_m = 1000,
                               filter_column = NULL,
                               min_value = NULL,
                               name_out = "riparian_zone",
                               output_path = NULL) {
  .check_zone_args(rivers_in, "rivers_in", pus, iso3, name_out, output_path)
  assertthat::assert_that(
    (is.numeric(buffer_m) && length(buffer_m) == 1 && buffer_m > 0) ||
      (assertthat::is.string(buffer_m) && buffer_m %in% names(rivers_in)),
    msg = "'buffer_m' must be a single number above 0, or the name of a column of 'rivers_in'."
  )
  assertthat::assert_that(
    is.null(filter_column) == is.null(min_value),
    msg = "Give both 'filter_column' and 'min_value', or neither."
  )
  if (!is.null(filter_column)) {
    assertthat::assert_that(
      assertthat::is.string(filter_column), filter_column %in% names(rivers_in),
      msg = "'filter_column' must name a column of 'rivers_in'."
    )
    assertthat::assert_that(
      is.numeric(min_value), length(min_value) == 1,
      msg = "'min_value' must be a single number."
    )
    keep <- !is.na(rivers_in[[filter_column]]) & rivers_in[[filter_column]] >= min_value
    log_message("Keeping {sum(keep)} of {nrow(rivers_in)} river segments with {filter_column} >= {min_value}.")
    rivers_in <- rivers_in[keep, ]
  }

  widths <- if (is.character(buffer_m)) as.numeric(rivers_in[[buffer_m]]) else buffer_m
  if (is.character(buffer_m)) {
    assertthat::assert_that(
      !anyNA(widths), all(widths > 0),
      msg = paste0("Column '", buffer_m, "' must hold buffer widths above 0 for every segment.")
    )
  }
  max_width <- if (length(widths) > 0) max(widths) else 0

  rivers <- .crop_to_pus(rivers_in, pus, max_width)
  log_message("Making riparian zone{.iso3_msg(iso3)} from {nrow(rivers)} river segments...")
  if (nrow(rivers) > 0 && is.character(buffer_m)) widths <- as.numeric(rivers[[buffer_m]])

  zone <- if (nrow(rivers) == 0) {
    log_message("No rivers within the planning units. Returning an empty zone.")
    NULL
  } else {
    sf::st_buffer(sf::st_geometry(rivers), dist = widths, nQuadSegs = 8)
  }
  .finish_zone(zone, pus, iso3, name_out, output_path)
}

#' Make a coastal zone layer
#'
#' Returns, for each planning unit, either the share (0-1) that lies within
#' `distance_km` of the coast, or the distance from its centre to the coast.
#' Use it to identify coastal land, for example to split Manage or Restore
#' into coastal sub-actions with `elsar_sub_actions()`, or as an ELSA input
#' feature.
#'
#' The coast is given as lines, or as polygons whose outlines are the coast:
#' land polygons (e.g. OpenStreetMap land polygons, Natural Earth land, GSHHG)
#' or ocean polygons. **Do not pass the country boundary**: its land borders
#' would count as coast. Polygons split into tiles (such as OpenStreetMap's
#' "split" land polygons) add straight cut lines that are not coast: use the
#' complete polygons or the coastline lines instead. The coastline is cropped to the planning units (plus
#' `distance_km`) first, so a global dataset can be passed as is.
#'
#' @param coastline_in An `sf` object of coastline lines, or of land or ocean
#'   polygons. Any CRS; it is transformed to that of `pus`.
#' @param pus A `SpatRaster` of planning units (1 inside, `NA` outside), in a
#'   projected CRS in metres.
#' @param iso3 Optional ISO3 country code, used in messages and in the output
#'   file name.
#' @param distance_km Width of the coastal zone in km (default 10). With
#'   `type = "distance"`, distances within `distance_km` of the coast are
#'   exact, and coastline more than `distance_km` beyond the planning units
#'   is ignored.
#' @param type `"zone"` (default) for the share of each planning unit within
#'   `distance_km` of the coast, or `"distance"` for the distance (km) from
#'   each planning unit's centre to the nearest coast. The zone is exact.
#'   Distances are exact for planning units within `distance_km` of the coast:
#'   they are measured from the cell centre to the coastline itself. Further
#'   inland, they are measured to the nearest cell the coastline passes
#'   through, which is exact to within half a cell diagonal (about 0.7 km for
#'   1 km planning units). Measuring every cell exactly would compare it with
#'   every coastline vertex, which takes hours on fine grids. Raise
#'   `distance_km` to make more of the layer exact.
#' @param name_out Name of the output layer and file (default
#'   `"coastal_zone"`, or `"coast_distance"` for `type = "distance"`).
#' @param output_path Optional directory to save the layer to, as
#'   `<name_out>_<iso3>.tif`.
#'
#' @return A single-layer `SpatRaster` on the planning-unit grid, `NA` outside
#'   the planning units.
#' @seealso [make_riparian_zone()]
#' @export
#'
#' @examples
#' pus <- terra::rast(
#'   nrows = 10, ncols = 10, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000,
#'   crs = "ESRI:54009", vals = 1
#' )
#' coast <- sf::st_sf(geometry = sf::st_sfc(
#'   sf::st_linestring(rbind(c(0, -1000), c(0, 11000))), crs = "ESRI:54009"
#' ))
#' coastal <- make_coastal_zone(coast, pus, distance_km = 3)
#' distance <- make_coastal_zone(coast, pus, distance_km = 3, type = "distance")
#' terra::plot(c(coastal, distance))
make_coastal_zone <- function(coastline_in,
                              pus,
                              iso3 = NULL,
                              distance_km = 10,
                              type = c("zone", "distance"),
                              name_out = NULL,
                              output_path = NULL) {
  type <- match.arg(type)
  name_out <- name_out %||% if (type == "zone") "coastal_zone" else "coast_distance"
  .check_zone_args(coastline_in, "coastline_in", pus, iso3, name_out, output_path)
  assertthat::assert_that(
    is.numeric(distance_km), length(distance_km) == 1, distance_km > 0,
    msg = "'distance_km' must be a single number above 0."
  )

  # Polygon outlines are the coast
  geom_types <- unique(as.character(sf::st_geometry_type(coastline_in)))
  is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  assertthat::assert_that(
    all(is_poly | geom_types %in% c("LINESTRING", "MULTILINESTRING")),
    msg = "'coastline_in' must contain lines or polygons."
  )
  coast <- sf::st_geometry(coastline_in)
  if (any(is_poly)) coast <- sf::st_boundary(sf::st_make_valid(coast))
  coast <- sf::st_sf(geometry = coast)

  coast <- .crop_to_pus(coast, pus, distance_km * 1000)
  log_message("Making coastal {type} layer{.iso3_msg(iso3)} from {nrow(coast)} coastline feature(s)...")

  if (type == "distance") {
    if (nrow(coast) == 0) {
      warning(
        "No coastline within ", distance_km, " km of the planning units; distances are all NA.",
        call. = FALSE
      )
      out <- terra::ifel(pus == 1, NA_real_, NA)
    } else {
      out <- .coast_distance(coast, pus[[1]], distance_km)
    }
    names(out) <- name_out
    .save_zone(out, iso3, name_out, output_path)
    return(out)
  }

  zone <- if (nrow(coast) == 0) {
    log_message("No coastline within {distance_km} km of the planning units. Returning an empty zone.")
    NULL
  } else {
    sf::st_buffer(sf::st_geometry(coast), dist = distance_km * 1000, nQuadSegs = 8)
  }
  .finish_zone(zone, pus, iso3, name_out, output_path)
}

# Internal helpers ---------------------------------------------------------------

.check_zone_args <- function(x, arg, pus, iso3, name_out, output_path) {
  assertthat::assert_that(
    inherits(x, "sf"),
    msg = paste0("'", arg, "' must be an sf object.")
  )
  assertthat::assert_that(
    inherits(pus, "SpatRaster"),
    msg = "'pus' must be a SpatRaster object."
  )
  assertthat::assert_that(
    !terra::is.lonlat(pus),
    msg = "'pus' must be in a projected CRS in metres, as made by make_planning_units()."
  )
  assertthat::assert_that(
    is.null(iso3) || assertthat::is.string(iso3),
    msg = "'iso3' must be NULL or a character string."
  )
  assertthat::assert_that(
    assertthat::is.string(name_out), nzchar(name_out),
    msg = "'name_out' must be a character string."
  )
  if (!is.null(output_path)) {
    assertthat::assert_that(
      dir.exists(output_path),
      msg = glue::glue("'output_path' directory does not exist: {output_path}")
    )
  }
  invisible(TRUE)
}

.iso3_msg <- function(iso3) if (is.null(iso3)) "" else paste0(" for ", iso3)

# Features that can reach the planning units, in the planning units' CRS
.crop_to_pus <- function(x, pus, margin_m) {
  if (nrow(x) == 0) return(sf::st_transform(x, sf::st_crs(pus)))
  area <- sf::st_as_sfc(sf::st_bbox(pus))
  area <- sf::st_buffer(area, margin_m + max(terra::res(pus)))
  if (sf::st_crs(x) != sf::st_crs(pus)) {
    # Select in the input's CRS first, so a global layer is not transformed
    # whole. Densify the box so its edges stay in place when transformed.
    span <- max(diff(sf::st_bbox(area)[c(1, 3)]), diff(sf::st_bbox(area)[c(2, 4)]))
    area_x <- sf::st_transform(sf::st_segmentize(area, span / 100), sf::st_crs(x))
    x <- x[lengths(sf::st_intersects(x, area_x)) > 0, ]
    x <- sf::st_transform(x, sf::st_crs(pus))
  }
  x[lengths(sf::st_intersects(x, area)) > 0, ]
}

# Cover fraction of the (dissolved) zone, masked to the planning units
.finish_zone <- function(zone, pus, iso3, name_out, output_path) {
  template <- pus[[1]]
  out <- if (is.null(zone)) {
    terra::ifel(template == 1, 0, NA)
  } else {
    cov <- coverage_fraction_dissolved(template, sf::st_sf(geometry = zone))
    terra::ifel(template == 1, cov, NA)
  }
  names(out) <- name_out
  .save_zone(out, iso3, name_out, output_path)
  out
}

.save_zone <- function(r, iso3, name_out, output_path) {
  if (is.null(output_path)) return(invisible(NULL))
  suffix <- if (is.null(iso3)) "" else paste0("_", iso3)
  save_raster(
    raster = r,
    filename = file.path(output_path, paste0(name_out, suffix, ".tif")),
    datatype = "FLT4S"
  )
}

# Distance (km) from each planning unit's centre to the coast. A fast raster
# distance (to the nearest cell the coastline touches, on a grid padded so
# coastline just outside the planning units counts) is within half a cell
# diagonal of the truth. Cells it puts within `distance_km` (plus that error)
# are then measured exactly, from the cell centre to the coastline geometry.
.coast_distance <- function(coast, template, distance_km) {
  res <- terra::res(template)
  pad <- ceiling(distance_km * 1000 / min(res))
  padded <- terra::extend(template, pad)
  coast_r <- terra::rasterize(terra::vect(coast), padded, touches = TRUE)
  d <- terra::crop(terra::distance(coast_r, unit = "m"), template)
  d <- terra::ifel(template == 1, d, NA)

  error_m <- sqrt(sum(res^2)) / 2
  near <- which(terra::values(d, mat = FALSE) <= distance_km * 1000 + error_m)
  if (length(near) > 0) {
    pieces <- .split_lines(sf::st_geometry(coast))
    exact <- numeric(length(near))
    # Batches keep memory flat on fine grids
    for (b in split(seq_along(near), ceiling(seq_along(near) / 2e5))) {
      xy <- terra::xyFromCell(template, near[b])
      pts <- sf::st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = sf::st_crs(coast))
      nearest <- sf::st_nearest_feature(pts, pieces)
      exact[b] <- as.numeric(sf::st_distance(pts, pieces[nearest], by_element = TRUE))
    }
    d[near] <- exact
  }
  d / 1000
}

# Coastline as short line pieces (at most `max_vertices` each), so the
# spatial index of st_nearest_feature() can skip most of a long coastline
.split_lines <- function(geom, max_vertices = 32) {
  lines <- sf::st_cast(sf::st_cast(geom, "MULTILINESTRING"), "LINESTRING")
  pieces <- lapply(lines, function(l) {
    m <- sf::st_coordinates(l)[, 1:2, drop = FALSE]
    n <- nrow(m)
    if (n <= max_vertices) return(list(m))
    starts <- seq(1, n - 1, by = max_vertices - 1)
    lapply(starts, function(i) m[i:min(i + max_vertices - 1, n), , drop = FALSE])
  })
  pieces <- unlist(pieces, recursive = FALSE)
  sf::st_sfc(lapply(pieces, sf::st_linestring), crs = sf::st_crs(geom))
}

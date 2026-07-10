#' Create a boundary of the planning region
#'
#' @param boundary_in A file containing the boundary information. Can be `sf` or `SpatRaster`
#' @param input_type A string that is either "sf" or "SpatRaster" (default is "sf").
#' @param limit_to_mainland Logical. When `TRUE`, keep only the single largest
#'   polygon (the mainland), dropping islands and other outlying parts. Applied
#'   after any `iso3` filter, so the largest polygon is chosen within the target
#'   region. Default `FALSE`.
#' @param col_name A string of the column containing the actual extent of the planning region (not outside area). Can be `NULL`.
#' @param filter_out A value representing the outside area in the data (e.g. `0`)
#' @param custom_projection Logical. `TRUE`if custom projection for planning region is wanted.
#' @param iso3 The iso3 country code (character) of the country of interest.
#' @param iso3_column Only relevant when `iso3` != NULL. A string of the name of where iso3 information can be found in a dataset.
#' @param filter_by_iso3 Logical. When `TRUE` (default) and `iso3` is supplied, the
#'   boundary is filtered to the feature(s) where `iso3_column == iso3`. Set
#'   `FALSE` to use the whole input (e.g. a custom regional boundary already
#'   clipped to the study area) while still passing `iso3` for the projection name.
#' @param dissolve Logical. When `TRUE`, dissolve the (optionally filtered)
#'   features into a single geometry so the whole input is treated as one
#'   planning region. Useful for multi-feature regional boundaries (e.g. several
#'   provinces). Default `FALSE`.
#' @param output_path An optional output path for the created file. Only needed when custom_projection = TRUE.
#'
#' @return `sf` object of the boundary of the planning region
#' @export
#'
#' @examples
#' boundary <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
make_boundary <- function(boundary_in,
                          input_type = "sf", # sf, raster
                          limit_to_mainland = FALSE,
                          col_name = NULL,
                          filter_out = 0,
                          custom_projection = TRUE,
                          iso3 = NULL,
                          iso3_column = NULL,
                          filter_by_iso3 = TRUE,
                          dissolve = FALSE,
                          output_path = NULL) {
  # Input validation
  assertthat::assert_that(
    input_type %in% c("sf", "SpatRaster"),
    msg = "'input_type' must be either 'sf' or 'SpatRaster'."
  )
  if (input_type == "sf") {
    assertthat::assert_that(inherits(boundary_in, "sf"),
                            msg = "'boundary_in' must be an sf object when input_type = 'sf'.")
  } else {
    assertthat::assert_that(inherits(boundary_in, "SpatRaster"),
                            msg = "'boundary_in' must be a SpatRaster when input_type = 'SpatRaster'.")
  }
  if (!is.null(iso3)) {
    assertthat::assert_that(assertthat::is.string(iso3),
                            msg = "'iso3' must be a character string.")
    assertthat::assert_that(!filter_by_iso3 || !is.null(iso3_column),
                            msg = "'iso3_column' must be provided when filtering by 'iso3'.")
  }
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  log_message("Creating boundary for planning region...")

  if (input_type == "sf") {
    nb <- boundary_in
  } else if (input_type == "SpatRaster") {
    nb <- boundary_in %>%
      terra::as.polygons() %>%
      sf::st_as_sf()
  }

  # Optionally drop non-data features (e.g. background coded as 0/NA)
  if (!is.null(col_name)) {
    assertthat::assert_that(
      col_name %in% names(nb),
      msg = glue::glue("'col_name' ('{col_name}') is not a column in the boundary data.")
    )
    nb <- nb %>%
      dplyr::filter(!!rlang::sym(col_name) != filter_out)
  }

  # Filter to the country/region of interest. Do this BEFORE limiting to the
  # mainland, so "largest polygon" is chosen within the target region rather
  # than across the whole input dataset.
  if (!is.null(iso3) && filter_by_iso3) {
    assertthat::assert_that(
      iso3_column %in% names(nb),
      msg = glue::glue("'iso3_column' ('{iso3_column}') is not a column in the boundary data.")
    )
    available <- unique(as.character(nb[[iso3_column]]))
    nb <- nb %>%
      dplyr::filter(!!rlang::sym(iso3_column) == iso3)
    assertthat::assert_that(
      nrow(nb) > 0,
      msg = glue::glue(
        "No features found for iso3 = '{iso3}' in column '{iso3_column}'. ",
        "Available values: {paste(utils::head(available, 25), collapse = ', ')}",
        if (length(available) > 25) {
          glue::glue(" (and {length(available) - 25} more).")
        } else {
          "."
        }
      )
    )
  }

  # Limit to the largest (mainland) polygon, dropping islands etc.
  if (isTRUE(limit_to_mainland)) {
    nb <- nb %>%
      sf::st_cast("POLYGON")
    nb <- nb %>%
      dplyr::slice(which.max(as.numeric(sf::st_area(nb))))
  }

  # Normalise to WGS84 before any custom projection is applied.
  nb <- sf::st_transform(nb, crs = sf::st_crs(4326))

  if (dissolve) { # treat multiple features (e.g. provinces) as one region
    nb <- nb %>%
      sf::st_make_valid() %>%
      sf::st_union() %>%
      sf::st_sf(geometry = .)
  }

  if (custom_projection) {
    wkt <- make_custom_mollweide_projection(
      boundary = nb,
      output_path = output_path,
      iso3_column = iso3_column,
      iso3 = iso3
    )
    nb <- sf::st_transform(nb, crs = sf::st_crs(wkt))
  }

  return(nb)
}

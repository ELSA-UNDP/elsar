#' Create a Binary or Fractional Raster of Protected Areas Aligned to Planning Units
#'
#' Generates a raster of protected areas for a given country, aligned to the planning
#' units. Data can be downloaded from the [Protected Planet](https://www.protectedplanet.net/)
#' database via the \pkg{wdpar} package, or provided as an `sf` object.
#'
#' All geometries are reprojected, dissolved to avoid double-counting overlaps, and
#' rasterized. The output can be binary (presence/absence based on a coverage threshold)
#' or fractional (proportion of each planning unit covered).
#'
#' @param pus SpatRaster. Planning units raster defining the output resolution and extent.
#' @param iso3 Character. ISO3 country code (e.g., "CHL").
#' @param from_wdpa Logical. If `TRUE`, downloads protected area data using the
#'   \pkg{wdpar} package. If `FALSE`, uses `sf_in`. Default is `TRUE`.
#' @param sf_in sf object or NULL. Protected area geometries to use when
#'
#'   `from_wdpa = FALSE`. Required if `from_wdpa = FALSE`.
#' @param download_path Character or NULL. Directory where WDPA data will be saved
#'   or loaded from. Required if `from_wdpa = TRUE`.
#' @param status Character vector. Which `STATUS` values to include from WDPA data.
#'   Valid values: "Designated", "Established", "Inscribed", "Proposed", "Adopted".
#'   Default is `c("Established", "Inscribed", "Designated")`.
#' @param pa_def Integer vector. Values for the `PA_DEF` column (1 = Protected Area,
#'   0 = OECM). Default is `1`.
#' @param include_mab_designation Logical. If `FALSE`, excludes UNESCO Man and
#'
#'   Biosphere (MAB) reserves. Default is `FALSE`.
#' @param buffer_points Logical. If `TRUE`, creates geodesic buffers around point
#'   geometries using the area attribute. Default is `TRUE`.
#' @param area_column Character. Column name containing area values for buffering
#'   point geometries. Default is `"REP_AREA"`.
#' @param area_calc_crs Character. CRS to use for buffer calculations.
#'   Default is `"ESRI:54009"` (World Mollweide).
#' @param n_quad_segs Integer. Number of segments per quarter circle when creating
#'   buffers. Default is `50`.
#' @param binary Logical. If `TRUE`, output is binary (0/1) based on `threshold`.
#'   If `FALSE`, returns fractional coverage. Default is `TRUE`.
#' @param threshold Numeric. Coverage fraction threshold for binary classification.
#'   Planning units with coverage above this value are classified as protected.
#'   Only used when `binary = TRUE`. Default is `0.10`.
#' @param force_update Logical. If `TRUE`, forces re-download of WDPA data even
#'   if cached. Default is `FALSE`.
#' @param output_path Character or NULL. Directory to save the output raster.
#'   If NULL, the raster is returned but not saved.
#'
#' @return If `sf_in` was provided (i.e., `from_wdpa = FALSE`), returns a SpatRaster.
#'   If data was downloaded from WDPA (`from_wdpa = TRUE`), returns a list with:
#' \describe{
#'   \item{raster}{SpatRaster of protected areas}
#'   \item{sf}{sf object of dissolved protected area polygons for reuse}
#' }
#'
#' @export
make_protected_areas <- function(
    pus,
    iso3,
    from_wdpa = TRUE,
    sf_in = NULL,
    download_path = NULL,
    status = c("Established", "Inscribed", "Designated"),
    pa_def = 1,
    include_mab_designation = FALSE,
    buffer_points = TRUE,
    area_column = "REP_AREA",
    area_calc_crs = "ESRI:54009",
    n_quad_segs = 50,
    binary = TRUE,
    threshold = 0.10,
    force_update = FALSE,
    output_path = NULL
) {
  # Input validation
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(
    all(status %in% c("Designated", "Established", "Inscribed", "Proposed", "Adopted")),
    msg = "status must be one of: Designated, Established, Inscribed, Proposed, Adopted"
  )
  assertthat::assert_that(
    all(pa_def %in% c(0, 1)),
    msg = "pa_def must be 0 (OECM) or 1 (Protected Area)"
  )

  if (from_wdpa) {
    assertthat::assert_that(
      !is.null(download_path),
      msg = "download_path is required when from_wdpa = TRUE"
    )
  } else {
    assertthat::assert_that(
      !is.null(sf_in) && inherits(sf_in, "sf"),
      msg = "sf_in must be a valid sf object when from_wdpa = FALSE"
    )
  }

  # Load data
  if (from_wdpa) {
    wdpa_dir <- file.path(download_path, "wdpa_downloads")
    if (!dir.exists(wdpa_dir)) dir.create(wdpa_dir, recursive = TRUE)

    log_msg("Downloading protected areas using the wdpar package")
    protected_areas <- wdpar::wdpa_fetch(
      iso3,
      wait = TRUE,
      check_version = TRUE,
      force_download = force_update,
      download_dir = wdpa_dir
    )
  } else {
    protected_areas <- sf_in
  }

  log_msg(glue::glue("Including {glue::glue_collapse(status, sep = ', ', last = ' and ')} areas only"))

  # Filter data by STATUS and PA_DEF
  protected_areas <- protected_areas %>%
    dplyr::filter(.data$STATUS %in% status, .data$PA_DEF %in% pa_def)

  # Remove MAB designated areas if specified
  if (!include_mab_designation) {
    log_msg("Excluding UNESCO Man and Biosphere (MAB) reserve areas")
    protected_areas <- protected_areas %>%
      dplyr::filter(!stringr::str_detect(.data$DESIG, "MAB"))
  }

  # If geometry includes POINT/MULTIPOINT, buffer or filter
  geom_type <- sf::st_geometry_type(protected_areas)

  if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
    if (buffer_points) {
      log_msg("Creating geodesic buffers around point geometries")
      protected_areas <- convert_points_polygon(
        sf_layer = protected_areas,
        area_crs = area_calc_crs,
        area_attr = area_column,
        nQuadSegs = n_quad_segs
      ) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        dplyr::summarise() %>%
        sf::st_make_valid()
    } else {
      protected_areas <- protected_areas %>%
        dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))) %>%
        sf::st_transform(sf::st_crs(pus)) %>%
        sf::st_make_valid()
    }
  } else {
    # If already polygonal, reproject and dissolve
    protected_areas <- protected_areas %>%
      sf::st_transform(sf::st_crs(pus)) %>%
      dplyr::summarise() %>%
      sf::st_make_valid()
  }

  # Rasterize with coverage fraction
  log_msg("Generating raster from dissolved protected area geometries")
  protected_areas_raster <- exactextractr::coverage_fraction(pus, protected_areas)[[1]]

  # Binary conversion if requested
  if (binary) {
    threshold <- threshold %||% 0  # Ensure threshold is numeric and not NULL
    protected_areas_raster <- terra::ifel(protected_areas_raster > threshold, 1, 0)
  }

  # Mask to original planning units extent (retain NA where no data)
  protected_areas_raster <- make_normalised_raster(
    protected_areas_raster,
    pus = pus,
    iso3 = iso3
  )

  # Save raster
  if (!is.null(output_path)) {
    elsar::save_raster(
      raster = protected_areas_raster,
      filename = glue::glue("{output_path}/protected_areas_{iso3}.tif"),
      datatype = if (binary) "INT1U" else "FLT4S"
    )
  }

  # Return sf object alongside raster when data was downloaded (for reuse)
  if (from_wdpa) {
    protected_areas_sf <- sf::st_cast(protected_areas, "POLYGON")
    return(list(
      raster = protected_areas_raster,
      sf = protected_areas_sf
    ))
  }

  return(protected_areas_raster)
}

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
#' @param from_wdpca Logical. If `TRUE`, downloads protected area data using the
#'   \pkg{wdpar} package. If `FALSE`, uses `sf_in`. Default is `TRUE`.
#' @param sf_in sf object or NULL. Protected area geometries to use when
#'
#'   `from_wdpca = FALSE`. Required if `from_wdpca = FALSE`.
#' @param download_path Character or NULL. Directory where WDPCA data will be saved
#'   or loaded from. Required if `from_wdpca = TRUE`.
#' @param status Character vector. Which `STATUS` values to include from WDPCA data.
#'   Valid values: "Designated", "Established", "Inscribed", "Proposed", "Adopted".
#'   Default is `c("Established", "Inscribed", "Designated")`.
#' @param site_type Character vector. Values for the `SITE_TYPE` column in WDPCA
#'   data. Valid values: "PA" (Protected Area) or "OECM" (Other Effective
#'   Area-based Conservation Measure). Default is `"PA"`. For backward
#'   compatibility, numeric values 1 (PA) and 0 (OECM) are also accepted.
#' @param pa_def Deprecated. Use `site_type` instead. Kept for backward
#'   compatibility.
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
#' @param force_update Logical. If `TRUE`, forces re-download of WDPCA data even
#'   if cached. Default is `FALSE`.
#' @param output_path Character or NULL. Directory to save the output raster.
#'   If NULL, the raster is returned but not saved.
#'
#' @return If `sf_in` was provided (i.e., `from_wdpca = FALSE`), returns a SpatRaster.
#'   If data was downloaded from WDPCA (`from_wdpca = TRUE`), returns a list with:
#' \describe{
#'   \item{raster}{SpatRaster of protected areas}
#'   \item{sf}{sf object of dissolved protected area polygons for reuse}
#' }
#'
#' @export
make_protected_areas <- function(
    pus,
    iso3,
    from_wdpca = TRUE,
    sf_in = NULL,
    download_path = NULL,
    status = c("Established", "Inscribed", "Designated"),
    site_type = "PA",
    pa_def = deprecated(),
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
  # Handle deprecated pa_def parameter
  if (lifecycle::is_present(pa_def)) {
    lifecycle::deprecate_warn(
      "0.0.7",
      "make_protected_areas(pa_def)",
      "make_protected_areas(site_type)"
    )
    site_type <- pa_def
  }

  # Convert old numeric site_type values to new string format
  # 1 -> "PA", 0 -> "OECM" (backward compatibility)
  if (is.numeric(site_type)) {
    site_type <- dplyr::case_when(
      site_type == 1 ~ "PA",
      site_type == 0 ~ "OECM",
      TRUE ~ as.character(site_type)
    )
  }

  # Input validation
  assertthat::assert_that(inherits(pus, "SpatRaster"))
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(
    all(status %in% c("Designated", "Established", "Inscribed",
                      "Proposed", "Adopted")),
    msg = "status must be one of: Designated, Established, Inscribed, Proposed, Adopted"
  )
  assertthat::assert_that(
    all(site_type %in% c("PA", "OECM")),
    msg = "site_type must be 'PA' (Protected Area) or 'OECM'"
  )

  if (from_wdpca) {
    assertthat::assert_that(
      !is.null(download_path),
      msg = "download_path is required when from_wdpca = TRUE"
    )
  } else {
    assertthat::assert_that(
      !is.null(sf_in) && inherits(sf_in, "sf"),
      msg = "sf_in must be a valid sf object when from_wdpca = FALSE"
    )
  }
  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Load data
  if (from_wdpca) {
    wdpa_dir <- file.path(download_path, "wdpa_downloads")
    if (!dir.exists(wdpa_dir)) dir.create(wdpa_dir, recursive = TRUE)

    log_message("Downloading protected areas using the wdpar package")
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

  log_message(
    "Including {glue::glue_collapse(status, sep = ', ', last = ' and ')} areas only"
  )

  # Handle WDPCA schema changes for backward compatibility:
 # - Current WDPCA uses SITE_TYPE with values "PA" or "OECM"
  # - Older cached data uses PA_DEF with numeric values 1 (PA) or 0 (OECM)
  if (!"SITE_TYPE" %in% names(protected_areas) &&
      "PA_DEF" %in% names(protected_areas)) {
    # Convert old numeric PA_DEF to new string SITE_TYPE
    protected_areas$SITE_TYPE <- dplyr::case_when(
      protected_areas$PA_DEF == 1 ~ "PA",
      protected_areas$PA_DEF == 0 ~ "OECM",
      TRUE ~ as.character(protected_areas$PA_DEF)
    )
  }

  # Filter data by STATUS and SITE_TYPE
  protected_areas <- protected_areas %>%
    dplyr::filter(.data$STATUS %in% status, .data$SITE_TYPE %in% site_type)

  # Remove MAB designated areas if specified
  # Use DESIG_ENG (English designation) when available in newer WDPCA data,

  # falling back to DESIG (original language) for older cached data
  if (!include_mab_designation) {
    log_message("Excluding UNESCO Man and Biosphere (MAB) reserve areas")
    desig_col <- if ("DESIG_ENG" %in% names(protected_areas)) "DESIG_ENG" else "DESIG"
    protected_areas <- protected_areas %>%
      dplyr::filter(!stringr::str_detect(.data[[desig_col]], "MAB"))
  }

  # If geometry includes POINT/MULTIPOINT, buffer or filter
  geom_type <- sf::st_geometry_type(protected_areas)

  if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
    if (buffer_points) {
      log_message("Creating geodesic buffers around point geometries")
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
  log_message("Generating raster from dissolved protected area geometries")
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
  if (from_wdpca) {
    protected_areas_sf <- sf::st_cast(protected_areas, "POLYGON")
    return(list(
      raster = protected_areas_raster,
      sf = protected_areas_sf
    ))
  }

  return(protected_areas_raster)
}

#' Create a Standardised Raster of Key Biodiversity Areas (KBAs)
#'
#' This function processes a Key Biodiversity Areas (KBA) vector dataset and converts it into
#' a normalised raster aligned with the input planning units. It allows for filtering to
#' include or exclude Alliance for Zero Extinction (AZE) sites, as well as optionally excluding
#' KBAs marked as "Regional". By default, it includes all KBAs except regional-only sites.
#' It can also be used to return only AZE sites if `aze_only = TRUE`.
#'
#' @param kba_in An `sf` object containing KBA vector features, including columns like `iso3`, `azestatus`, and `kba_qual`.
#' @param pus A `SpatRaster` object representing planning units (reference extent and resolution).
#' @param iso3 A character string representing the 3-letter ISO country code (e.g., "KEN").
#' @param include_aze_sites Logical. If `TRUE`, includes KBAs that are also AZE sites (default is `FALSE`).
#' @param aze_only Logical. If `TRUE`, returns only confirmed AZE sites (default is `FALSE`).
#' @param include_regional_kba Logical. If `FALSE`, filters out KBAs marked as "Regional" or "Global/ Regional to be determined".
#' @param output_path Optional character. Directory path to save output raster. If `NULL`, output is not written to file.
#'
#' @return A `SpatRaster` with normalised values showing KBA (or AZE) coverage across planning units.
#' @export
#'
#' @examples
#' \dontrun{
#' kba_raster <- make_kbas(
#'   kba_in = kba_sf,
#'   pus = planning_units,
#'   iso3 = "KEN",
#'   aze_only = TRUE,
#'   output_path = "outputs",
#'  )
#' }
make_kbas <- function(
    kba_in,
    pus,
    iso3,
    include_aze_sites = FALSE,
    aze_only = FALSE,
    include_regional_kba = FALSE,
    output_path = NULL
) {
  # Validate inputs
  assertthat::assert_that(inherits(kba_in, "sf"), msg = "kba_in must be an sf object.")
  assertthat::assert_that(inherits(pus, "SpatRaster"), msg = "pus must be a SpatRaster object.")
  assertthat::assert_that(is.character(iso3) && nchar(iso3) == 3, msg = "iso3 must be a 3-character country code.")

  # To keep consistent with use of iso3 elsewhere, and to avoid issues around filtering
  # on the attribute of the same name in the KBA dataset, we use iso3_filter.
  iso3_filter <- iso3

  kba <- kba_in %>%
    dplyr::filter(iso3 == iso3_filter)

  if (aze_only) {
    cat("Returning only AZE sites.\n")
    kba <- kba %>%
      dplyr::filter(azestatus == "confirmed")
  } else if (!include_aze_sites) {
    cat("Excluding AZE sites.\n")
    kba <- kba %>%
      dplyr::filter(is.na(azestatus) | azestatus != "confirmed")
  } else {
    cat("Including AZE sites.\n")
  }

  if (nrow(kba) == 0) {
    if (aze_only) {
      cat("No matching AZE sites found in the study region — returning empty raster.\n")
    } else {
      cat("No matching KBA features found in the study region — returning empty raster.\n")
    }
    kba <- pus
  } else {
    if (!include_regional_kba) {
      cat("Excluding Regional KBAs or those with undetermined Global status.\n")
      kba <- kba %>%
        dplyr::filter(kba_qual %ni% c("Regional", "Global/ Regional to be determined"))
    }

    kba <- kba %>%
      sf::st_transform(crs = sf::st_crs(pus)) %>%
      sf::st_make_valid() %>%
      dplyr::summarise() %>%
      sf::st_make_valid()

    cat("Rasterising and normalising features...\n")
    kba <- exactextractr::coverage_fraction(pus, kba)[[1]] %>%
      elsar::make_normalised_raster(
        pus = pus,
        iso3 = iso3
        )
  }

  if (!is.null(output_path)) {
    # Auto-generate name_out
    if (aze_only) {
      name_out <- "aze_sites"
    } else if (!include_aze_sites) {
      name_out <- "kba_aze_sites"
    } else {
      name_out <- "kba_sites"
    }

    terra::writeRaster(
      kba_out,
      filename = glue::glue("{output_path}/{name_out}_{iso3}.tif"),
      datatype = "FLT4S",
      filetype = "COG",
      gdal = c(
        "COMPRESS=ZSTD",
        "PREDICTOR=3",
        "OVERVIEWS=NONE",
        "NUM_THREADS=ALL_CPUS"
      ),
      overwrite = TRUE
    )
  }

  return(kba)
}

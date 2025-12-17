# LULC Product Configuration
#
# This file contains centralized configuration for supported Land Use/Land Cover (LULC)
# products including class mappings and metadata. This enables consistent handling
# across different LULC data sources.

#' Get LULC Class Mappings for a Product
#'
#' Returns a named list of standardized class mappings for the specified LULC product.
#' This provides consistent access to class values across different LULC products,
#' enabling product-agnostic processing in downstream functions.
#'
#' @param product Character. Product ID: one of "esri_10m", "dynamic_world", or
#'   "esa_worldcover". Each product has different class value schemes.
#'
#' @return A named list with class value mappings. Common keys include:
#'   \itemize{
#'     \item \code{water}: Water bodies
#'     \item \code{trees}: Tree cover / forest
#'     \item \code{agriculture}: Cropland / cultivated areas
#'     \item \code{built_area}: Built-up / urban areas
#'     \item \code{forest_managed}: Managed forest classes (may be vector)
#'     \item \code{forest_all}: All forest-related classes (may be vector)
#'   }
#'
#' @export
#'
#' @examples
#' # Get all class mappings for ESRI 10m LULC
#' classes <- get_lulc_classes("esri_10m")
#' classes$agriculture  # Returns 5
#' classes$built_area   # Returns 7
#'
#' # Get Dynamic World classes
#' dw_classes <- get_lulc_classes("dynamic_world")
#' dw_classes$agriculture  # Returns 4
#'
#' # Get ESA WorldCover classes
#' esa_classes <- get_lulc_classes("esa_worldcover")
#' esa_classes$agriculture  # Returns 40
get_lulc_classes <- function(product = c("esri_10m", "dynamic_world", "esa_worldcover")) {
  product <- match.arg(product)

  class_maps <- list(
    # ESRI Global LULC 10m Time Series
    # Source: https://www.arcgis.com/home/item.html?id=cfcb7609de5f478eb7666240902d4d3d
    esri_10m = list(
      water = 1,
      trees = 2,
      flooded_vegetation = 4,
      agriculture = 5,
      bare_ground = 8,
      snow_ice = 9,
      clouds = 10,
      rangeland = 11,
      built_area = 7,
      # Managed forests include: disturbed (20), plantation (31, 32),
      # agroforestry (40), silviculture (53)
      forest_managed = c(31, 32, 40, 53),
      forest_all = c(2, 20, 31, 32, 40, 53),
      forest_disturbed = 20
    ),

    # Google Dynamic World
    # Source: https://developers.google.com/earth-engine/datasets/catalog/GOOGLE_DYNAMICWORLD_V1
    dynamic_world = list(
      water = 0,
      trees = 1,
      grass = 2,
      flooded_vegetation = 3,
      agriculture = 4,
      shrub_scrub = 5,
      built_area = 6,
      bare_ground = 7,
      snow_ice = 8,
      # Dynamic World has a single "trees" class
      forest_managed = 1,
      forest_all = 1
    ),

    # ESA WorldCover v200 (2021 baseline)
    # Source: https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200
    esa_worldcover = list(
      tree_cover = 10,
      shrubland = 20,
      grassland = 30,
      agriculture = 40,
      built_area = 50,
      bare_sparse = 60,
      snow_ice = 70,
      water = 80,
      herbaceous_wetland = 90,
      mangroves = 95,
      moss_lichen = 100,
      # ESA has a single "tree cover" class
      forest_managed = 10,
      forest_all = c(10, 95),  # Including mangroves
      trees = 10
    )
  )

  class_maps[[product]]
}


#' Get Specific LULC Class Value
#'
#' Retrieves a specific class value for a given LULC product. This is the primary
#' interface for consumer functions to obtain class values dynamically based on
#' the LULC product being used.
#'
#' @param product Character. LULC product ID: one of "esri_10m", "dynamic_world",
#'   or "esa_worldcover".
#' @param class_name Character. Standardized class name to retrieve. Common names
#'   include: "agriculture", "built_area", "trees", "water", "forest_managed",
#'   "forest_all".
#'
#' @return Integer or integer vector of class values for the specified class.
#'
#' @export
#'
#' @examples
#' # Get agriculture class value for different products
#' get_lulc_class_value("esri_10m", "agriculture")       # Returns 5
#' get_lulc_class_value("dynamic_world", "agriculture")  # Returns 4
#' get_lulc_class_value("esa_worldcover", "agriculture") # Returns 40
#'
#' # Get built area class value
#' get_lulc_class_value("esri_10m", "built_area")        # Returns 7
#' get_lulc_class_value("dynamic_world", "built_area")   # Returns 6
#'
#' # Get managed forest classes (may return vector)
#' get_lulc_class_value("esri_10m", "forest_managed")    # Returns c(31, 32, 40, 53)
get_lulc_class_value <- function(product, class_name) {
  # Validate product
  valid_products <- c("esri_10m", "dynamic_world", "esa_worldcover")
  if (!product %in% valid_products) {
    stop(
      glue::glue("Invalid product '{product}'. Must be one of: {paste(valid_products, collapse = ', ')}"),
      call. = FALSE
    )
  }

  classes <- get_lulc_classes(product)

  if (!class_name %in% names(classes)) {
    available <- paste(names(classes), collapse = ", ")
    stop(
      glue::glue(
        "Class '{class_name}' not found for product '{product}'. ",
        "Available classes: {available}"
      ),
      call. = FALSE
    )
  }

  classes[[class_name]]
}


#' List Available LULC Products
#'
#' Returns information about all supported LULC products, including their
#' display names, resolutions, and data sources.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{product_id}: Internal product identifier
#'     \item \code{display_name}: Human-readable product name
#'     \item \code{resolution}: Native resolution in meters
#'     \item \code{source_type}: Data source type ("gee" or "local")
#'     \item \code{temporal}: Temporal availability description
#'   }
#'
#' @export
#'
#' @examples
#' list_lulc_products()
list_lulc_products <- function() {
  tibble::tibble(
    product_id = c("esri_10m", "dynamic_world", "esa_worldcover", "local"),
    display_name = c(
      "ESRI Global LULC 10m",
      "Google Dynamic World",
      "ESA WorldCover v200",
      "User-Provided Local File"
    ),
    resolution = c(10, 10, 10, NA_real_),
    source_type = c("gee", "gee", "gee", "local"),
    temporal = c(
      "Annual (2017-present)",
      "Near real-time (2015-present)",
      "Static (2021 baseline)",
      "User-defined"
    )
  )
}

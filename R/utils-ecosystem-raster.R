#' Ecosystem rasters for the ecosystem coverage layers
#'
#' [make_underrepresented_ecosystems()] and
#' [make_threatened_ecosystems_protection()] accept a classified ecosystem
#' raster as well as polygons. The overlays use exactextractr, so they are
#' exact for the raster as given and never rasterise the other layer
#' approximately:
#'
#' * protected areas are dissolved by tile and their exact coverage fraction
#'   of every ecosystem cell is measured with
#'   [exactextractr::coverage_fraction()] (see [coverage_fraction_dissolved()]);
#' * the intact mask on the planning-unit grid is resampled to the ecosystem
#'   grid with [exactextractr::exact_resample()], which weights every
#'   planning unit by the share of the ecosystem cell it covers;
#' * each ecosystem's score is mapped back to its cells and aggregated to the
#'   planning units with [make_normalised_raster()] (exact_resample in the
#'   same CRS).
#'
#' Areas are summed per class with [terra::zonal()]. The ecosystem raster is
#' first projected (nearest neighbour) to the planning-unit CRS when it is in
#' another one, cropped to the planning units and masked to them, so cells
#' outside the analysis area do not count towards any ecosystem's area.
#'
#' @name ecosystem_raster
#' @keywords internal
NULL

#' Ecosystem groups of a classified raster
#'
#' @param eco Single-layer SpatRaster of ecosystem classes: integer codes, or
#'   a categorical raster (with a levels table). NA is "no ecosystem".
#' @param group_attribute Character or NULL. For a categorical raster, the
#'   column of its levels table to group classes by (NULL: the active
#'   category). Ignored for a plain integer raster, which is grouped by cell
#'   value.
#' @param pus Planning-unit SpatRaster.
#' @return List with `r` (SpatRaster of group ids 1..n on the ecosystem grid,
#'   in the planning-unit CRS, NA outside ecosystems and planning units) and
#'   `groups` (data frame: `gid`, `group`).
#' @keywords internal
prepare_ecosystem_raster <- function(eco, group_attribute, pus) {
  assertthat::assert_that(
    terra::nlyr(eco) == 1,
    msg = "An ecosystem raster must have one layer of ecosystem classes."
  )

  if (terra::is.factor(eco)) {
    lv <- terra::cats(eco)[[1]]
    if (is.null(group_attribute)) {
      group_attribute <- names(lv)[terra::activeCat(eco) + 1]
    }
    assertthat::assert_that(
      group_attribute %in% names(lv)[-1],
      msg = glue::glue(
        "'{group_attribute}' is not a column of the ecosystem raster's levels ",
        "table ({paste(names(lv)[-1], collapse = ', ')})."
      )
    )
    codes <- lv[[1]]
    labels <- lv[[group_attribute]]
    levels(eco) <- NULL # drop the table: classify on the codes
  } else {
    if (!is.null(group_attribute)) {
      log_message("Ecosystem raster has no levels table: grouping by cell value ('{group_attribute}' is not used).")
    }
    codes <- NULL
  }

  if (!terra::same.crs(eco, pus)) {
    log_message("Projecting the ecosystem raster to the planning-unit CRS (nearest neighbour)...")
    eco <- terra::project(eco, terra::crs(pus), method = "near")
  }
  eco <- .crop_to(eco, pus)
  assertthat::assert_that(!is.null(eco),
                          msg = "The ecosystem raster does not overlap the planning units.")
  # Keep only cells inside the planning units (nearest PU, by cell centre)
  inside <- terra::resample(pus, eco, method = "near")
  eco <- terra::mask(eco, inside)

  present <- terra::unique(eco)
  present <- if (is.null(present)) numeric(0) else present[[1]]
  assertthat::assert_that(length(present) > 0,
                          msg = "The ecosystem raster has no ecosystem cells inside the planning units.")
  if (is.null(codes)) {
    assertthat::assert_that(
      all(abs(present - round(present)) < 1e-9),
      msg = "An ecosystem raster without a levels table must hold integer class codes."
    )
    codes <- labels <- present
  }
  # Codes missing from the levels table are dropped (NA), with a message
  unknown <- setdiff(present, codes)
  if (length(unknown) > 0) {
    log_message("Ignoring {length(unknown)} ecosystem raster value(s) missing from its levels table.")
  }
  keep <- codes %in% present
  groups <- unique(labels[keep])
  gid <- match(labels[keep], groups)
  r <- terra::classify(eco, cbind(codes[keep], gid), others = NA)
  names(r) <- "ecosystem"
  list(r = r, groups = data.frame(gid = seq_along(groups), group = groups))
}

#' Total and overlapping area of each ecosystem group, in cells
#'
#' @param r Group-id SpatRaster from [prepare_ecosystem_raster()].
#' @param overlap SpatRaster on the same grid: the fraction (0-1) of every
#'   cell inside the other layer.
#' @return Data frame: `gid`, `area` (cells), `area_overlap` (cells).
#' @keywords internal
ecosystem_raster_areas <- function(r, overlap) {
  ones <- terra::ifel(is.na(r), NA, 1)
  overlap <- terra::ifel(is.na(overlap), 0, overlap)
  z <- terra::zonal(c(ones, overlap), r, fun = "sum", na.rm = TRUE)
  names(z) <- c("gid", "area", "area_overlap")
  z
}

#' Map a score per ecosystem group to the planning units
#'
#' @param r Group-id SpatRaster.
#' @param gid,value Group ids and their scores.
#' @param pus Planning-unit SpatRaster.
#' @param method How ecosystem cells combine in a planning unit (`"max"` or
#'   `"mean"`, area-weighted; see [make_normalised_raster()]).
#' @param iso3 Character or NULL.
#' @param rescaled Logical. Rescale to 0-1 across planning units.
#' @return SpatRaster on the planning-unit grid. Planning units without any
#'   ecosystem cell take the nearest planning unit's value.
#' @keywords internal
ecosystem_raster_to_pus <- function(r, gid, value, pus, method, iso3, rescaled) {
  scores <- terra::classify(r, cbind(gid, value), others = NA)
  on_pus <- make_normalised_raster(scores, pus = pus, iso3 = iso3, rescaled = FALSE,
                                   method_override = method, fill_na = NULL)
  on_pus <- fill_nearest_cells(on_pus, pus)
  make_normalised_raster(on_pus, pus = pus, iso3 = iso3, rescaled = rescaled)
}

#' Give planning units with no value the value of the nearest one that has
#'
#' The raster counterpart of [fill_nearest_feature()]. Only cells on the edge
#' of the valued area are searched, since the nearest valued cell always lies
#' there.
#' @param r SpatRaster on the planning-unit grid.
#' @param pus Planning-unit SpatRaster.
#' @return `r` with every planning unit filled (unchanged if none has a value).
#' @keywords internal
fill_nearest_cells <- function(r, pus) {
  v <- terra::values(r, mat = FALSE)
  in_pus <- !is.na(terra::values(pus, mat = FALSE))
  gap <- which(in_pus & is.na(v))
  if (length(gap) == 0 || all(is.na(v))) return(r)
  log_message("Assigning {length(gap)} planning unit(s) with no ecosystem cell to the nearest one...")
  # Valued cells with at least one neighbour without a value
  has <- terra::ifel(is.na(r), 0, 1)
  n_has <- terra::focal(has, w = 3, fun = "sum", na.policy = "omit", fillvalue = 0)
  edge <- which(!is.na(v) & terra::values(n_has, mat = FALSE) < 9)
  to_pts <- function(cells) {
    xy <- terra::xyFromCell(r, cells)
    sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"),
                 crs = sf::st_crs(terra::crs(r)))
  }
  nn <- sf::st_nearest_feature(to_pts(gap), to_pts(edge))
  r[gap] <- v[edge][nn]
  r
}

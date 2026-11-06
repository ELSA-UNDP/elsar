# Post-analysis tools for ELSA results -----------------------------------------
#
# Functions to summarise, compare and report the action map produced by an ELSA
# run. A prepared result is a categorical SpatRaster with an "action" layer whose
# levels carry the action scheme (label, colour, group, is_action), plus an
# optional "pa_fraction" layer with the share of each cell covered by existing
# protected areas.

#' Define the ELSA action scheme
#'
#' Describes how the values of an ELSA action raster map to actions: their
#' labels, colours and reporting groups, and which value means "no action".
#' Every post-analysis function in elsar reads its labels and colours from this
#' scheme, so a custom scheme carries through to all tables and plots.
#'
#' @param values Numeric vector. The cell values used in the action raster.
#' @param labels Character vector. A label for each value. Must be unique.
#' @param colours Character vector. A colour (name or hex code) for each value.
#' @param groups Character vector. A reporting group for each value, used to
#'   combine actions in bar charts (e.g. two restoration actions reported as
#'   one "Restore" group). Defaults to `labels`.
#' @param no_action Character vector. Labels of the values that mean no action
#'   was assigned. They count towards a region's area but are not reported or
#'   drawn as actions. Use `character(0)` if every value is an action.
#'
#' @return A tibble with one row per value and columns `value`, `label`,
#'   `colour`, `group` and `is_action`.
#'
#' @seealso [elsar_prepare_actions()]
#' @export
#'
#' @examples
#' # The default ELSA scheme
#' elsar_action_scheme()
#'
#' # A run with a fourth action and different colours
#' elsar_action_scheme(
#'   values = 1:5,
#'   labels = c("Protect", "Restore", "Manage", "Agroforestry", "No action"),
#'   colours = c("#1B9E77", "#7570B3", "#E6AB02", "#A6761D", "grey85"),
#'   groups = c("Protect", "Restore", "Manage", "Manage", "No action"),
#'   no_action = "No action"
#' )
elsar_action_scheme <- function(
    values = 1:5,
    labels = c("Protect", "Restore", "Manage", "Urban Greening", "Do Nothing"),
    colours = c("#06C723", "#8803FC", "#FFEA00", "#FF0000", "#E5E5E5"),
    groups = labels,
    no_action = "Do Nothing") {
  n <- length(values)
  assertthat::assert_that(
    is.numeric(values), n > 0,
    msg = "'values' must be a non-empty numeric vector."
  )
  assertthat::assert_that(
    length(labels) == n, length(colours) == n, length(groups) == n,
    msg = "'values', 'labels', 'colours' and 'groups' must have the same length."
  )
  assertthat::assert_that(
    !anyDuplicated(values), !anyDuplicated(labels),
    msg = "'values' and 'labels' must not contain duplicates."
  )
  assertthat::assert_that(
    is.character(no_action), all(no_action %in% labels),
    msg = "Every 'no_action' entry must be one of 'labels'."
  )
  colour_ok <- vapply(colours, function(x) {
    !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
  }, logical(1))
  assertthat::assert_that(
    all(colour_ok),
    msg = paste0("Invalid colour(s): ", toString(colours[!colour_ok]), ".")
  )

  tibble::tibble(
    value = values,
    label = as.character(labels),
    colour = as.character(colours),
    group = as.character(groups),
    is_action = !labels %in% no_action
  )
}

#' Prepare an ELSA action map for post-analysis
#'
#' Reads an ELSA action raster, applies an action scheme to it and, optionally,
#' aligns a protected-area layer to it. The result feeds every other ELSA
#' results function ([elsar_summarise_actions()], [elsar_protection_gap()] and
#' the plotting functions).
#'
#' @param actions A `SpatRaster` (first layer used) or path to the ELSA action
#'   raster, e.g. the `*_ELSA_Action_*.tif` file from an ELSA run.
#' @param scheme A tibble from [elsar_action_scheme()]. Defaults to the
#'   standard ELSA scheme.
#' @param pas Optional existing protected areas, as either:
#'   * a `SpatRaster` (or path) of fractional cover (0-1), such as the output of
#'     [make_protected_areas()] with `binary = FALSE`; binary 0/1 rasters also
#'     work; or
#'   * an `sf` or `SpatVector` of protected-area polygons, whose cover of each
#'     cell is calculated exactly.
#'
#'   Rasters on a different grid are resampled to the action grid.
#' @param pa_threshold Numeric between 0 and 1, or `NULL` (the default). If
#'   `NULL`, protected-area cover is kept as a fraction, so a cell that is 30%
#'   protected contributes 30% of its area to "in existing PAs". If a number,
#'   cells with cover at or above it count as fully protected and all others
#'   as unprotected, which matches how a lock-in threshold was applied in the
#'   run.
#'
#' @return A `SpatRaster` with a categorical `action` layer (levels hold the
#'   scheme, and a colour table is set) and, if `pas` is given, a `pa_fraction`
#'   layer.
#'
#' @section Protected areas in ELSA runs:
#' By default ELSA locks existing protected areas into the Protect zone, so
#' every protected cell is assigned Protect. The protected-area layer then
#' separates *current* protection (Protect inside existing PAs) from *new*
#' protection proposed by ELSA (Protect outside PAs). If protected areas were
#' not locked in, the same split shows which actions ELSA assigned inside
#' them. Either way, summaries report every action's area inside and outside
#' existing PAs.
#'
#' @seealso [elsar_action_scheme()], [elsar_summarise_actions()],
#'   [elsar_example_results()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' x
#' terra::cats(x)[[1]]
elsar_prepare_actions <- function(actions,
                                  scheme = elsar_action_scheme(),
                                  pas = NULL,
                                  pa_threshold = NULL) {
  actions <- .read_raster_arg(actions, "actions")[[1]]
  .check_scheme(scheme)

  if (!is.null(pa_threshold)) {
    assertthat::assert_that(
      is.numeric(pa_threshold), length(pa_threshold) == 1,
      pa_threshold >= 0, pa_threshold <= 1,
      msg = "'pa_threshold' must be NULL or a single number between 0 and 1."
    )
  }

  # Values outside the scheme become NA, with a warning
  present <- terra::unique(actions)[[1]]
  unknown <- setdiff(present, scheme$value)
  if (length(unknown) > 0) {
    warning(
      "Action raster values not in the scheme were set to NA: ",
      toString(unknown), ". Pass a matching scheme to elsar_action_scheme().",
      call. = FALSE
    )
    actions <- terra::classify(actions, cbind(unknown, NA))
  }

  # Round in case a float raster was written (e.g. 1.0000001)
  actions <- terra::round(actions)
  levels(actions) <- as.data.frame(scheme[, c("value", "label", "colour", "group", "is_action")])
  terra::coltab(actions) <- data.frame(value = scheme$value, col = scheme$colour)
  # Set after levels(), which renames the layer to the active category
  names(actions) <- "action"

  if (is.null(pas)) {
    return(actions)
  }

  pa <- .pa_to_fraction(pas, actions)
  if (!is.null(pa_threshold)) {
    pa <- terra::ifel(pa >= pa_threshold, 1, 0)
  }
  pa <- terra::mask(pa, actions)
  names(pa) <- "pa_fraction"

  c(actions, pa)
}

#' Summarise ELSA actions by region
#'
#' Calculates how much area ELSA assigned to each action, in total and per
#' region, and how much of it lies inside existing protected areas.
#'
#' Cell areas come from [terra::cellSize()]: planar areas for projected
#' rasters, which are exact in the equal-area projections ELSA uses (see
#' [make_custom_mollweide_projection()]), and geodesic areas for geographic
#' (lon/lat) rasters. Avoid summarising in projections that distort area,
#' such as Web Mercator. Cells cut by a region boundary are weighted by the
#' share inside the region ([exactextractr::exact_extract()]), so regional
#' totals can be slightly smaller than the national total when planning-unit
#' cells extend beyond the region polygons along the coast or border.
#'
#' @param x A prepared action map from [elsar_prepare_actions()].
#' @param regions Optional `sf` or `SpatVector` of regions (e.g. admin-1
#'   units). If `NULL`, a single national summary is returned.
#' @param region_col Character. Column in `regions` holding region names.
#'   Rows that share a name are combined into one region (e.g. a multi-part
#'   region stored as several rows), with a message.
#' @param units Character. Area units: `"ha"` (default), `"km2"` or `"m2"`.
#' @param national_label Character. Region name used for the national summary.
#'
#' @return A tibble with one row per region and action (including no-action
#'   values), and columns:
#' \describe{
#'   \item{region}{Region name, or `national_label`.}
#'   \item{action, group, colour, is_action}{From the action scheme.}
#'   \item{area}{Area assigned to the action, in `units`.}
#'   \item{area_in_pa, area_outside_pa}{The part of `area` inside and outside
#'     existing protected areas. `NA` if `x` has no protected-area layer.}
#'   \item{pct_of_region}{`area` as a percentage of the region's planning
#'     area: all cells with a value in the scheme, including no-action cells.
#'     Cells that are `NA` in the action raster (outside the planning units)
#'     are not counted.}
#'   \item{pct_of_action}{`area` as a percentage of the action's total area
#'     across all regions, i.e. how an action is distributed between
#'     regions.}
#' }
#' The planning area of each region is stored in the `region_area`
#' attribute.
#'
#' @seealso [elsar_protection_gap()], [elsar_plot_action_bars()], [elsar_export_results()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#'
#' # National totals
#' elsar_summarise_actions(x)
#'
#' # By region, in km2
#' elsar_summarise_actions(x, ex$regions, region_col = "region_name", units = "km2")
elsar_summarise_actions <- function(x,
                                    regions = NULL,
                                    region_col = NULL,
                                    units = c("ha", "km2", "m2"),
                                    national_label = "National") {
  scheme <- .scheme_from_actions(x)
  units <- match.arg(units)
  unit_factor <- c(m2 = 1, ha = 1e-4, km2 = 1e-6)[[units]]
  has_pa <- "pa_fraction" %in% names(x)

  # Per-cell weights: area and protected area (m2)
  cell_area <- .cell_area(x)
  pa_area <- if (has_pa) cell_area * x[["pa_fraction"]] else cell_area * 0
  weights <- c(cell_area, pa_area)
  names(weights) <- c("area", "pa_area")

  zs <- .action_zonal_sums(x, weights, regions, region_col, national_label)
  sums <- zs$sums
  region_names <- zs$region_names

  # One row per region x scheme value, zero-filled
  grid <- expand.grid(region = region_names, value = scheme$value,
                      stringsAsFactors = FALSE)
  out <- dplyr::left_join(grid, sums, by = c("region", "value")) |>
    dplyr::mutate(
      area = dplyr::coalesce(.data$area, 0) * unit_factor,
      pa_area = dplyr::coalesce(.data$pa_area, 0) * unit_factor
    ) |>
    dplyr::left_join(scheme, by = "value") |>
    dplyr::group_by(.data$region) |>
    dplyr::mutate(region_area = sum(.data$area)) |>
    dplyr::group_by(.data$value) |>
    dplyr::mutate(action_total = sum(.data$area)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      region = factor(.data$region, levels = region_names),
      action = factor(.data$label, levels = scheme$label),
      area_in_pa = if (has_pa) .data$pa_area else NA_real_,
      area_outside_pa = if (has_pa) .data$area - .data$pa_area else NA_real_,
      pct_of_region = .safe_pct(.data$area, .data$region_area),
      pct_of_action = .safe_pct(.data$area, .data$action_total)
    ) |>
    dplyr::arrange(.data$region, .data$action)

  region_area <- out |>
    dplyr::distinct(.data$region, .data$region_area)

  out <- out |>
    dplyr::select(
      "region", "action", "group", "colour", "is_action",
      "area", "area_in_pa", "area_outside_pa", "pct_of_region", "pct_of_action"
    ) |>
    tibble::as_tibble()

  attr(out, "units") <- units
  attr(out, "region_area") <- region_area
  attr(out, "has_pa") <- has_pa
  out
}

#' Compare current and proposed protection against a target
#'
#' From an action summary, calculates for each region the area already in
#' protected areas, the new protection ELSA proposes, and how far the total
#' falls short of an area-based target such as Target 3 of the
#' Kunming-Montreal Global Biodiversity Framework (30%).
#'
#' *Current protection* is all area inside existing protected areas, whatever
#' action ELSA assigned to it. *New protection* is the area of the protection
#' action(s) outside existing protected areas. When protected areas are
#' locked into Protect (the ELSA default), these are simply Protect inside and
#' outside PAs.
#'
#' @param summary A tibble from [elsar_summarise_actions()] computed on an
#'   action map with a protected-area layer.
#' @param target Numeric between 0 and 1. The share of each region's planning
#'   area to compare against. Default `0.30`. Use `NULL` for no target.
#' @param protect_actions Character vector. The action label(s) that count as
#'   new protection. Default `"Protect"`.
#' @param include_national Logical. If `TRUE` (default) and `summary` has
#'   several regions, adds a row that totals them, named `national_label`.
#' @param national_label Character. Name of the national row.
#'
#' @return A tibble with one row per region and columns `region`,
#'   `region_area`, `current`, `new`, `total` (areas in the summary's units),
#'   `pct_current`, `pct_new`, `pct_total`, and, if `target` is set,
#'   `target_pct`, `gap` (area still needed to reach the target, never below
#'   0) and `target_met`.
#'
#' @seealso [elsar_plot_protection_gap()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
#' elsar_protection_gap(s)
elsar_protection_gap <- function(summary,
                                 target = 0.30,
                                 protect_actions = "Protect",
                                 include_national = TRUE,
                                 national_label = "National") {
  .check_summary(summary)
  assertthat::assert_that(
    isTRUE(attr(summary, "has_pa")),
    msg = paste(
      "The summary has no protected-area information.",
      "Pass 'pas' to elsar_prepare_actions() before summarising."
    )
  )
  assertthat::assert_that(
    all(protect_actions %in% levels(summary$action)),
    msg = paste0(
      "'protect_actions' not found among the actions: ",
      toString(setdiff(protect_actions, levels(summary$action))), "."
    )
  )
  if (!is.null(target)) {
    assertthat::assert_that(
      is.numeric(target), length(target) == 1, target >= 0, target <= 1,
      msg = "'target' must be NULL or a single number between 0 and 1."
    )
  }

  out <- summary |>
    dplyr::group_by(.data$region) |>
    dplyr::summarise(
      region_area = sum(.data$area),
      current = sum(.data$area_in_pa),
      new = sum(.data$area_outside_pa[.data$action %in% protect_actions]),
      .groups = "drop"
    )

  if (include_national && nrow(out) > 1) {
    national <- out |>
      dplyr::summarise(
        region = national_label,
        dplyr::across(c("region_area", "current", "new"), sum)
      )
    out <- dplyr::bind_rows(
      dplyr::mutate(out, region = as.character(.data$region)),
      national
    ) |>
      dplyr::mutate(region = factor(.data$region, levels = .data$region))
  }

  out <- out |>
    dplyr::mutate(
      total = .data$current + .data$new,
      pct_current = .safe_pct(.data$current, .data$region_area),
      pct_new = .safe_pct(.data$new, .data$region_area),
      pct_total = .safe_pct(.data$total, .data$region_area)
    )

  if (!is.null(target)) {
    out <- out |>
      dplyr::mutate(
        target_pct = 100 * target,
        gap = pmax(0, target * .data$region_area - .data$total),
        target_met = .data$total >= target * .data$region_area
      )
  }

  attr(out, "units") <- attr(summary, "units")
  out
}

#' Load the example ELSA results
#'
#' Loads a small synthetic ELSA result for the fictional country "Elsaland":
#' an action map in which existing protected areas are locked into Protect,
#' the protected areas as fractional cover, and three admin regions. Use it to
#' try the ELSA results functions. See `data-raw/example_results.R` in the
#' package source for how it was made.
#'
#' @return A list with:
#' \describe{
#'   \item{actions}{`SpatRaster` of action values 1-5 in the default
#'     [elsar_action_scheme()], at 1 km resolution in World Mollweide.}
#'   \item{pas}{`SpatRaster` of protected-area cover (0-1).}
#'   \item{regions}{`sf` with columns `region_id` and `region_name`.}
#' }
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' terra::plot(ex$actions)
elsar_example_results <- function() {
  path <- function(f) {
    p <- system.file("extdata", f, package = "elsar")
    assertthat::assert_that(nzchar(p), msg = paste0("Example file '", f, "' not found."))
    p
  }
  list(
    actions = terra::rast(path("example_elsa_actions.tif")),
    pas = terra::rast(path("example_elsa_pas.tif")),
    regions = sf::st_read(path("example_elsa_regions.gpkg"), quiet = TRUE)
  )
}

# Internal helpers ---------------------------------------------------------------

.read_raster_arg <- function(x, arg) {
  if (is.character(x)) {
    assertthat::assert_that(
      length(x) == 1, file.exists(x),
      msg = paste0("'", arg, "' file not found: ", x[1])
    )
    x <- terra::rast(x)
  }
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    msg = paste0("'", arg, "' must be a SpatRaster or a path to a raster file.")
  )
  x
}

.check_scheme <- function(scheme) {
  needed <- c("value", "label", "colour", "group", "is_action")
  assertthat::assert_that(
    is.data.frame(scheme), all(needed %in% names(scheme)),
    msg = "'scheme' must be created with elsar_action_scheme()."
  )
  invisible(TRUE)
}

# Recover the scheme from a prepared action map
.scheme_from_actions <- function(x) {
  assertthat::assert_that(
    inherits(x, "SpatRaster"), "action" %in% names(x),
    msg = "'x' must be a prepared action map from elsar_prepare_actions()."
  )
  lv <- terra::cats(x[["action"]])[[1]]
  assertthat::assert_that(
    is.data.frame(lv),
    all(c("label", "colour", "group", "is_action") %in% names(lv)),
    msg = "'x' must be a prepared action map from elsar_prepare_actions()."
  )
  names(lv)[1] <- "value"
  tibble::as_tibble(lv[, c("value", "label", "colour", "group", "is_action")]) |>
    dplyr::mutate(is_action = as.logical(.data$is_action))
}

.check_summary <- function(summary) {
  needed <- c("region", "action", "area", "area_in_pa", "area_outside_pa")
  assertthat::assert_that(
    is.data.frame(summary), all(needed %in% names(summary)),
    msg = "'summary' must be created with elsar_summarise_actions()."
  )
  invisible(TRUE)
}

.prepare_regions <- function(regions, region_col, x) {
  if (inherits(regions, "SpatVector")) regions <- sf::st_as_sf(regions)
  assertthat::assert_that(
    inherits(regions, "sf"),
    msg = "'regions' must be an sf or SpatVector object."
  )
  assertthat::assert_that(
    is.character(region_col), length(region_col) == 1,
    region_col %in% names(regions),
    msg = "'region_col' must name a column in 'regions'."
  )
  assertthat::assert_that(
    !anyNA(regions[[region_col]]),
    msg = paste0("'", region_col, "' contains missing values.")
  )
  regions[[region_col]] <- as.character(regions[[region_col]])
  dups <- unique(regions[[region_col]][duplicated(regions[[region_col]])])
  if (length(dups) > 0) {
    message(
      "Rows sharing a region name are combined: ", toString(dups),
      ". Use a unique ID column as 'region_col' if they are different regions."
    )
  }
  if (sf::st_crs(regions) != sf::st_crs(terra::crs(x))) {
    regions <- sf::st_transform(regions, terra::crs(x))
  }
  regions[, region_col]
}

# Convert any accepted PA input into a fractional cover raster on the action grid
.pa_to_fraction <- function(pas, template) {
  if (inherits(pas, "SpatVector")) pas <- sf::st_as_sf(pas)
  if (inherits(pas, c("sf", "sfc"))) {
    pas <- sf::st_as_sf(pas)
    if (nrow(pas) == 0) {
      return(terra::ifel(is.na(template), NA, 0))
    }
    pas <- sf::st_transform(pas, terra::crs(template))
    pas <- sf::st_sf(geometry = sf::st_union(sf::st_make_valid(pas)))
    return(exactextractr::coverage_fraction(template, pas)[[1]])
  }

  pa <- .read_raster_arg(pas, "pas")[[1]]
  rng <- terra::minmax(pa, compute = TRUE)
  assertthat::assert_that(
    rng[1] >= 0, rng[2] <= 1,
    msg = paste(
      "Protected-area raster values must be between 0 and 1 (fractional cover).",
      "Divide percentages by 100 first."
    )
  )
  if (!terra::compareGeom(pa, template, stopOnError = FALSE)) {
    message("Resampling protected areas to the action raster grid.")
    pa <- terra::project(pa, template, method = "average")
  }
  terra::ifel(is.na(pa), 0, pa)
}


# Planar cell areas (m2) for projected rasters (ELSA uses equal-area
# projections, so a 1 km cell is exactly 100 ha); geodesic for lon/lat.
.cell_area <- function(x) {
  terra::cellSize(x[["action"]], unit = "m", mask = TRUE, transform = FALSE)
}

# Sum each weight layer by action value, nationally or per region. Cells cut by
# a region boundary are weighted by their coverage fraction. Returns a list with
# `sums` (region, value and one column per weight layer) and `region_names`.
.action_zonal_sums <- function(x, weights, regions, region_col, national_label) {
  weight_names <- names(weights)
  action <- x[["action"]]
  levels(action) <- NULL
  names(action) <- ".action"

  if (is.null(regions)) {
    sums <- terra::zonal(weights, action, fun = "sum", na.rm = TRUE)
    names(sums) <- c("value", weight_names)
    sums$region <- national_label
    return(list(sums = sums, region_names = national_label))
  }

  regions <- .prepare_regions(regions, region_col, x)
  region_names <- unique(regions[[region_col]])
  per_region <- exactextractr::exact_extract(
    c(action, weights), regions,
    fun = function(df) {
      # With summarize_df, include_cols arrive as columns of df
      df <- df[!is.na(df$.action), , drop = FALSE]
      if (nrow(df) == 0) {
        return(NULL)
      }
      w <- df$coverage_fraction
      vals <- lapply(df[weight_names], function(v) dplyr::coalesce(v, 0) * w)
      data.frame(region = df[[region_col]][1], value = df$.action, vals)
    },
    summarize_df = TRUE,
    include_cols = region_col,
    progress = FALSE
  )
  if (!all(c("region", "value") %in% names(per_region))) {
    # No region overlaps any action cell
    per_region <- data.frame(region = character(0), value = numeric(0))
    for (nm in weight_names) per_region[[nm]] <- numeric(0)
  }
  sums <- per_region |>
    dplyr::group_by(.data$region, .data$value) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(weight_names), sum), .groups = "drop")
  list(sums = as.data.frame(sums), region_names = region_names)
}

.safe_pct <- function(x, total) {
  # Recycle first: ifelse() returns a result as long as its test
  total <- rep_len(total, length(x))
  ifelse(total > 0, 100 * x / total, 0)
}

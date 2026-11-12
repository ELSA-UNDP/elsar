# Overlap of ELSA actions with other layers ----------------------------------------

#' Summarise how layers overlap ELSA actions
#'
#' Measures how much of each layer (e.g. Key Biodiversity Areas, indigenous and
#' community lands, ecosystem types, or any ELSA input feature) falls under each
#' action, nationally or per region, and how much of it lies inside existing
#' protected areas.
#'
#' @section Relation to ELSA representation outputs:
#' An ELSA run already reports national representation of the features it was
#' given, and those figures should be cited for the run itself. This function
#' complements them: it works from the action map alone (no prioritisation
#' problem needed), breaks results down by region, splits them by existing
#' protected areas, and accepts layers that were not used in the run, such as
#' policy overlays.
#'
#' @inheritParams elsar_summarise_actions
#' @param layers The layers to overlay, as one of:
#'   * a `SpatRaster` (or path) with one or more named numeric layers, e.g.
#'     the feature stack from [get_all_features()] or a layer from
#'     [make_kbas()]. Categorical rasters (e.g. ecosystem types) must first be
#'     split into one 0/1 layer per class, e.g. with [terra::segregate()];
#'   * a named list of `SpatRaster`, `sf` or `SpatVector` objects. Polygons are
#'     converted to the share of each cell they cover.
#'
#'   Rasters on a different grid are resampled to the action grid.
#' @param measure How a layer's cell values are added up:
#'   * `"area"` (default): value x cell area. For cover fractions (0-1) this
#'     is the area of the layer; for normalised features it is an
#'     area-weighted amount. Reported in `units`.
#'   * `"sum"`: the plain sum of cell values, for counts such as population.
#'
#' @return A tibble with one row per region, layer and action, and columns
#'   `region`, `layer`, `action`, `group`, `colour`, `is_action`, `amount`,
#'   `amount_in_pa`, `amount_outside_pa` (`NA` without a protected-area layer)
#'   and `pct_of_layer`: the share of the layer's amount within the region's
#'   planning area that falls under the action. Amounts outside the planning
#'   area (`NA` cells of the action map) are not counted.
#'
#' @seealso [elsar_plot_overlap()], [elsar_summarise_actions()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#'
#' # A made-up Key Biodiversity Area
#' kba <- sf::st_buffer(sf::st_sfc(sf::st_point(c(40000, 30000)), crs = terra::crs(x)), 9000)
#' ov <- elsar_summarise_overlap(x, list(KBA = sf::st_sf(geometry = kba)))
#' ov[, c("layer", "action", "amount", "pct_of_layer")]
elsar_summarise_overlap <- function(x,
                                    layers,
                                    regions = NULL,
                                    region_col = NULL,
                                    measure = c("area", "sum"),
                                    units = c("ha", "km2", "m2"),
                                    national_label = .txt("national")) {
  scheme <- .scheme_from_actions(x)
  measure <- match.arg(measure)
  units <- match.arg(units)
  unit_factor <- if (measure == "area") c(m2 = 1, ha = 1e-4, km2 = 1e-6)[[units]] else 1
  has_pa <- "pa_fraction" %in% names(x)

  layers <- .overlay_layers(layers, x, measure)
  layer_names <- names(layers)

  cell_area <- .cell_area(x)
  amount <- if (measure == "area") layers * cell_area else layers
  amount <- terra::mask(amount, x[["action"]])
  ids <- paste0(".l", seq_along(layer_names))
  names(amount) <- ids
  weights <- amount
  if (has_pa) {
    in_pa <- amount * x[["pa_fraction"]]
    names(in_pa) <- paste0(ids, "_pa")
    weights <- c(amount, in_pa)
  }

  zs <- .action_zonal_sums(x, weights, regions, region_col, national_label)

  grid <- expand.grid(region = zs$region_names, value = scheme$value, stringsAsFactors = FALSE)
  wide <- dplyr::left_join(grid, zs$sums, by = c("region", "value"))

  out <- lapply(seq_along(ids), function(i) {
    a <- dplyr::coalesce(wide[[ids[i]]], 0) * unit_factor
    p <- if (has_pa) dplyr::coalesce(wide[[paste0(ids[i], "_pa")]], 0) * unit_factor else NA_real_
    data.frame(region = wide$region, value = wide$value, layer = layer_names[i],
               amount = a, amount_in_pa = p)
  })
  out <- do.call(rbind, out) |>
    dplyr::left_join(scheme, by = "value") |>
    dplyr::group_by(.data$region, .data$layer) |>
    dplyr::mutate(pct_of_layer = .safe_pct(.data$amount, sum(.data$amount))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      region = factor(.data$region, levels = zs$region_names),
      layer = factor(.data$layer, levels = layer_names),
      action = factor(.data$label, levels = scheme$label),
      amount_outside_pa = .data$amount - .data$amount_in_pa
    ) |>
    dplyr::arrange(.data$region, .data$layer, .data$action) |>
    dplyr::select(
      "region", "layer", "action", "group", "colour", "is_action",
      "amount", "amount_in_pa", "amount_outside_pa", "pct_of_layer"
    ) |>
    tibble::as_tibble()

  attr(out, "measure") <- measure
  attr(out, "units") <- if (measure == "area") units else NA_character_
  attr(out, "has_pa") <- has_pa
  out
}

#' Plot how layers overlap ELSA actions
#'
#' Draws the output of [elsar_summarise_overlap()] as one 100% bar per layer,
#' split by the share of the layer under each action.
#'
#' @param overlap A tibble from [elsar_summarise_overlap()].
#' @param show_no_action Logical. Include the share under no action, so bars
#'   reach 100%. Default `TRUE`.
#' @param orientation `"vertical"` (default) or `"horizontal"` bars.
#' @param title Optional plot title.
#' @param base_size Base font size in points.
#'
#' @return A `ggplot` object, with a panel per region if there are several.
#' @seealso [elsar_summarise_overlap()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' layers <- list(
#'   "Existing PAs" = ex$pas,
#'   "Eastern wetland" = sf::st_sf(geometry = sf::st_buffer(
#'     sf::st_sfc(sf::st_point(c(65000, 15000)), crs = terra::crs(x)), 8000
#'   ))
#' )
#' elsar_plot_overlap(elsar_summarise_overlap(x, layers))
elsar_plot_overlap <- function(overlap,
                               show_no_action = TRUE,
                               orientation = c("vertical", "horizontal"),
                               title = NULL,
                               base_size = 10) {
  orientation <- match.arg(orientation)
  vertical <- orientation == "vertical"
  needed <- c("region", "layer", "action", "colour", "is_action", "pct_of_layer")
  assertthat::assert_that(
    is.data.frame(overlap), all(needed %in% names(overlap)),
    msg = "'overlap' must be created with elsar_summarise_overlap()."
  )
  d <- if (show_no_action) overlap else overlap[overlap$is_action, ]
  sch <- unique(d[, c("action", "colour")])
  sch <- sch[order(as.integer(sch$action)), ]
  colours <- stats::setNames(sch$colour, as.character(sch$action))
  d$action <- factor(as.character(d$action), levels = names(colours))

  layer_levels <- levels(d$layer)
  d$layer <- factor(as.character(d$layer), levels = if (vertical) layer_levels else rev(layer_levels))
  mapping <- if (vertical) {
    ggplot2::aes(x = .data$layer, y = .data$pct_of_layer, fill = .data$action)
  } else {
    ggplot2::aes(x = .data$pct_of_layer, y = .data$layer, fill = .data$action)
  }
  pct_scale <- (if (vertical) ggplot2::scale_y_continuous else ggplot2::scale_x_continuous)(
    labels = .label_pct(), breaks = seq(0, 100, 25),
    limits = c(0, 100 + 1e-6), expand = ggplot2::expansion(mult = c(0, 0.02))
  )
  value_title <- .txt("pct_of_layer")

  p <- ggplot2::ggplot(d, mapping) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width = 0.7, colour = "white", linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(values = colours, labels = .wrap(names(colours)), name = NULL) +
    .category_scale(vertical) +
    pct_scale +
    ggplot2::labs(title = title) +
    (if (vertical) ggplot2::labs(x = NULL, y = value_title) else ggplot2::labs(x = value_title, y = NULL)) +
    .results_theme(base_size) +
    .category_axis_theme(vertical, length(layer_levels), layer_levels) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  if (nlevels(droplevels(d$region)) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$region))
  }
  p
}

# Turn any accepted `layers` input into a named SpatRaster on the action grid
.overlay_layers <- function(layers, x, measure) {
  template <- x[["action"]]
  if (is.character(layers) || inherits(layers, "SpatRaster")) {
    r <- .read_raster_arg(layers, "layers")
    layers <- as.list(r)
    names(layers) <- names(r)
  }
  assertthat::assert_that(
    is.list(layers), length(layers) > 0,
    !is.null(names(layers)), all(nzchar(names(layers))), !anyDuplicated(names(layers)),
    msg = "'layers' must be a SpatRaster or a named list with unique names."
  )

  method <- if (measure == "sum") "sum" else "average"
  out <- lapply(names(layers), function(nm) {
    l <- layers[[nm]]
    if (inherits(l, "SpatVector")) l <- sf::st_as_sf(l)
    if (inherits(l, c("sf", "sfc"))) {
      l <- sf::st_as_sf(l)
      if (nrow(l) == 0) return(terra::ifel(is.na(template), NA, 0))
      l <- sf::st_transform(l, terra::crs(template))
      l <- sf::st_sf(geometry = sf::st_union(sf::st_make_valid(l)))
      return(exactextractr::coverage_fraction(template, l)[[1]])
    }
    assertthat::assert_that(
      inherits(l, "SpatRaster"),
      msg = paste0("Layer '", nm, "' must be a SpatRaster, sf or SpatVector.")
    )
    l <- l[[1]]
    assertthat::assert_that(
      !terra::is.factor(l),
      msg = paste0(
        "Layer '", nm, "' is categorical; adding up class codes is meaningless. ",
        "Split it into one 0/1 layer per class first, e.g. with terra::segregate()."
      )
    )
    if (!terra::compareGeom(l, template, stopOnError = FALSE)) {
      message("Resampling layer '", nm, "' to the action raster grid.")
      l <- terra::project(l, template, method = method)
    }
    rng <- terra::minmax(l, compute = TRUE)
    if (!is.na(rng[1]) && rng[1] < 0) {
      warning("Layer '", nm, "' has negative values; amounts may be misleading.", call. = FALSE)
    }
    l
  })
  out <- terra::rast(out)
  names(out) <- names(layers)
  out
}

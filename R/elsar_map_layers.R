# Map furniture and context layers of the elsar style.

#' Default attribution of elsar maps
#'
#' @description
#' `elsar_attribution()` builds the attribution line used as the caption of
#' elsar maps. Add it with `ggplot2::labs(caption = elsar_attribution())` or
#' use [elsar_map_caption()].
#'
#' @param source Character, optional. The data source(s), shown before the
#'   attribution as "Data: ...".
#' @param text Character. The attribution text. Default
#'   `"UNBL | www.unbiodiversitylab.org"`.
#'
#' @return A single character string.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' elsar_attribution()
#' elsar_attribution(source = "WAD, JRC (2018)")
elsar_attribution <- function(source = NULL, text = "UNBL | www.unbiodiversitylab.org") {
  assertthat::assert_that(is.null(source) || is.character(source),
    msg = "'source' must be NULL or a character string."
  )
  assertthat::assert_that(is.character(text), length(text) == 1,
    msg = "'text' must be a single character string."
  )
  parts <- c(
    if (!is.null(source) && any(nzchar(source))) paste0("Data: ", paste(source, collapse = "; ")),
    if (nzchar(text)) text
  )
  paste(parts, collapse = "  \u00b7  ")
}

#' Attribution caption for elsar maps
#'
#' @description
#' `elsar_map_caption()` adds the attribution as the plot caption: small,
#' muted text under the map, aligned to the plot edge. It replaces the boxed
#' text label drawn on top of the data by the classic style.
#'
#' @inheritParams elsar_attribution
#' @param position Character. `"right"` (default) or `"left"`.
#'
#' @return A list of ggplot2 components to add with `+`.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = boundary_dat) +
#'   elsar_map_caption(source = "UN Geospatial") +
#'   elsar_theme_map()
elsar_map_caption <- function(source = NULL,
                              text = "UNBL | www.unbiodiversitylab.org",
                              position = c("right", "left")) {
  position <- match.arg(position)
  list(
    ggplot2::labs(caption = elsar_attribution(source = source, text = text)),
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = if (position == "left") 0 else 1))
  )
}

#' Scale bar and north arrow in the elsar style
#'
#' @description
#' `elsar_map_furniture()` adds a compact scale bar (thin ticked line,
#' muted label) and a small minimal north arrow, both in neutral inks so they
#' never compete with the data. Uses \pkg{ggspatial}; the plot needs a
#' [ggplot2::coord_sf()] (added automatically by `geom_sf()` and
#' `tidyterra::geom_spatraster()`).
#'
#' @param scale_bar Logical. Add a scale bar. Default `TRUE`.
#' @param north_arrow Logical. Add a north arrow. Default `TRUE`.
#' @param scale_location,arrow_location Character. Corner for the scale bar
#'   (default `"bl"`, bottom left) and north arrow (default `"tr"`, top
#'   right): `"bl"`, `"br"`, `"tl"` or `"tr"`.
#' @param base_size Numeric. Base font size of the plot, used to size the
#'   labels. Default `10`.
#' @param base_family Character. Font family, as in [elsar_theme_map()].
#' @param dark Logical. Use the dark-mode inks.
#' @param width_hint Numeric (0-1). Approximate scale bar length as a
#'   fraction of the panel width. Default `0.18`.
#'
#' @return A list of ggplot2 layers to add with `+`.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = boundary_dat) +
#'   elsar_map_furniture() +
#'   elsar_theme_map()
elsar_map_furniture <- function(scale_bar = TRUE,
                                north_arrow = TRUE,
                                scale_location = "bl",
                                arrow_location = "tr",
                                base_size = 10,
                                base_family = "",
                                dark = FALSE,
                                width_hint = 0.18) {
  corners <- c("bl", "br", "tl", "tr")
  assertthat::assert_that(
    assertthat::is.flag(scale_bar), assertthat::is.flag(north_arrow),
    msg = "'scale_bar' and 'north_arrow' must be TRUE or FALSE."
  )
  assertthat::assert_that(
    scale_location %in% corners, arrow_location %in% corners,
    msg = "'scale_location' and 'arrow_location' must be one of 'bl', 'br', 'tl', 'tr'."
  )
  ink <- elsar_inks(dark)
  family <- .elsar_font(base_family)
  pad <- grid::unit(base_size * 0.8, "pt")
  out <- list()

  if (scale_bar) {
    out <- c(out, list(ggspatial::annotation_scale(
      location = scale_location,
      style = "ticks",
      width_hint = width_hint,
      line_col = ink[["ink_secondary"]],
      text_col = ink[["ink_secondary"]],
      line_width = 0.6,
      tick_height = 0.5,
      height = grid::unit(base_size * 0.45, "pt"),
      text_cex = base_size * 0.072,
      text_family = family,
      text_pad = grid::unit(base_size * 0.25, "pt"),
      pad_x = pad,
      pad_y = pad
    )))
  }
  if (north_arrow) {
    out <- c(out, list(ggspatial::annotation_north_arrow(
      location = arrow_location,
      which_north = "grid",
      height = grid::unit(base_size * 2.8, "pt"),
      width = grid::unit(base_size * 1.6, "pt"),
      pad_x = pad,
      pad_y = pad,
      style = ggspatial::north_arrow_minimal(
        line_width = 0.9,
        line_col = ink[["ink_secondary"]],
        fill = ink[["ink_secondary"]],
        text_col = ink[["ink_secondary"]],
        text_family = family,
        text_size = base_size * 0.8
      )
    )))
  }
  out
}

#' Context layers for elsar maps
#'
#' @description
#' `elsar_map_context()` draws the setting a map's data sits in: optional
#' neighbouring land in a muted fill, a soft drop shadow and halo around the
#' study area, and the study area itself in a light land tone (visible
#' wherever the data are missing). Add it *before* the data layer.
#'
#' `elsar_map_outline()` draws a crisp, thin outline of the study area. Add
#' it *after* the data layer.
#'
#' @param x The study area: an `sf`/`sfc` object, a `SpatVector`, or a
#'   `SpatRaster` (its non-missing cells are outlined).
#' @param neighbours Optional `sf`/`sfc` or `SpatVector` of surrounding land
#'   (e.g. neighbouring countries), drawn behind everything else.
#' @param halo Logical. Draw a soft halo around the study area. Default
#'   `TRUE`.
#' @param shadow Logical. Draw a soft drop shadow under the study area.
#'   Default `FALSE`.
#' @param fill Fill colour of the study area. Default: the `land` ink from
#'   [elsar_inks()].
#' @param dark Logical. Use the dark-mode inks.
#' @param colour Outline colour. Default: the `ink_secondary` ink.
#' @param linewidth Outline width in mm. Default `0.3`.
#'
#' @return A list of ggplot2 layers to add with `+`.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' npl <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
#' ggplot2::ggplot() +
#'   elsar_map_context(npl, neighbours = boundary_dat, shadow = TRUE) +
#'   elsar_map_outline(npl) +
#'   ggplot2::coord_sf(xlim = c(79, 89.5), ylim = c(25.5, 31), datum = NA) +
#'   elsar_theme_map()
elsar_map_context <- function(x,
                              neighbours = NULL,
                              halo = TRUE,
                              shadow = FALSE,
                              fill = NULL,
                              dark = FALSE) {
  assertthat::assert_that(
    assertthat::is.flag(halo), assertthat::is.flag(shadow),
    msg = "'halo' and 'shadow' must be TRUE or FALSE."
  )
  ink <- elsar_inks(dark)
  area <- .as_outline_sf(x)
  fill <- fill %||% ink[["land"]]
  out <- list()

  if (!is.null(neighbours)) {
    nb <- .as_sf(neighbours, "neighbours")
    nb <- sf::st_transform(nb, sf::st_crs(area))
    out <- c(out, list(ggplot2::geom_sf(
      data = nb, inherit.aes = FALSE, fill = ink[["land"]],
      colour = ink[["surface"]], linewidth = 0.35
    )))
  }

  if (shadow) {
    bb <- sf::st_bbox(area)
    step <- 0.0035 * max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
    geom <- sf::st_geometry(area)
    for (i in 3:1) {
      shifted <- sf::st_sfc(geom + c(step * i * 0.6, -step * i), crs = sf::st_crs(area))
      out <- c(out, list(ggplot2::geom_sf(
        data = sf::st_sf(geometry = shifted), inherit.aes = FALSE,
        fill = scales::alpha(ink[["shadow"]], 0.07), colour = NA
      )))
    }
  }

  if (halo) {
    out <- c(out, list(
      ggplot2::geom_sf(
        data = area, inherit.aes = FALSE, fill = NA,
        colour = scales::alpha(ink[["halo"]], 0.45), linewidth = 3.2
      ),
      ggplot2::geom_sf(
        data = area, inherit.aes = FALSE, fill = NA,
        colour = scales::alpha(ink[["halo"]], 0.9), linewidth = 1.4
      )
    ))
  }

  c(out, list(ggplot2::geom_sf(
    data = area, inherit.aes = FALSE, fill = fill, colour = NA
  )))
}

#' @rdname elsar_map_context
#' @export
elsar_map_outline <- function(x, colour = NULL, linewidth = 0.3, dark = FALSE) {
  assertthat::assert_that(assertthat::is.number(linewidth), linewidth >= 0,
    msg = "'linewidth' must be a non-negative number."
  )
  colour <- colour %||% elsar_inks(dark)[["ink_secondary"]]
  ggplot2::geom_sf(
    data = .as_outline_sf(x), inherit.aes = FALSE, fill = NA,
    colour = colour, linewidth = linewidth
  )
}

#' Inset locator map
#'
#' @description
#' `elsar_map_locator()` draws a small locator map: the surrounding region in
#' a muted land tone with the study area picked out in dark ink.
#' `elsar_add_locator()` places it in a corner of a map. Positioning uses
#' the \pkg{patchwork} package, which must be installed.
#'
#' @param focus The study area (`sf`/`sfc`, `SpatVector` or `SpatRaster`).
#' @param context `sf`/`sfc` or `SpatVector` of the surrounding region, e.g.
#'   neighbouring countries. Its CRS is used for the locator.
#' @param dark Logical. Use the dark-mode inks.
#' @param plot A ggplot2 map.
#' @param locator The locator map from `elsar_map_locator()`.
#' @param location Character. Corner for the inset: `"tr"` (default), `"tl"`,
#'   `"br"` or `"bl"`.
#' @param size Numeric (0-1). Inset width and height as a fraction of the
#'   map panel. Default `0.28`.
#'
#' @return `elsar_map_locator()` returns a ggplot; `elsar_add_locator()`
#'   returns a patchwork object, which prints and saves like a ggplot.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' npl <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
#' elsar_map_locator(npl, context = boundary_dat)
#'
#' \donttest{
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   map <- ggplot2::ggplot() +
#'     elsar_map_context(npl) +
#'     elsar_theme_map()
#'   elsar_add_locator(map, elsar_map_locator(npl, boundary_dat))
#' }
#' }
elsar_map_locator <- function(focus, context, dark = FALSE) {
  ink <- elsar_inks(dark)
  context <- .as_sf(context, "context")
  focus <- sf::st_transform(.as_outline_sf(focus), sf::st_crs(context))

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = context, inherit.aes = FALSE, fill = ink[["land"]],
      colour = ink[["land_line"]], linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data = focus, inherit.aes = FALSE, fill = ink[["ink_secondary"]],
      colour = NA
    ) +
    ggplot2::coord_sf(datum = NA, expand = TRUE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = ink[["surface"]], colour = ink[["rule"]], linewidth = 0.4
      ),
      plot.margin = ggplot2::margin(3, 3, 3, 3)
    )
}

#' @rdname elsar_map_locator
#' @export
elsar_add_locator <- function(plot, locator, location = "tr", size = 0.28) {
  rlang::check_installed("patchwork", reason = "to place an inset locator map.")
  assertthat::assert_that(location %in% c("bl", "br", "tl", "tr"),
    msg = "'location' must be one of 'bl', 'br', 'tl', 'tr'."
  )
  assertthat::assert_that(assertthat::is.number(size), size > 0, size < 1,
    msg = "'size' must be a number between 0 and 1."
  )
  pad <- 0.01
  left <- if (substr(location, 2, 2) == "l") pad else 1 - size - pad
  bottom <- if (substr(location, 1, 1) == "b") pad else 1 - size - pad
  plot + patchwork::inset_element(
    locator,
    left = left, bottom = bottom, right = left + size, top = bottom + size,
    align_to = "panel", clip = TRUE
  )
}

# Helpers ----------------------------------------------------------------------

.as_sf <- function(x, arg = "x") {
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  if (inherits(x, "sfc")) x <- sf::st_sf(geometry = x)
  assertthat::assert_that(inherits(x, "sf"),
    msg = paste0("'", arg, "' must be an sf, sfc or SpatVector object.")
  )
  x
}

# The outline of a study area as a single-feature sf object.
.as_outline_sf <- function(x) {
  if (inherits(x, "SpatRaster")) {
    r <- x[[1]]
    if (terra::ncell(r) > 4e6) {
      r <- terra::aggregate(r, fact = ceiling(sqrt(terra::ncell(r) / 4e6)), fun = "max", na.rm = TRUE)
    }
    mask <- terra::classify(!is.na(r), cbind(0, NA))
    x <- sf::st_as_sf(terra::as.polygons(mask, dissolve = TRUE))
  }
  x <- .as_sf(x, "x")
  geom <- sf::st_union(sf::st_make_valid(sf::st_geometry(x)))
  sf::st_sf(geometry = geom)
}

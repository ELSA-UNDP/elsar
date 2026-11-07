# Tests for the elsar plotting style API and the plotting functions that use it.

small_raster <- function() {
  r <- terra::rast(
    nrows = 20, ncols = 30, xmin = 0, xmax = 30000, ymin = 0, ymax = 20000,
    crs = "ESRI:54009"
  )
  terra::values(r) <- seq(0, 1, length.out = terra::ncell(r))
  r[1:15] <- NA
  names(r) <- "value"
  r
}

build_ok <- function(p) {
  expect_no_error(b <- ggplot2::ggplot_build(p))
  expect_no_error(ggplot2::ggplotGrob(p))
  invisible(b)
}

# Inks, palettes, themes ---------------------------------------------------------

test_that("elsar_inks() returns valid light and dark colours", {
  for (dark in c(FALSE, TRUE)) {
    ink <- elsar_inks(dark)
    expect_true(all(c("ink", "ink_secondary", "ink_muted", "surface", "land") %in% names(ink)))
    expect_no_error(grDevices::col2rgb(ink))
  }
  expect_false(identical(elsar_inks()[["surface"]], elsar_inks(TRUE)[["surface"]]))
})

test_that("elsar_palette() keeps the established ELSA action colours", {
  act <- elsar_palette("actions")
  expect_equal(
    unname(act[c("Protect", "Restore", "Manage", "Urban Greening")]),
    c("#06C723", "#8803FC", "#FFEA00", "#FF0000")
  )
  # Same colours and labels as the default action scheme
  scheme <- elsar_action_scheme()
  expect_equal(unname(act[scheme$label]), scheme$colour)
})

test_that("elsar_palette() returns the requested number of colours", {
  expect_length(elsar_palette("categorical", n = 3), 3)
  expect_length(elsar_palette("sequential", n = 9, option = "mako"), 9)
  expect_length(elsar_palette("diverging", n = 7), 7)
  expect_equal(elsar_palette("sequential", n = 5, direction = -1), rev(elsar_palette("sequential", n = 5)))
  expect_error(elsar_palette("categorical", n = 7), "Other")
  expect_error(elsar_palette("sequential", option = "turbo"), "viridis")
  expect_error(elsar_palette("nope"))
})

test_that("diverging palette has a neutral midpoint", {
  mid <- elsar_palette("diverging", n = 7)[4]
  rgb <- grDevices::col2rgb(mid)[, 1]
  expect_lt(max(rgb) - min(rgb), 10)
})

test_that("themes build and validate their arguments", {
  expect_s3_class(elsar_theme_map(), "theme")
  expect_s3_class(elsar_theme_map(dark = TRUE, legend_position = "right"), "theme")
  expect_s3_class(elsar_theme(grid = "xy"), "theme")
  expect_error(elsar_theme_map(legend_position = "middle"), "legend_position")
  expect_error(elsar_theme_map(base_size = -1), "base_size")
  expect_equal(elsar_theme_map()$plot.title.position, "plot")
  expect_s3_class(elsar_theme_map()$panel.border, "element_rect")
  expect_s3_class(elsar_theme_map(graticule = FALSE)$panel.border, "element_blank")
  expect_error(elsar_theme_map(graticule = NA), "graticule")
})

test_that("maps get a degree graticule, 0/1 legends and an auto legend position", {
  moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m"
  tall <- terra::rast(nrows = 40, ncols = 20, xmin = 0, xmax = 2e5, ymin = 0, ymax = 4e5, crs = moll)
  wide <- terra::rast(nrows = 20, ncols = 40, xmin = 0, xmax = 4e5, ymin = 0, ymax = 2e5, crs = moll)
  terra::values(tall) <- rep(c(0, 1), length.out = terra::ncell(tall))
  terra::values(wide) <- stats::runif(terra::ncell(wide))
  expect_equal(.map_legend_position(tall), "right")
  expect_equal(.map_legend_position(wide), "top")
  expect_equal(.map_legend_position(tall, "bottom"), "bottom")
  expect_true(.is_binary_raster(tall))
  expect_false(.is_binary_raster(wide))

  p <- suppressMessages(elsar_plot_static_raster(tall, data_type = "discrete", legend_title = "Zone"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$plot$coordinates$datum, sf::st_crs(4326))
  expect_equal(b$plot$theme$legend.position, "right")
  fill <- b$plot$scales$get_scales("fill")
  expect_equal(fill$get_labels(), c("Outside", "Inside"))

  p2 <- suppressMessages(elsar_plot_static_raster(wide, graticule = FALSE))
  expect_true(is.na(ggplot2::ggplot_build(p2)$plot$coordinates$datum))

  classic <- suppressMessages(elsar_plot_static_raster(tall, data_type = "discrete", style = "classic"))
  expect_setequal(levels(classic$data$interval %||% ggplot2::ggplot_build(classic)$plot$layers[[1]]$data$interval),
                  c("Outside", "Inside"))
})

test_that("unknown font families fall back to the default", {
  skip_if_not_installed("systemfonts")
  expect_warning(f <- .elsar_font("No Such Font Family 123"), "not available")
  expect_equal(f, "")
  expect_equal(.elsar_font(""), "")
})

test_that("scales and guides build in ggplot2 plots", {
  df <- data.frame(x = 1:6, y = 1:6, v = seq(-1, 1, length.out = 6), g = letters[1:6])
  base <- ggplot2::ggplot(df, ggplot2::aes(x, y))

  build_ok(base + ggplot2::geom_tile(ggplot2::aes(fill = v)) + scale_fill_elsar_c() + elsar_theme_map())
  build_ok(base + ggplot2::geom_point(ggplot2::aes(colour = v)) + scale_colour_elsar_c(palette = "diverging") +
    elsar_theme(legend_position = "right"))
  build_ok(base + ggplot2::geom_tile(ggplot2::aes(fill = g)) + scale_fill_elsar_d() + elsar_theme_map())
  build_ok(base + ggplot2::geom_tile(ggplot2::aes(fill = g)) + scale_fill_elsar_d(palette = "sequential") +
    elsar_theme_map())
  build_ok(base + ggplot2::geom_point(ggplot2::aes(colour = g)) + scale_colour_elsar_d(palette = "sequential"))

  # Diverging scale is centred on the midpoint
  centre <- ggplot2::get_layer_data(base + ggplot2::geom_tile(ggplot2::aes(fill = c(0, 0.5, 1, 0.2, 0.5, 2))) +
    scale_fill_elsar_c(palette = "diverging", midpoint = 0.5))$fill[2]
  expect_lt(diff(range(grDevices::col2rgb(centre)[, 1])), 10)

  acts <- data.frame(x = 1:3, y = 1, a = c("Protect", "Restore", "Manage"))
  d <- ggplot2::layer_data(ggplot2::ggplot(acts, ggplot2::aes(x, y, fill = a)) + ggplot2::geom_tile() +
    scale_fill_elsar_d(palette = "actions"))
  expect_equal(toupper(d$fill), c("#06C723", "#8803FC", "#FFEA00"))
})

test_that("guides work in both directions", {
  df <- data.frame(x = 1:3, y = 1:3, v = 1:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = v)) + ggplot2::geom_tile()
  build_ok(p + scale_fill_elsar_c(guide = guide_elsar_colourbar(direction = "vertical")))
  build_ok(p + scale_fill_elsar_c() + elsar_theme_map(legend_position = "right"))
  expect_s3_class(guide_elsar_steps(), "Guide")
  expect_error(guide_elsar_colourbar(length = -1), "positive")
})

# Map layers -----------------------------------------------------------------------

test_that("map furniture, caption and context layers build", {
  npl <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
  p <- ggplot2::ggplot() +
    elsar_map_context(npl, neighbours = boundary_dat, shadow = TRUE) +
    elsar_map_outline(npl) +
    elsar_map_furniture() +
    elsar_map_caption(source = "test") +
    elsar_theme_map()
  build_ok(p)
  expect_match(p$labels$caption %||% ggplot2::get_labs(p)$caption, "Data: test")

  # Context from a raster
  build_ok(ggplot2::ggplot() + elsar_map_context(small_raster(), dark = TRUE) + elsar_theme_map(dark = TRUE))
  expect_error(elsar_map_furniture(scale_location = "middle"))
})

test_that("elsar_attribution() composes source and text", {
  expect_equal(elsar_attribution(), "UNBL | www.unbiodiversitylab.org")
  expect_match(elsar_attribution(source = c("A", "B")), "^Data: A; B")
  expect_equal(elsar_attribution(text = ""), "")
})

test_that("locator map builds and can be inset", {
  npl <- boundary_dat[boundary_dat$iso3cd == "NPL", ]
  loc <- elsar_map_locator(npl, boundary_dat)
  build_ok(loc)
  skip_if_not_installed("patchwork")
  map <- ggplot2::ggplot() + elsar_map_context(npl) + elsar_theme_map()
  out <- elsar_add_locator(map, loc, location = "bl")
  expect_s3_class(out, "patchwork")
  expect_error(elsar_add_locator(map, loc, size = 2), "size")
})

# Plotting functions ---------------------------------------------------------------

test_that("elsar_plot_static_raster() builds in both styles", {
  r <- small_raster()
  bg <- terra::extend(r, 5)
  bg[is.na(bg)] <- 0.5
  boundary <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))

  for (style in c("modern", "classic")) {
    build_ok(elsar_plot_static_raster(r, legend_title = "v", style = style))
    build_ok(elsar_plot_static_raster(r, type = "ggplot_raster", background = bg, style = style))
    build_ok(elsar_plot_static_raster(r,
      data_type = "discrete", type = "ggplot_raster",
      number_categories = 4, data_layer = "value", style = style
    ))
  }
  build_ok(elsar_plot_static_raster(r, boundary = boundary, plot_title = "Title"))

  # raster_df_out still returns the plot and the data
  out <- elsar_plot_static_raster(r, data_type = "discrete", number_categories = 4, raster_df_out = TRUE)
  expect_length(out, 2)
  expect_true(all(c("x", "y", "interval") %in% names(out[[2]])))

  # custom palettes still replace the default scale
  p <- elsar_plot_static_raster(r, custom_palette = ggplot2::scale_fill_distiller(palette = "YlGnBu"))
  build_ok(p)

  expect_error(elsar_plot_static_raster(r, style = "retro"), "style")
})

test_that("the elsar.plot_style option switches the default style", {
  r <- small_raster()
  withr::local_options(elsar.plot_style = "classic")
  p <- elsar_plot_static_raster(r)
  # classic plots draw tiles, modern ones use tidyterra rasters
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomTile"), logical(1))))
})

test_that("deprecated static raster wrappers warn and still plot", {
  r <- small_raster()
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(p <- elsar_plot_static_raster_c(r), class = "lifecycle_warning_deprecated")
  build_ok(p)
  expect_warning(
    p <- elsar_plot_static_raster_d(r, number_categories = 3, data_layer = "value"),
    class = "lifecycle_warning_deprecated"
  )
  build_ok(p)
})

test_that("elsar_plot_optics() keeps its arguments working in both styles", {
  r <- small_raster()
  for (style in c("modern", "classic")) {
    for (loc in c("bottom_right", "bottom_left", "top_right", "top_left")) {
      p <- elsar_plot_static_raster(r, style = style) +
        elsar_plot_optics(include_north_scale = TRUE, text_location = loc, style = style)
      build_ok(p)
    }
    build_ok(elsar_plot_static_raster(r, style = style) +
      elsar_plot_optics(theme = list(ggplot2::theme_minimal()), include_text = FALSE, style = style))
  }
  modern <- elsar_plot_static_raster(r) + elsar_plot_optics()
  expect_equal(ggplot2::get_labs(modern)$caption, "UNBL | www.unbiodiversitylab.org")
  expect_false(any(vapply(modern$layers, function(l) inherits(l$geom, "GeomLabel"), logical(1))))
  classic <- elsar_plot_static_raster(r, style = "classic") + elsar_plot_optics(style = "classic")
  expect_true(any(vapply(classic$layers, function(l) inherits(l$geom, "GeomLabel"), logical(1))))
  expect_error(elsar_plot_optics(style = "retro"), "style")
})

test_that("elsar_plot_feature() builds in both styles", {
  r <- small_raster()
  pus <- r
  pus[!is.na(pus)] <- 1
  for (style in c("modern", "classic")) {
    build_ok(elsar_plot_feature(r, pus, legend_title = "v", style = style))
    build_ok(elsar_plot_feature(r, pus, invert_palette = TRUE, no_legend = TRUE, color_map = "mako", style = style))
  }
  dir <- withr::local_tempdir()
  elsar_plot_feature(r, pus, legend_title = "My Feature", figure_path = dir, iso3 = "TST", custom_resolution = 30)
  expect_true(file.exists(file.path(dir, "my_feature_tst.png")))
})

test_that("elsar_plot_distribution() builds in both styles", {
  r <- small_raster()
  df_cat <- elsar_plot_static_raster(r, data_type = "discrete", number_categories = 4, raster_df_out = TRUE)[[2]]
  df_con <- elsar_plot_static_raster(r, raster_df_out = TRUE)[[2]]
  for (style in c("modern", "classic")) {
    build_ok(elsar_plot_distribution(raster_df = df_cat, categorical = TRUE, style = style))
    suppressMessages(build_ok(elsar_plot_distribution(
      raster_df = df_con, categorical = FALSE, col_interest = "value", style = style
    )))
  }
  # col_interest for categorical data (previously failed)
  df_cat2 <- dplyr::rename(df_cat, cls = "interval")
  build_ok(elsar_plot_distribution(raster_df = df_cat2, categorical = TRUE, col_interest = "cls", style = "classic"))
})

test_that("elsar_plot_background() builds in both styles", {
  r <- small_raster()
  bg <- terra::extend(r, 5)
  bg[is.na(bg)] <- 0.5
  for (style in c("modern", "classic")) {
    out <- elsar_plot_background(background_dat = bg, main_data = r, style = style)
    build_ok(out[[1]])
    out <- elsar_plot_background(
      background_dat = bg, main_data = r, data_type = "discrete",
      number_categories = 3, data_layer = "value", style = style
    )
    build_ok(out[[1]])
  }
})

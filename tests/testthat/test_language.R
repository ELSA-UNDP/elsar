# Language of plot text: elsar_labels(), the elsar.language and elsar.labels
# options, and the defaults and plots that use them.

lang_crs <- "ESRI:54009"

lang_actions <- function() {
  terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4000, ymin = 0, ymax = 4000,
    crs = lang_crs, vals = c(1, 1, 2, 2, 1, 3, 2, 5, 5, 5, 4, 5, 5, NA, 5, 5)
  )
}

lang_pas <- function() {
  r <- lang_actions()
  terra::values(r) <- c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  r
}

lang_regions <- function() {
  sq <- function(x0, x1) {
    sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 4000), c(x0, 4000), c(x0, 0))))
  }
  sf::st_sf(name = c("West", "East"), geometry = sf::st_sfc(sq(0, 2000), sq(2000, 4000), crs = lang_crs))
}

value_title <- function(p) ggplot2::get_labs(p)$y

test_that("every language has the same keys, all non-empty", {
  keys <- names(elsar_labels("en"))
  for (lang in c("es", "fr", "pt", "ru")) {
    text <- elsar_labels(lang)
    expect_identical(names(text), keys)
    expect_true(all(nzchar(text)))
    # Action names must be unique, or schemes built from them fail
    expect_false(anyDuplicated(text[.action_keys]) > 0)
  }
})

test_that("elsar_labels() follows the elsar.language option, English by default", {
  withr::local_options(elsar.language = NULL, elsar.labels = NULL)
  expect_identical(elsar_labels()[["protect"]], "Protect")
  withr::local_options(elsar.language = "es")
  expect_identical(elsar_labels()[["protect"]], "Protección")
  withr::local_options(elsar.language = "ES")
  expect_identical(elsar_labels()[["national"]], "Nacional")
})

test_that("elsar_labels() rejects unknown languages and keys", {
  expect_error(elsar_labels("de"), "not supported")
  expect_error(elsar_labels(c("en", "es")), "single language code")
  withr::local_options(elsar.labels = c(not_a_key = "x"))
  expect_error(elsar_labels(), "Unknown key")
  withr::local_options(elsar.labels = "% de la provincia")
  expect_error(elsar_labels(), "named character vector")
})

test_that("elsar.labels overrides single pieces of text in any language", {
  withr::local_options(elsar.language = "es", elsar.labels = c(pct_of_region = "% de la provincia"))
  text <- elsar_labels()
  expect_identical(text[["pct_of_region"]], "% de la provincia")
  expect_identical(text[["area"]], "Superficie")
  # A list works too
  withr::local_options(elsar.labels = list(national = "Ecuador"))
  expect_identical(elsar_labels()[["national"]], "Ecuador")
})

test_that("default action scheme and palette follow the language", {
  withr::local_options(elsar.language = "es", elsar.labels = NULL)
  scheme <- elsar_action_scheme()
  expect_identical(scheme$label, unname(elsar_labels("es")[.action_keys]))
  expect_identical(scheme$is_action, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  pal <- elsar_palette("actions")
  expect_identical(names(pal), scheme$label)
  expect_identical(unname(pal), scheme$colour)
  # The action scale matches names in the current language and in English
  vals <- scale_fill_elsar_d(palette = "actions")$palette(10)
  expect_true(all(c("Protección", "Protect") %in% names(vals)))
})

test_that("scheme from an ELSA config uses the current language", {
  withr::local_options(elsar.language = "fr", elsar.labels = NULL)
  cfg <- list(actions = list(
    list(rasterValue = 1, slug = "protect", name = "Protect", color = "#06c723"),
    list(rasterValue = 2, slug = "reduce-pressures", name = "Reduce Pressures", color = "#ffea00")
  ))
  scheme <- elsar_action_scheme_from_config(cfg)
  expect_identical(scheme$label, c("Protection", "Gestion", "Aucune action"))
})

test_that("action labels match across languages and ELSA tool names", {
  expect_identical(
    .label_key(c("Protect", "Protección", "PROTECCIÓN", "Proteção",
                 "Охрана")),
    rep("protect", 5)
  )
  expect_identical(.label_key(c("Reduce Pressures", "Manejo", "Gestion")), rep("manage", 3))
  expect_identical(.label_key(c("Sin acción", "Do nothing", "No action")), rep("donothing", 3))
  # So an English-labelled ELSA raster fits a Spanish scheme without a warning
  withr::local_options(elsar.language = "es")
  raster_labels <- data.frame(value = 1:5, label = c("Protect", "Reduce Pressures", "Restore",
                                                     "Urban Adapt", "Do Nothing"))
  expect_no_warning(.check_scheme_fits(elsar_action_scheme(), 1:5, raster_labels))
})

test_that("summaries, protection gap and plots use the current language", {
  withr::local_options(elsar.language = "es", elsar.labels = c(pct_of_region = "% de la provincia"))
  x <- elsar_prepare_actions(lang_actions(), pas = lang_pas())
  s <- elsar_summarise_actions(x, lang_regions(), region_col = "name")
  g <- elsar_protection_gap(s)
  expect_identical(as.character(g$region), c("West", "East", "Nacional"))
  # The protect action is found by its Spanish name: same gap as in English
  g_en <- withr::with_options(list(elsar.language = "en"), {
    x_en <- elsar_prepare_actions(lang_actions(), pas = lang_pas())
    elsar_protection_gap(elsar_summarise_actions(x_en, lang_regions(), region_col = "name"))
  })
  cols <- c("current", "new", "pct_current", "pct_new", "pct_total")
  expect_equal(as.data.frame(g)[cols], as.data.frame(g_en)[cols])

  bars <- elsar_plot_action_bars(s)
  expect_identical(value_title(bars), "% de la provincia")
  expect_identical(value_title(elsar_plot_action_bars(s, metric = "area")), "Superficie (ha)")

  built <- ggplot2::ggplot_build(elsar_plot_action_bars(s))
  legend <- gsub("\n", " ", built$plot$scales$get_scales("fill")$get_labels())
  expect_true("Áreas protegidas existentes" %in% legend)

  gap_plot <- elsar_plot_protection_gap(g)
  gb <- ggplot2::ggplot_build(gap_plot)
  expect_identical(value_title(gap_plot), "% del área de planificación")
  sec <- gb$layout$panel_params[[1]]$y.sec$get_labels()
  expect_true(any(grepl("^Meta\n30 ?%$|^Meta\n30%$", sec)))
  expect_true(any(grepl(",", gb$data[[2]]$label)))

  # Maps get Spanish compass letters
  m <- elsar_plot_actions(x)
  expect_identical(m$scales$get_scales("x")$labels(c(-79.5, 0, 2)), c("79,5°O", "0°", "2°E"))
  expect_identical(m$scales$get_scales("y")$labels(c(-4, 1)), c("4°S", "1°N"))
  expect_no_error(suppressMessages(ggplot2::ggplotGrob(m)))

  # Regional panel builds, with the overridden axis title
  p <- elsar_plot_region(x, s, lang_regions(), "name", "West")
  expect_no_error(suppressMessages(patchwork::patchworkGrob(p)))
  expect_identical(value_title(p[[2]][[1]]), "% de la provincia")
})

test_that("number labels use the language's marks", {
  withr::local_options(elsar.language = "es", elsar.labels = NULL)
  expect_identical(.label_area()(12345), "12.345")
  expect_identical(.label_pct(accuracy = 0.1)(12.34), "12,3%")
  withr::local_options(elsar.language = "fr")
  expect_identical(.label_area()(12345), "12 345")
  withr::local_options(elsar.language = "en")
  expect_identical(.label_area()(12345), "12K")
})

test_that("English output is unchanged", {
  withr::local_options(elsar.language = NULL, elsar.labels = NULL)
  x <- elsar_prepare_actions(lang_actions(), pas = lang_pas())
  s <- elsar_summarise_actions(x, lang_regions(), region_col = "name")
  expect_identical(levels(s$action), c("Protect", "Manage", "Restore", "Urban Greening", "Do Nothing"))
  expect_identical(value_title(elsar_plot_action_bars(s)), "% of region")
  expect_identical(value_title(elsar_plot_action_bars(s, metric = "area")), "Area (ha)")
  g <- elsar_protection_gap(s)
  gb <- ggplot2::ggplot_build(elsar_plot_protection_gap(g))
  expect_identical(gb$layout$panel_params[[1]]$y.sec$get_labels(), "Target\n30%")
  # sf labels the graticule; no degree scales are added
  expect_null(.degree_scales(TRUE))
  expect_match(elsar_attribution(source = "A"), "^Data: A")
})

test_that("0/1 legends and captions follow the language", {
  withr::local_options(elsar.language = "pt", elsar.labels = NULL)
  expect_match(elsar_attribution(source = "A"), "^Dados: A")
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000,
                   crs = lang_crs, vals = rep(c(0, 1), 50))
  p <- elsar_plot_static_raster(r, data_type = "discrete", style = "modern")
  fill <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_identical(fill$get_labels(), c("Fora", "Dentro"))
})

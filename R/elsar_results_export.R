# Export ELSA results ---------------------------------------------------------------

#' Save plots to files
#'
#' Saves one plot or a named list of plots (e.g. from [elsar_plot_regions()])
#' with file names that are safe on every operating system.
#'
#' @param plots A `ggplot`/`patchwork` object or a named list of them.
#' @param dir Character. Output directory, created if needed.
#' @param prefix Character. Prefix for file names. For a single plot, the file
#'   name is `prefix` alone.
#' @param width,height Numeric. Size in inches. Default 10 x 4.5, which suits
#'   [elsar_plot_region()].
#' @param dpi Numeric. Resolution for raster formats. Default 300.
#' @param device Character. File format passed to [ggplot2::ggsave()], e.g.
#'   `"png"` (default), `"pdf"` or `"svg"`.
#' @param bg Background colour. Default `"white"`.
#'
#' @return The file paths, invisibly, named like `plots`.
#' @seealso [elsar_export_results()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#' s <- elsar_summarise_actions(x, ex$regions, region_col = "region_name")
#' plots <- elsar_plot_regions(x, s, ex$regions, "region_name")
#' paths <- elsar_save_plots(plots, tempdir(), prefix = "region", dpi = 72)
#' basename(paths)
elsar_save_plots <- function(plots,
                             dir,
                             prefix = "plot",
                             width = 10,
                             height = 4.5,
                             dpi = 300,
                             device = "png",
                             bg = "white") {
  single <- inherits(plots, "ggplot") || inherits(plots, "patchwork")
  if (single) plots <- stats::setNames(list(plots), "")
  assertthat::assert_that(
    is.list(plots), length(plots) > 0,
    single || (!is.null(names(plots)) && all(nzchar(names(plots)))),
    msg = "'plots' must be a plot or a named list of plots."
  )
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  stems <- if (single) prefix else paste(prefix, .safe_name(names(plots)), sep = "_")
  # Case-insensitive file systems (Windows, macOS) treat "North" and "north" as
  # one file, so make names unique ignoring case
  stems <- stems[seq_along(stems)]
  dup <- duplicated(tolower(stems)) | duplicated(tolower(stems), fromLast = TRUE)
  stems[dup] <- make.unique(tolower(stems[dup]), sep = "_")
  paths <- file.path(dir, paste0(stems, ".", device))

  for (i in seq_along(plots)) {
    ggplot2::ggsave(
      filename = paths[i], plot = plots[[i]], width = width, height = height,
      units = "in", dpi = dpi, device = device, bg = bg
    )
  }
  log_message("Saved {length(paths)} plot(s) to {dir}")
  invisible(stats::setNames(paths, names(plots)))
}

#' Summarise, plot and export ELSA results in one step
#'
#' Runs the full post-analysis workflow on an ELSA action map and writes the
#' results to a folder:
#'
#' * `<prefix>_actions_national.csv` and, with regions,
#'   `<prefix>_actions_by_region.csv` from [elsar_summarise_actions()];
#' * `<prefix>_protection_gap.csv` from [elsar_protection_gap()], if
#'   protected areas are given;
#' * `<prefix>_map.png` ([elsar_plot_actions()]), `<prefix>_bars.png`
#'   ([elsar_plot_action_bars()]) and `<prefix>_protection_gap.png`
#'   ([elsar_plot_protection_gap()]);
#' * one panel per region in `regions/` ([elsar_plot_region()]).
#'
#' Use the individual functions for finer control.
#'
#' @param x An ELSA action raster (`SpatRaster` or file path), or an action map
#'   already prepared with [elsar_prepare_actions()].
#' @param dir Character. Output directory, created if needed.
#' @param regions,region_col Optional regions and their name column, as in
#'   [elsar_summarise_actions()].
#' @param pas,scheme,pa_threshold Passed to [elsar_prepare_actions()] when `x`
#'   is not yet prepared.
#' @param target,protect_actions Passed to [elsar_protection_gap()].
#' @param units Area units, as in [elsar_summarise_actions()].
#' @param region_plots Logical. Save a panel per region. Default `TRUE`.
#' @param prefix Character. Prefix for output file names. Default `"elsa"`.
#' @param dpi Numeric. Resolution of PNG files. Default 300.
#'
#' @return Invisibly, a list with the `actions` map, `summary` (national),
#'   `summary_regions`, `gap`, `plots` and the written `files`.
#' @seealso [elsar_save_plots()], [elsar_results_report()]
#' @export
#'
#' @examples
#' \donttest{
#' ex <- elsar_example_results()
#' out <- elsar_export_results(
#'   ex$actions,
#'   dir = file.path(tempdir(), "elsaland"),
#'   regions = ex$regions, region_col = "region_name",
#'   pas = ex$pas, dpi = 72
#' )
#' basename(out$files)
#' }
elsar_export_results <- function(x,
                                 dir,
                                 regions = NULL,
                                 region_col = NULL,
                                 pas = NULL,
                                 scheme = elsar_action_scheme(),
                                 pa_threshold = NULL,
                                 target = 0.30,
                                 protect_actions = NULL,
                                 units = c("ha", "km2", "m2"),
                                 region_plots = TRUE,
                                 prefix = "elsa",
                                 dpi = 300) {
  units <- match.arg(units)
  if (!.is_prepared(x)) {
    x <- elsar_prepare_actions(x, scheme = scheme, pas = pas, pa_threshold = pa_threshold)
  } else if (!is.null(pas)) {
    warning("'x' is already prepared; 'pas' ignored.", call. = FALSE)
  }
  has_pa <- "pa_fraction" %in% names(x)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- character(0)
  write_csv <- function(d, name) {
    path <- file.path(dir, paste0(prefix, "_", name, ".csv"))
    utils::write.csv(d, path, row.names = FALSE)
    path
  }

  log_message("Summarising ELSA actions...")
  national <- elsar_summarise_actions(x, units = units)
  files <- c(files, write_csv(national, "actions_national"))

  by_region <- NULL
  if (!is.null(regions)) {
    by_region <- elsar_summarise_actions(x, regions, region_col, units = units)
    files <- c(files, write_csv(by_region, "actions_by_region"))
  }
  main_summary <- by_region %||% national

  gap <- NULL
  if (has_pa) {
    # The national row comes from the national summary, not the sum of the
    # regions, so the CSV, the plot and the report agree
    gap <- elsar_protection_gap(main_summary, target = target,
                                protect_actions = protect_actions,
                                national = if (!is.null(by_region)) national)
    files <- c(files, write_csv(gap, "protection_gap"))
  }

  log_message("Plotting ELSA results...")
  plots <- list(
    map = elsar_plot_actions(x, regions = regions),
    bars = elsar_plot_action_bars(main_summary)
  )
  if (!is.null(gap)) plots$protection_gap <- elsar_plot_protection_gap(gap)

  n_bars <- nlevels(main_summary$region)
  # Vertical bars: width grows with the number of regions
  sizes <- list(map = c(8, 7), bars = c(max(6, 2 + 0.7 * n_bars), 4.5),
                protection_gap = c(max(6, 2.5 + 0.7 * (n_bars + 1)), 4.5))
  for (nm in names(plots)) {
    files <- c(files, elsar_save_plots(
      plots[[nm]], dir, prefix = paste0(prefix, "_", nm),
      width = sizes[[nm]][1], height = sizes[[nm]][2], dpi = dpi
    ))
  }

  if (region_plots && !is.null(by_region)) {
    log_message("Plotting {n_bars} regional panels...")
    plots$regions <- elsar_plot_regions(x, by_region, regions, region_col)
    files <- c(files, elsar_save_plots(
      plots$regions, file.path(dir, "regions"), prefix = prefix, dpi = dpi
    ))
  }

  log_message("ELSA results written to {dir}")
  invisible(list(
    actions = x,
    summary = national,
    summary_regions = by_region,
    gap = gap,
    plots = plots,
    files = unname(files)
  ))
}

.is_prepared <- function(x) {
  inherits(x, "SpatRaster") && "action" %in% names(x) &&
    !inherits(try(.scheme_from_actions(x), silent = TRUE), "try-error")
}

.safe_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  # macOS's iconv writes accents as separate marks ("São" -> "S~ao"): drop them
  x <- gsub("[`'^~\"]", "", x)
  x <- gsub("[^[:alnum:]]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "region")
}

#' Render an ELSA results report
#'
#' Renders a report of an ELSA action map from the package's R Markdown
#' template: key figures, a national map and table, protection against a
#' target, actions by region, overlap with other layers and a profile with a
#' map-and-bars panel for each region. Sections are left out when their inputs
#' are not given.
#'
#' The template is also available in RStudio under *File > New File > R
#' Markdown > From Template > ELSA results report*. Rendered as is, it reports
#' on the bundled example data.
#'
#' @inheritParams elsar_export_results
#' @param layers Optional layers to report overlap for, as in
#'   [elsar_summarise_overlap()].
#' @param output_file Character. Path of the report to write.
#' @param format `"html"` (default) or `"word"`.
#' @param title,country Character. Report title and subtitle.
#' @param open Logical. Open the report when done. Default: in interactive
#'   sessions.
#' @param quiet Logical. Suppress rendering progress. Default `TRUE`.
#'
#' @return The path of the report, invisibly.
#' @seealso [elsar_export_results()]
#' @export
#'
#' @examples
#' \donttest{
#' if (rmarkdown::pandoc_available()) {
#'   ex <- elsar_example_results()
#'   path <- elsar_results_report(
#'     ex$actions,
#'     regions = ex$regions, region_col = "region_name", pas = ex$pas,
#'     output_file = file.path(tempdir(), "elsaland_report.html"),
#'     country = "Elsaland", open = FALSE
#'   )
#' }
#' }
elsar_results_report <- function(x,
                                 regions = NULL,
                                 region_col = NULL,
                                 pas = NULL,
                                 layers = NULL,
                                 scheme = elsar_action_scheme(),
                                 pa_threshold = NULL,
                                 target = 0.30,
                                 protect_actions = NULL,
                                 units = c("ha", "km2", "m2"),
                                 output_file = "elsa_results_report.html",
                                 format = c("html", "word"),
                                 title = "ELSA results report",
                                 country = "",
                                 open = interactive(),
                                 quiet = TRUE) {
  assertthat::assert_that(
    requireNamespace("rmarkdown", quietly = TRUE),
    requireNamespace("knitr", quietly = TRUE),
    msg = "Rendering the report needs the 'rmarkdown' and 'knitr' packages."
  )
  assertthat::assert_that(
    rmarkdown::pandoc_available(),
    msg = "Rendering the report needs pandoc (bundled with RStudio, or see https://pandoc.org)."
  )
  units <- match.arg(units)
  format <- match.arg(format)
  if (!.is_prepared(x)) {
    x <- elsar_prepare_actions(x, scheme = scheme, pas = pas, pa_threshold = pa_threshold)
  }
  # Read regions now so the report gets a validated sf
  if (!is.null(regions)) regions <- .prepare_regions(regions, region_col, x)

  template <- system.file(
    "rmarkdown", "templates", "elsa_results_report", "skeleton", "skeleton.Rmd",
    package = "elsar"
  )
  assertthat::assert_that(nzchar(template), msg = "Report template not found in elsar.")

  # Render from a temporary copy so intermediate files never touch the package
  work <- tempfile("elsa_report_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  rmd <- file.path(work, "elsa_results_report.Rmd")
  file.copy(template, rmd)

  # The extension follows the format, so a Word report is never named .html
  ext <- if (format == "html") "html" else "docx"
  if (!grepl(paste0("\\.", ext, "$"), output_file, ignore.case = TRUE)) {
    output_file <- paste0(tools::file_path_sans_ext(output_file), ".", ext)
  }
  output_file <- normalizePath(output_file, mustWork = FALSE)
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  output_format <- if (format == "html") {
    rmarkdown::html_document(toc = TRUE, toc_float = TRUE, toc_depth = 2)
  } else {
    rmarkdown::word_document(toc = TRUE)
  }

  log_message("Rendering ELSA results report...")
  out <- rmarkdown::render(
    rmd,
    output_format = output_format,
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    intermediates_dir = work,
    params = list(
      title = title, country = country, actions = x, regions = regions,
      # NULL params fall back to the template default, so "no target" is NA
      region_col = region_col, layers = layers, target = target %||% NA,
      protect_actions = protect_actions, units = units
    ),
    envir = new.env(parent = globalenv()),
    quiet = quiet
  )
  log_message("Report written to {out}")
  if (open) utils::browseURL(out)
  invisible(out)
}

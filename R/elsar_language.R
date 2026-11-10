# Language of plot text ----------------------------------------------------------
#
# Every piece of text elsar draws on a plot (axis titles, legend labels, the
# default action names, degree labels, number formats) comes from here, so
# plots can be made in the languages the documentation is translated into.

# Text of each language, by key. Keep the keys and their order the same in
# every language; elsar_labels() documents them.
.elsar_text <- list(
  en = c(
    protect = "Protect",
    manage = "Manage",
    restore = "Restore",
    urban_greening = "Urban Greening",
    do_nothing = "Do Nothing",
    existing_pas = "Existing PAs",
    new_protection = "New protection (ELSA)",
    national = "National",
    pct_of_region = "% of region",
    pct_of_planning_area = "% of planning area",
    pct_of_layer = "% of layer within planning area",
    area = "Area",
    target = "Target",
    outside = "Outside",
    inside = "Inside",
    data = "Data",
    west = "W",
    east = "E",
    south = "S",
    north = "N"
  ),
  es = c(
    protect = "Protecci\u00f3n",
    manage = "Manejo",
    restore = "Restauraci\u00f3n",
    urban_greening = "Enverdecimiento urbano",
    do_nothing = "Sin acci\u00f3n",
    existing_pas = "\u00c1reas protegidas existentes",
    new_protection = "Nueva protecci\u00f3n (ELSA)",
    national = "Nacional",
    pct_of_region = "% de la regi\u00f3n",
    pct_of_planning_area = "% del \u00e1rea de planificaci\u00f3n",
    pct_of_layer = "% de la capa en el \u00e1rea de planificaci\u00f3n",
    area = "Superficie",
    target = "Meta",
    outside = "Fuera",
    inside = "Dentro",
    data = "Datos",
    west = "O",
    east = "E",
    south = "S",
    north = "N"
  ),
  fr = c(
    protect = "Protection",
    manage = "Gestion",
    restore = "Restauration",
    urban_greening = "Verdissement urbain",
    do_nothing = "Aucune action",
    existing_pas = "Aires prot\u00e9g\u00e9es existantes",
    new_protection = "Nouvelle protection (ELSA)",
    national = "National",
    pct_of_region = "% de la r\u00e9gion",
    pct_of_planning_area = "% de la zone de planification",
    pct_of_layer = "% de la couche dans la zone de planification",
    area = "Superficie",
    target = "Cible",
    outside = "Ext\u00e9rieur",
    inside = "Int\u00e9rieur",
    data = "Donn\u00e9es",
    west = "O",
    east = "E",
    south = "S",
    north = "N"
  ),
  pt = c(
    protect = "Prote\u00e7\u00e3o",
    manage = "Manejo",
    restore = "Restaura\u00e7\u00e3o",
    urban_greening = "Esverdeamento urbano",
    do_nothing = "Sem a\u00e7\u00e3o",
    existing_pas = "\u00c1reas protegidas existentes",
    new_protection = "Nova prote\u00e7\u00e3o (ELSA)",
    national = "Nacional",
    pct_of_region = "% da regi\u00e3o",
    pct_of_planning_area = "% da \u00e1rea de planejamento",
    pct_of_layer = "% da camada na \u00e1rea de planejamento",
    area = "\u00c1rea",
    target = "Meta",
    outside = "Fora",
    inside = "Dentro",
    data = "Dados",
    west = "O",
    east = "L",
    south = "S",
    north = "N"
  ),
  ru = c(
    protect = "\u041e\u0445\u0440\u0430\u043d\u0430",
    manage = "\u0423\u043f\u0440\u0430\u0432\u043b\u0435\u043d\u0438\u0435",
    restore = "\u0412\u043e\u0441\u0441\u0442\u0430\u043d\u043e\u0432\u043b\u0435\u043d\u0438\u0435",
    urban_greening = "\u0413\u043e\u0440\u043e\u0434\u0441\u043a\u043e\u0435 \u043e\u0437\u0435\u043b\u0435\u043d\u0435\u043d\u0438\u0435",
    do_nothing = "\u0411\u0435\u0437 \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0439",
    existing_pas = "\u0421\u0443\u0449\u0435\u0441\u0442\u0432\u0443\u044e\u0449\u0438\u0435 \u041e\u041e\u041f\u0422",
    new_protection = "\u041d\u043e\u0432\u0430\u044f \u043e\u0445\u0440\u0430\u043d\u0430 (ELSA)",
    national = "\u0421\u0442\u0440\u0430\u043d\u0430 \u0432 \u0446\u0435\u043b\u043e\u043c",
    pct_of_region = "% \u0440\u0435\u0433\u0438\u043e\u043d\u0430",
    pct_of_planning_area = "% \u0442\u0435\u0440\u0440\u0438\u0442\u043e\u0440\u0438\u0438 \u043f\u043b\u0430\u043d\u0438\u0440\u043e\u0432\u0430\u043d\u0438\u044f",
    pct_of_layer = "% \u0441\u043b\u043e\u044f \u0432 \u043f\u0440\u0435\u0434\u0435\u043b\u0430\u0445 \u0442\u0435\u0440\u0440\u0438\u0442\u043e\u0440\u0438\u0438 \u043f\u043b\u0430\u043d\u0438\u0440\u043e\u0432\u0430\u043d\u0438\u044f",
    area = "\u041f\u043b\u043e\u0449\u0430\u0434\u044c",
    target = "\u0426\u0435\u043b\u044c",
    outside = "\u0412\u043d\u0435",
    inside = "\u0412\u043d\u0443\u0442\u0440\u0438",
    data = "\u0414\u0430\u043d\u043d\u044b\u0435",
    west = "\u00a0\u0437.\u00a0\u0434.",
    east = "\u00a0\u0432.\u00a0\u0434.",
    south = "\u00a0\u044e.\u00a0\u0448.",
    north = "\u00a0\u0441.\u00a0\u0448."
  )
)

# Decimal and thousands marks of each language
.elsar_number_marks <- list(
  en = c(decimal = ".", big = ","),
  es = c(decimal = ",", big = "."),
  fr = c(decimal = ",", big = "\u202f"),
  pt = c(decimal = ",", big = "."),
  ru = c(decimal = ",", big = "\u00a0")
)

# Keys of the standard actions, in the ELSA tool's raster value order
.action_keys <- c("protect", "manage", "restore", "urban_greening", "do_nothing")

#' Text used in elsar plots, in the chosen language
#'
#' @description
#' Returns the text elsar draws on plots: default action names, legend labels,
#' axis titles and degree labels. Plots are in English by default. Set the
#' language once per session and every plot, default action scheme and
#' summary uses it:
#'
#' ```r
#' options(elsar.language = "es")
#' ```
#'
#' Supported languages are English (`"en"`), Spanish (`"es"`), French
#' (`"fr"`), Portuguese (`"pt"`) and Russian (`"ru"`), the languages of the
#' elsar documentation. Numbers follow the language's decimal and thousands
#' marks (e.g. `1.234,5` in Spanish), and map graticules use its compass
#' letters (e.g. `79,5°O`).
#'
#' To change single pieces of text, in any language, set
#' `options(elsar.labels = c(key = "text"))`, e.g.
#' `options(elsar.labels = c(pct_of_region = "% de la provincia"))`.
#'
#' Arguments that take text (such as `pa_label`, `national_label` or the
#' `labels` of an action scheme) still take precedence for a single call.
#'
#' @section Keys:
#' \describe{
#'   \item{`protect`, `manage`, `restore`, `urban_greening`, `do_nothing`}{
#'     Names of the standard ELSA actions, used by [elsar_action_scheme()]
#'     and [elsar_palette()].}
#'   \item{`existing_pas`, `new_protection`}{Legend labels for existing
#'     protected areas and new protection proposed by ELSA.}
#'   \item{`national`}{Name of the national row in summaries.}
#'   \item{`pct_of_region`, `pct_of_planning_area`, `pct_of_layer`, `area`}{
#'     Axis titles of bar charts. The area title is followed by the units.}
#'   \item{`target`}{Label of the target line.}
#'   \item{`outside`, `inside`}{Legend labels of 0/1 layers.}
#'   \item{`data`}{Prefix of the data source in map captions.}
#'   \item{`west`, `east`, `south`, `north`}{Compass letters of graticule
#'     labels.}
#' }
#'
#' @param language Character. Language code. Defaults to the
#'   `elsar.language` option, or `"en"`.
#'
#' @return A named character vector, one element per key.
#'
#' @family elsar style
#' @export
#'
#' @examples
#' elsar_labels()
#' elsar_labels("es")[c("protect", "pct_of_region")]
#'
#' # Make every plot in Spanish, with provinces instead of regions
#' old <- options(
#'   elsar.language = "es",
#'   elsar.labels = c(pct_of_region = "% de la provincia")
#' )
#' elsar_action_scheme()
#' options(old)
elsar_labels <- function(language = getOption("elsar.language", "en")) {
  language <- .check_language(language)
  text <- .elsar_text[[language]]
  overrides <- getOption("elsar.labels")
  if (is.null(overrides)) {
    return(text)
  }
  overrides <- unlist(overrides)
  assertthat::assert_that(
    is.character(overrides), !is.null(names(overrides)), all(nzchar(names(overrides))),
    msg = "The 'elsar.labels' option must be a named character vector, e.g. c(pct_of_region = \"% of province\")."
  )
  unknown <- setdiff(names(overrides), names(text))
  assertthat::assert_that(
    length(unknown) == 0,
    msg = paste0(
      "Unknown key(s) in the 'elsar.labels' option: ", toString(unknown),
      ". See ?elsar_labels for the keys."
    )
  )
  text[names(overrides)] <- overrides
  text
}

.check_language <- function(language) {
  assertthat::assert_that(
    assertthat::is.string(language),
    msg = "The language must be a single language code, e.g. \"es\"."
  )
  language <- tolower(language)
  assertthat::assert_that(
    language %in% names(.elsar_text),
    msg = paste0(
      "Language '", language, "' is not supported. Use one of: ",
      toString(names(.elsar_text)), "."
    )
  )
  language
}

.language <- function() .check_language(getOption("elsar.language", "en"))

# One piece of text in the current language
.txt <- function(key) elsar_labels()[[key]]

# Default names of the standard actions, in the current language
.action_names <- function() unname(elsar_labels()[.action_keys])

.number_marks <- function() .elsar_number_marks[[.language()]]

# Label functions for values on bar charts. English keeps the short scale
# (1.2K, 3.4M); other languages write the full number with their own marks,
# since the K/M/B suffixes are English.
.label_pct <- function(accuracy = NULL) {
  scales::label_percent(scale = 1, accuracy = accuracy, decimal.mark = .number_marks()[["decimal"]])
}

.label_area <- function(accuracy = NULL) {
  if (.language() == "en") {
    return(scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = accuracy))
  }
  marks <- .number_marks()
  scales::label_number(
    accuracy = accuracy %||% 1,
    big.mark = marks[["big"]], decimal.mark = marks[["decimal"]]
  )
}

# Degree labels of a map graticule in the current language, as x and y scales
# to add next to coord_sf(datum = .map_datum(TRUE)). English keeps sf's own
# labels, so NULL is returned for it.
.degree_scales <- function(graticule = TRUE) {
  if (!isTRUE(graticule) || (.language() == "en" && is.null(getOption("elsar.labels")))) {
    return(NULL)
  }
  text <- elsar_labels()
  decimal <- .number_marks()[["decimal"]]
  degrees <- function(negative, positive) {
    function(x) {
      num <- format(abs(x), decimal.mark = decimal, trim = TRUE, drop0trailing = TRUE)
      hemi <- ifelse(x < 0, negative, ifelse(x > 0, positive, ""))
      ifelse(is.na(x), NA_character_, paste0(num, "\u00b0", hemi))
    }
  }
  list(
    ggplot2::scale_x_continuous(labels = degrees(text[["west"]], text[["east"]])),
    ggplot2::scale_y_continuous(labels = degrees(text[["south"]], text[["north"]]))
  )
}

# Reduce an action label to a language-independent key, so labels written by
# the ELSA tool (in any supported language, or with the tool's own names such
# as "Reduce Pressures") match the scheme's labels.
.label_key <- function(x) {
  key <- .normalise_label(x)
  synonyms <- c(reducepressures = "manage", urbanadapt = "urbangreening",
                nothing = "donothing", noaction = "donothing")
  for (lang in names(.elsar_text)) {
    names_in_lang <- .normalise_label(.elsar_text[[lang]][.action_keys])
    synonyms[names_in_lang] <- gsub("_", "", .action_keys)
  }
  ifelse(key %in% names(synonyms), synonyms[key], key)
}

.normalise_label <- function(x) {
  x <- tolower(as.character(x))
  x <- chartr(
    "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00e7\u00f1",
    "aaaaaeeeeiiiiooooouuuucn",
    x
  )
  gsub("[^\\p{L}\\p{N}]", "", x, perl = TRUE)
}

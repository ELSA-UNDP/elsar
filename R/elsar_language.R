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
    protect = "Protección",
    manage = "Manejo",
    restore = "Restauración",
    urban_greening = "Enverdecimiento urbano",
    do_nothing = "Sin acción",
    existing_pas = "Áreas protegidas existentes",
    new_protection = "Nueva protección (ELSA)",
    national = "Nacional",
    pct_of_region = "% de la región",
    pct_of_planning_area = "% del área de planificación",
    pct_of_layer = "% de la capa en el área de planificación",
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
    existing_pas = "Aires protégées existantes",
    new_protection = "Nouvelle protection (ELSA)",
    national = "National",
    pct_of_region = "% de la région",
    pct_of_planning_area = "% de la zone de planification",
    pct_of_layer = "% de la couche dans la zone de planification",
    area = "Superficie",
    target = "Cible",
    outside = "Extérieur",
    inside = "Intérieur",
    data = "Données",
    west = "O",
    east = "E",
    south = "S",
    north = "N"
  ),
  pt = c(
    protect = "Proteção",
    manage = "Manejo",
    restore = "Restauração",
    urban_greening = "Esverdeamento urbano",
    do_nothing = "Sem ação",
    existing_pas = "Áreas protegidas existentes",
    new_protection = "Nova proteção (ELSA)",
    national = "Nacional",
    pct_of_region = "% da região",
    pct_of_planning_area = "% da área de planejamento",
    pct_of_layer = "% da camada na área de planejamento",
    area = "Área",
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
    protect = "Охрана",
    manage = "Управление",
    restore = "Восстановление",
    urban_greening = "Городское озеленение",
    do_nothing = "Без действий",
    existing_pas = "Существующие ООПТ",
    new_protection = "Новая охрана (ELSA)",
    national = "Страна в целом",
    pct_of_region = "% региона",
    pct_of_planning_area = "% территории планирования",
    pct_of_layer = "% слоя в пределах территории планирования",
    area = "Площадь",
    target = "Цель",
    outside = "Вне",
    inside = "Внутри",
    data = "Данные",
    west = " з. д.",
    east = " в. д.",
    south = " ю. ш.",
    north = " с. ш."
  )
)

# Decimal and thousands marks of each language
.elsar_number_marks <- list(
  en = c(decimal = ".", big = ","),
  es = c(decimal = ",", big = "."),
  fr = c(decimal = ",", big = " "),
  pt = c(decimal = ",", big = "."),
  ru = c(decimal = ",", big = " ")
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
      ifelse(is.na(x), NA_character_, paste0(num, "°", hemi))
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
    "áàâãäéèêëíìîïóòôõöúùûüçñ",
    "aaaaaeeeeiiiiooooouuuucn",
    x
  )
  gsub("[^\\p{L}\\p{N}]", "", x, perl = TRUE)
}

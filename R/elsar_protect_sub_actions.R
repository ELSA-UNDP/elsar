# Protect sub-actions: existing protected areas, expansions, corridors and new
# protected areas, from the action map's protected-area layer and patch analysis.

#' Split Protect into existing, expanded, connecting and new protected areas
#'
#' Adds rules that split the Protect action by how each area relates to
#' existing protected areas (PAs). This is how ELSA's proposed protection
#' is usually reported against an area-based target such as Target 3 of the
#' Kunming-Montreal Global Biodiversity Framework (30%):
#'
#' * **Existing PAs**: Protect cells covered by existing protected areas.
#' * **PA expansion**: patches of new protection that touch an existing PA,
#'   or are within `expansion_km` of one.
#' * **Corridor between PAs** (if `corridors = TRUE`): patches of new
#'   protection that reach two or more different PAs, and so could connect
#'   them.
#' * **Small new area** (if `min_new_km2` is set): patches that reach no PA
#'   and are smaller than `min_new_km2`. These may suit other effective
#'   area-based conservation measures (OECMs) better than new PAs.
#' * **New PA**: all other patches of new protection.
#'
#' The prepared action map must have a protected-area layer (`pas` in
#' [elsar_prepare_actions()]).
#'
#' @section How patches and PAs are identified:
#' A patch is a group of Protect cells outside PAs that touch, including
#' diagonally. A whole patch gets one class. PAs are identified the same way,
#' from the cells whose protected-area cover is at least `pa_threshold`, so PAs
#' that touch each other count as one PA. A patch reaches a PA when one of its
#' cell centres is within `expansion_km` of a PA cell centre. Cells next to
#' each other, including diagonally, always count as reaching. Distances are in
#' the map's units, so the action map must be in a projected CRS in metres, as
#' ELSA maps are.
#'
#' @param rules Rules from [elsar_sub_actions()] to add to, or `NULL` to start
#'   a new rule set.
#' @param parent Character. Label of the Protect action in the action scheme.
#'   Defaults to "Protect" in the current language.
#' @param pa_threshold Number between 0 and 1. A cell counts as inside an
#'   existing PA when at least this share of it is protected. Default 0.5.
#' @param expansion_km Number. How close (in km) a patch must be to an existing
#'   PA to count as its expansion. Default 0: the patch must touch the PA.
#' @param corridors Logical. Report patches that reach two or more PAs as
#'   corridors. Default `TRUE`.
#' @param min_new_km2 Optional number. Patches that reach no PA and are smaller
#'   than this (in km2) are reported as small new areas, not new PAs.
#' @param labels Optional named character vector to rename classes, with any of
#'   the names `existing`, `expansion`, `corridor`, `small` and `new`. Defaults
#'   come from [elsar_labels()] in the current language.
#' @param colours Optional named character vector of colours, with the same
#'   names. By default, existing PAs are dark green, as in elsar's other
#'   results plots, and the other classes are shades of the Protect colour.
#'
#' @return An `elsar_sub_actions` rule set.
#' @seealso [elsar_sub_actions()], [elsar_assign_sub_actions()],
#'   [elsar_protection_gap()]
#' @export
#'
#' @examples
#' ex <- elsar_example_results()
#' x <- elsar_prepare_actions(ex$actions, pas = ex$pas)
#'
#' rules <- add_protect_sub_actions(expansion_km = 2, min_new_km2 = 5)
#' rules
#'
#' sub <- elsar_assign_sub_actions(x, rules)
#' elsar_summarise_actions(sub, units = "km2")
add_protect_sub_actions <- function(rules = NULL,
                                    parent = .txt("protect"),
                                    pa_threshold = 0.5,
                                    expansion_km = 0,
                                    corridors = TRUE,
                                    min_new_km2 = NULL,
                                    labels = NULL,
                                    colours = NULL) {
  if (!is.null(rules)) .check_sub_action_rules(rules)
  assertthat::assert_that(
    assertthat::is.string(parent), nzchar(parent),
    msg = "'parent' must be a single non-empty string: the label of the Protect action."
  )
  assertthat::assert_that(
    is.null(rules) || !parent %in% rules$parent,
    msg = paste0("The rules already split '", parent, "'. Give each parent once.")
  )
  assertthat::assert_that(
    is.numeric(pa_threshold), length(pa_threshold) == 1, pa_threshold > 0, pa_threshold <= 1,
    msg = "'pa_threshold' must be a single number above 0 and at most 1."
  )
  assertthat::assert_that(
    is.numeric(expansion_km), length(expansion_km) == 1, expansion_km >= 0,
    msg = "'expansion_km' must be a single number of 0 or more."
  )
  assertthat::assert_that(
    assertthat::is.flag(corridors), !is.na(corridors),
    msg = "'corridors' must be TRUE or FALSE."
  )
  assertthat::assert_that(
    is.null(min_new_km2) ||
      (is.numeric(min_new_km2) && length(min_new_km2) == 1 && min_new_km2 > 0),
    msg = "'min_new_km2' must be NULL or a single number above 0."
  )

  classes <- c("existing", "expansion", if (corridors) "corridor", if (!is.null(min_new_km2)) "small", "new")
  default_labels <- c(
    existing = .txt("existing_pas"), expansion = .txt("pa_expansion"),
    corridor = .txt("pa_corridor"), small = .txt("pa_small_new"), new = .txt("pa_new")
  )
  labels <- .protect_overrides(labels, default_labels, "labels")
  if (!is.null(colours)) {
    colours <- unlist(colours)
    .check_colours(unname(colours), length(colours))
  }
  colours <- .protect_overrides(colours, c(existing = "#005000"), "colours", fill = NA_character_)

  params <- list(
    pa_threshold = pa_threshold, expansion_km = expansion_km,
    corridors = corridors, min_new_km2 = min_new_km2
  )
  layer <- .protect_layer_name(parent)
  conditions <- c(
    existing = paste0("existing PA (cover >= ", pa_threshold, ")"),
    expansion = if (expansion_km > 0) paste0("within ", expansion_km, " km of one PA") else "touches one PA",
    corridor = "reaches 2+ PAs",
    small = paste0("reaches no PA, smaller than ", min_new_km2, " km2")
  )

  n <- length(classes)
  # Built outside tibble(), which would otherwise unquote the !! itself
  quosures <- lapply(classes[-n], function(cl) {
    rlang::new_quosure(
      rlang::expr(!!rlang::sym(layer) == !!.protect_codes[[cl]]),
      env = baseenv()
    )
  })
  new <- tibble::tibble(
    parent = parent,
    label = unname(labels[classes]),
    priority = seq_len(n),
    condition = c(unname(conditions[classes[-n]]), NA_character_),
    colour = unname(colours[classes]),
    is_other = c(rep(FALSE, n - 1), TRUE),
    quosure = c(quosures, list(NULL)),
    kind = "protect",
    params = rep(list(params), n)
  )
  .check_sub_action_labels(new)

  out <- if (is.null(rules)) new else rbind(tibble::as_tibble(rules), new)
  .check_sub_action_labels(out)
  class(out) <- c("elsar_sub_actions", class(tibble::tibble()))
  out
}

# Internal helpers ---------------------------------------------------------------

.protect_codes <- c(existing = 1L, expansion = 2L, corridor = 3L, small = 4L, new = 5L)

.protect_layer_name <- function(parent) paste0(".elsar_protect_", parent)

# Replace defaults with user values, checking the names
.protect_overrides <- function(user, defaults, arg, fill = NULL) {
  all_names <- names(.protect_codes)
  if (!is.null(fill)) {
    defaults <- c(defaults, stats::setNames(rep(fill, length(all_names)), all_names))
    defaults <- defaults[!duplicated(names(defaults))]
  }
  if (is.null(user)) return(defaults)
  user <- unlist(user)
  assertthat::assert_that(
    is.character(user), !is.null(names(user)), all(names(user) %in% all_names),
    msg = paste0(
      "'", arg, "' must be a named character vector with names among: ",
      toString(all_names), "."
    )
  )
  defaults[names(user)] <- user
  defaults
}

# Add the Protect class raster of each Protect parent to the condition layers
.add_protect_layers <- function(layers, x, rules, scheme) {
  if (!"kind" %in% names(rules)) return(layers)
  protect <- rules[rules$kind == "protect", ]
  for (p in unique(protect$parent)) {
    assertthat::assert_that(
      "pa_fraction" %in% names(x),
      msg = paste0(
        "Splitting '", p, "' by existing protected areas needs a protected-area ",
        "layer: pass 'pas' to elsar_prepare_actions()."
      )
    )
    params <- protect$params[protect$parent == p][[1]]
    value <- scheme$value[scheme$label == p]
    layers[[.protect_layer_name(p)]] <- .protect_classes(x, value, params)
  }
  layers
}

# Class of every cell of the Protect action (codes in .protect_codes); NA
# elsewhere
.protect_classes <- function(x, value, params) {
  assertthat::assert_that(
    !terra::is.lonlat(x),
    msg = paste(
      "Splitting Protect by patches needs a projected action map (distances in metres),",
      "as ELSA produces. Project it first, e.g. with terra::project()."
    )
  )
  action <- x[["action"]]
  levels(action) <- NULL
  is_protect <- action == value
  is_pa <- x[["pa_fraction"]] >= params$pa_threshold

  existing <- is_protect & is_pa
  candidate <- is_protect & !is_pa
  classes <- terra::ifel(existing, .protect_codes[["existing"]], NA)

  cand_id <- terra::patches(terra::ifel(candidate, 1, NA), directions = 8, zeroAsNA = TRUE)
  ids <- terra::unique(cand_id)[[1]]
  if (length(ids) == 0) return(terra::mask(classes, terra::ifel(is_protect, 1, NA)))

  # Lowest and highest PA id within reach of each cell: a patch reaches two or
  # more PAs exactly when its lowest and highest reachable ids differ
  pa_id <- terra::patches(terra::ifel(is_pa, 1, NA), directions = 8, zeroAsNA = TRUE)
  w <- .reach_window(x, params$expansion_km)
  lo <- terra::focal(pa_id, w, fun = "min", na.rm = TRUE)
  hi <- terra::focal(pa_id, w, fun = "max", na.rm = TRUE)
  names(cand_id) <- "patch"

  z_lo <- terra::zonal(lo, cand_id, fun = "min", na.rm = TRUE)
  z_hi <- terra::zonal(hi, cand_id, fun = "max", na.rm = TRUE)
  z_area <- terra::zonal(.cell_area(x), cand_id, fun = "sum", na.rm = TRUE)
  z <- data.frame(patch = z_lo[[1]], lo = z_lo[[2]], hi = z_hi[[2]], area = z_area[[2]] / 1e6)
  z$lo[!is.finite(z$lo)] <- NA
  z$hi[!is.finite(z$hi)] <- NA

  reach <- !is.na(z$lo)
  code <- rep(.protect_codes[["new"]], nrow(z))
  code[reach] <- .protect_codes[["expansion"]]
  if (isTRUE(params$corridors)) code[reach & z$lo != z$hi] <- .protect_codes[["corridor"]]
  if (!is.null(params$min_new_km2)) code[!reach & z$area < params$min_new_km2] <- .protect_codes[["small"]]

  patch_class <- terra::classify(cand_id, cbind(z$patch, code))
  terra::cover(classes, patch_class)
}

# Focal window of cells within `km` of the centre (always the 8 neighbours),
# with NA outside so focal() ignores those cells
.reach_window <- function(x, km) {
  w <- matrix(1, 3, 3)
  if (km > 0) {
    circle <- terra::focalMat(x[["action"]], km * 1000, type = "circle")
    if (nrow(circle) > 3) {
      w <- ifelse(circle > 0, 1, NA)
      mid <- (nrow(w) + 1) / 2
      w[(mid - 1):(mid + 1), (mid - 1):(mid + 1)] <- 1
    }
  }
  w
}

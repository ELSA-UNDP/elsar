#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom stats quantile setNames
#' @importFrom utils head select.list tail
## usethis namespace: end
# Import one function from sf so that loading elsar also loads sf's namespace
# and registers its S3 methods (e.g. `[.sf`, `filter.sf`). elsar otherwise
# refers to sf only via `sf::`, which does not load sf at package-load time, so
# operations on sf objects (including the bundled `boundary_dat`) would
# dispatch to the wrong method until sf happened to be loaded some other way.
#' @importFrom sf st_as_sf
NULL

#' Define spatial prioritization problems with optional locked-in constraints
#'
#' These functions define spatial prioritization problems using the `prioritizr` package.
#' Each function allows customization of locked-in constraints for specific areas,
#' affecting how zones are allocated (protect, restore, manage, etc.).
#' For example, areas may be freely allocated, or restricted based on OECMs (Other
#' Effective Conservation Measures) or degraded areas within protected areas.
#'
#' @param input A list or dataframe containing input parameters for the prioritization (e.g. from shiny app). Needs to contain targets for different zones.
#' @param prob.ta An existing `prioritizr` problem object to be updated with constraints.
#' @param pu_temp A `SpatRaster`of raster layers representing the available planning units for the different zones (protect, restore, manage, etc.).
#' @param pu A `SpatRaster` of all possible planning units in the planning region (one layer)
#' @param PA A `SpatRaster`with the current protected areas that are always locked in (single layer)
#' @param PA0 A `SpatRaster`that is the opposite of `PA`, so only allows to select areas that are not currently PAs. (single layer) NOTE: we can probs get rid of this and calc on the fly
#' @param Rest A `SpatRaster`with the current restoration projects that are always locked in (single layer)
#' @param Rest0 A `SpatRaster`that is the opposite of `Rest`, so only allows to select areas that are not currently being restored.(single layer) NOTE: we can probs get rid of this and calc on the fly
#' @param pas_locked_out A `SpatRaster` that allows to lock out all the current PAs from being selected in other zones (as many layers as there are zones). NOTE: we can probs get rid of this and just use PA and PA0 to do on the fly.
#'
#' @return A `prioritizr` problem object with updated constraints based on the input.
#' @noRd
#' @keywords internal
#'
define_problem_avail <- function(input,
                                 prob.ta,
                                 pu_temp,
                                 pu,
                                 PA,
                                 PA0,
                                 Rest,
                                 Rest0,
                                 pas_locked_out) {
  # get minimum targets required to lock in existing PAs and Restoratio areas
  target <- get_elsa_min_lockin_target(c(PA, Rest, PA0), input, pu)


  prob.ta <- prob.ta %>%
    prioritizr::add_max_utility_objective(terra::global(pu, "sum",
                                                        na.rm = TRUE)[[1]]) %>%
    prioritizr::add_locked_out_constraints(pas_locked_out) %>%
    prioritizr::add_locked_in_constraints(c(Rest0, Rest, Rest0, Rest0, Rest0)) %>%
    prioritizr::add_linear_constraints(
      threshold = get_elsa_target(pu, target[1]),
      sense = "<=",
      data = c(
        pu_temp[[1]], # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) %>%
    prioritizr::add_linear_constraints(
      threshold = get_elsa_target(pu, target[2]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]], # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) %>%
    prioritizr::add_linear_constraints(
      threshold = get_elsa_target(pu, target[3]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]], # manage_zone
        pu_temp[[4]] * 0, # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) %>%
    prioritizr::add_mandatory_allocation_constraints()

  return(prob.ta)
}

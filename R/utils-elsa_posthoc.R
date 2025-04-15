#' Calculate feature representation
#'
#' This function calculates the feature representation of a single feature per zone comapred to all possible PUs for this feature across all zones.
#' E.g. if 60 out of 100 possible PUs are selected for zone 1 and 50 out of 100 possible PUs are selected for zone 2:
#' This DOES not mean that the overall feature representation in the planning region is 60% (60/100) + 50% (50/100) (= 110%, so > 100%)
#' Instead we need to get the representation in the zone versus all possible planning units in the whole region.
#' This means that if in our case zones do not share any overlapping possible area prior to prioritisation, we would have 200 possible planning units overall for the feature in the planning region.
#' The representation across the whole planning region would then be: 60/200 + 50/200, so 110/200 (= 55%)
#'
#' @param prob.all A `prioritizr` problem. Needed as an input for the `prioritizr` function `eval_feature_representation_summary()`
#' @param elsa_result A `prioritizr` solution. Needed as an input for the `prioritizr` function `eval_feature_representation_summary()`
#' @param elsa_raster A `SpatRaster` containing label information of the the zones used to obtain solution. Has no zero values.
#' @param feat_df A `df` containing information on the features, including columns "label" and "theme"
#' @param ELSA_text A `tbl_df` containing the translations for the displayed text in the plot. Needs
#'
#' @return A `list` with two `tbl_df`, one with the raw data and the second with the data prepared for plotting.
#' @export
#'
calc_feat_rep <- function(prob.all,
                          elsa_result,
                          elsa_raster,
                          feat_df,
                          ELSA_text) {
  # First get overall info from prioritizr
  overall_rep <- prioritizr::eval_feature_representation_summary(prob.all, elsa_result) %>%
    dplyr::rename(
      zone = summary,
      relative_held_overall = .data$relative_held,
      total_amount_overall = .data$total_amount #absolute numbers of possible PUs across all zones for feature
    ) %>%
    dplyr::filter(.data$zone == "overall") %>%
    dplyr::select(-"zone", -"absolute_held")

  # Calculate relative values for each zone and pivot them into new columns
  relative_rep <- prioritizr::eval_feature_representation_summary(prob.all, elsa_result) %>%
    dplyr::rename(zone = .data$summary) %>%
    dplyr::filter(.data$zone != "overall") %>%
    dplyr::select("zone", "feature", "absolute_held") %>% # absolute numbers (NOT %) of selected PUs of a feature PER ZONE
    dplyr::left_join(overall_rep, by = "feature") %>%
    dplyr::mutate(relative_held_zone = .data$absolute_held / .data$total_amount_overall) %>% #calculate the proportion of selected PUs for a feature in a zone vs all possible
    dplyr::select("zone", "feature", "relative_held_zone") %>%
    tidyr::pivot_wider(names_from = .data$zone, values_from = .data$relative_held_zone)

  # Combine the overall totals with the relative values
  feature_rep <- overall_rep %>%
    dplyr::left_join(relative_rep, by = "feature")

  feature_rep_tabl <- feature_rep %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(. * 100, 1))) %>%
    dplyr::rename_with(~ tibble::as_tibble(cats(elsa_raster)[[1]])$label,
                       .cols = -c(1:3)) %>%
    dplyr::select(3:7) %>%
    tibble::add_column(
      Name = feat_df$label,
      Theme = feat_df$theme,
      .before = 1
    ) %>%
    dplyr::rename(
      "{ELSA_text %>% dplyr::filter(var == 'data') %>% dplyr::pull(language)}" := .data$Name,
      "{ELSA_text %>% dplyr::filter(var == 'theme') %>% dplyr::pull(language)}" := .data$Theme,
      "{ELSA_text %>% dplyr::filter(var == 'overall') %>% dplyr::pull(language)}" := .data$relative_held_overall,
    )

  return(list(feature_rep, feature_rep_tabl))
}

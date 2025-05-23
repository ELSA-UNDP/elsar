#' Create a Map of the ELSA output to download with GIS layers
#'
#' This function gives a quick way of getting a map immediately when downloading the output GIS layers.
#'
#' @param elsa_raster A `SpatRaster`with one layer that contains different categories for the different zones.
#' @param ELSA_text A `tbl_df` containing the translations for the displayed text in the plot.
#' @param pu0 A `SpatRaster` of all possible planning units in the planning region (one layer)
#'
#' @return A ggplot object with a map of the prioritization output
#' @export
elsa_plot_output_map <- function(elsa_raster,
                                 ELSA_text,
                                 pu0) {
  legend_title <- paste0("ELSA ", ELSA_text %>% dplyr::filter(.data$var == "action") %>% dplyr::pull(.data$language))

  raster_attributes <- tibble::as_tibble(terra::cats(elsa_raster[[1]])[[1]]) %>%
    dplyr::filter(.data$value %in% unique(terra::values(elsa_raster[[1]])))

  outlines <- terra::as.polygons(pu0) %>%
    # And convert to lines
    terra::as.lines()

  elsa_map <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = elsa_raster) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(
      name = legend_title,
      values = dplyr::pull(raster_attributes, .data$colour),
      breaks = utils::head(dplyr::pull(raster_attributes, .data$label), -1), # remove last element to not show the do-nothing zone in legend
      na.translate = FALSE
    ) +
    tidyterra::geom_spatvector(
      data = outlines,
      color = ggplot2::alpha("grey", 0.7),
      linewidth = 0.15
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.25,
      height = ggplot2::unit(0.25, "cm")
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      text = ggplot2::element_text(size = 10, colour = "black"),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Create a Bar Plot of Feature Representation
#'
#' This function generates a stacked bar plot displaying the representation of each input
#' feature, broken up into specified actions (e.g., "Protect," "Restore," "Manage,"
#' and optionally "Urban Greening").
#'
#' @param feature_rep_tabl A dataframe containing representation values for each feature.
#' Columns should include numeric data for each feature and action.
#' @param input The input object of an R Shiny app containing budget information for each action.
#' This is used to label the budget percentage of each action in the plot legend.
#' @param ELSA_text A `tbl_df` containing the translations for the displayed text in the plot.
#' @param pal.elsa A `tibble` with palette information for plot
#'
#' @return A ggplot object with a stacked bar chart showing feature representation percentages.
#' @export
elsa_plot_representation <- function(feature_rep_tabl,
                                          input,
                                          ELSA_text,
                                          pal.elsa = tibble::tibble(
                                            colour = c("#4daf4a", "#984ea3", "#377eb8", "#adaf4a", "#FFFFFF00"),
                                            category = c("Protect", "Restore", "Manage", "PR", "Do_Nothing")
                                          )) {
  # Reshape data to a long format for plotting, including optional urban greening column
  data_long <- feature_rep_tabl %>%
    tidyr::pivot_longer(
      cols = c(4, 5, 6, 7),
      names_to = ELSA_text %>% dplyr::filter(.data$var == "category") %>% dplyr::pull(.data$language),
      values_to = ELSA_text %>% dplyr::filter(.data$var == "pct") %>% dplyr::pull(.data$language)
    ) %>%
    dplyr::mutate(
      dplyr::across(1, ~ factor(., levels = sort(unique(.), decreasing = TRUE))), # Need to reverse factor levels so they plot correctly - ggplot will flip them.
      dplyr::across(c(2, 4), as.factor)
    ) %>%
    dplyr::arrange(dplyr::desc(dplyr::across(1)))

  # Create legend text dynamically based on user-defined zone budgets
  protect_budget <- glue::glue("{ELSA_text %>% dplyr::filter(.data$var == 'protect') %>% dplyr::pull(.data$language)} {round(input$zone_1_target, 1)}%")
  restore_budget <- glue::glue("{ELSA_text %>% dplyr::filter(.data$var == 'restore') %>% dplyr::pull(.data$language)} {round(input$zone_2_target, 1)}%")
  manage_budget <- glue::glue("{ELSA_text %>% dplyr::filter(.data$var == 'manage') %>% dplyr::pull(.data$language)} {round(input$zone_3_target, 1)}%")
  pr_budget <- glue::glue("{ELSA_text %>% dplyr::filter(.data$var == 'pr') %>% dplyr::pull(.data$language)}")

  # Define fill colors for each zone in the plot
  fill_values <- c(
    "Protect" = pal.elsa$colour[1],
    "Restore" = pal.elsa$colour[2],
    "Manage" = pal.elsa$colour[3],
    "Protect and Restore" = pal.elsa$colour[4]
  )

  # Define legend items and breaks in the legend
  breaks <- c(
    ELSA_text %>% dplyr::filter(.data$var == "protect") %>% dplyr::pull(.data$language),
    ELSA_text %>% dplyr::filter(.data$var == "restore") %>% dplyr::pull(.data$language),
    ELSA_text %>% dplyr::filter(.data$var == "manage") %>% dplyr::pull(.data$language),
    ELSA_text %>% dplyr::filter(.data$var == "pr") %>% dplyr::pull(.data$language)
  )

  # Ensure fill values are in the correct language
  names(fill_values) <- breaks

  # Create labels with budget information for each legend item
  labels <- c(protect_budget, restore_budget, manage_budget, pr_budget)

  # Create a ggplot stacked bar chart
  elsa_representation_plot <- ggplot2::ggplot(
    data_long,
    ggplot2::aes(
      x = .data[[names(data_long)[1]]],
      y = .data[[names(data_long)[5]]],
      fill = .data[[names(data_long)[4]]]
    )
  ) +
    ggplot2::geom_bar(stat = "identity") + # Add bars with values
    ggplot2::scale_fill_manual(
      values = fill_values,
      name = glue::glue("ELSA {ELSA_text %>% filter(var == 'action') %>% pull(language)}"), # Legend title
      labels = labels, # Set legend labels with budget
      breaks = breaks # Set legend order
    ) +
    ggplot2::coord_flip() + # Flip the coordinates for a horizontal bar chart
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 25)
    ) +
    ggplot2::labs(
      x = " ", # No label on x-axis
      y = glue::glue("{ELSA_text %>% filter(var == 'rep') %>% pull(language)} %") # Label for y-axis
    ) +
    ggplot2::theme_minimal() + # Minimal theme for cleaner look
    ggplot2::theme(
      text = ggplot2::element_text(family = "Roboto", colour = "#495057"), # Font styling
      legend.position = "top", # Place legend above the plot
      legend.background = ggplot2::element_rect(color = "#495057", linewidth = 0.5), # Legend box style
      legend.text = ggplot2::element_text(size = 14, face = "bold"),
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      panel.border = ggplot2::element_rect(color = "#495057", linewidth = 1, fill = NA) # Plot border styling
    )

  return(elsa_representation_plot) # Return the completed plot
}

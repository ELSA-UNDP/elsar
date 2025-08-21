#' Function to easily adapt the visual appearance of a plot
#'
#' `elsar_plot_optics()` allows to use specific themes for plotting and add north arrows and scales if wanted.
#'
#' @param type A character denoting whether "ggplot" or "tmap" is being used. Needs to match the main plot
#' @param theme For `ggplot` this allows to use a pre-defined "deafult" plot theme or provide a custom one as a `list`.
#' @param include_north_scale Logical. Determines whether a north arrow and scale should be added.
#' @param include_logo Logical. Whether to include a logo (.png image) in the plot
#' @param logo_path A path to where the `png` file of the logo is saved (e.g. "man/figures/elsaR_hex_sticker.png")
#' @param logo_dim A vector with desired width and height of the image. Default is c(60, 60).
#' @param logo_pos A vector of the desired position of the image (xmin, xmax, ymin, ymax).
#' @param include_text Logical. Whether to include text in the plot (e.g. website).
#' @param text_to_display The text to display. Dafult is "UNBL | www.unbiodiversitylab.org",
#' @param text_location The location of the text. Options are "bottom_right" (default), "top_right", "top_left" and "bottom_left".
#' @param move_horizontal After setting a text location, this attribute allows to move the text block horizontally if needed.
#' @param move_vertical After setting a text location, this attribute allows to move the text block vertically if needed.
#'
#' @return For `ggplot` returns a list that can simply be added to a `ggplot`, e.g. [elsar_plot_static_raster_c()].
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#' wad_plot <- testPlot <- elsar_plot_static_raster_c(
#'   raster_in = wadOut,
#'   type = "ggplot_raster",
#'   background = wad_dat,
#'   legend_title = "wad"
#' ) + elsar_plot_optics()
elsar_plot_optics <- function(type = "ggplot",
                              theme = "default",
                              include_north_scale = FALSE,
                              include_logo = FALSE,
                              logo_path = "man/figures/elsaR_hex_sticker.png", #change later
                              logo_dim = c(60, 60),
                              logo_pos,
                              include_text = TRUE,
                              text_to_display = "UNBL | www.unbiodiversitylab.org",
                              text_location = "bottom_right",
                              move_horizontal = NULL,
                              move_vertical = NULL
                              ) {
  # ggplot
  if (type == "ggplot") {
    ggList <- list()
    # scale bar and north arrow
    if (include_north_scale) {
      ggList <- c(
        ggList,
        list(
          ggspatial::annotation_scale(location = "bl", width_hint = 0.4),
          ggspatial::annotation_north_arrow(
            location = "bl", # which_north = "true",
            height = ggplot2::unit(1, "cm"),
            width = ggplot2::unit(1, "cm"),
            pad_x = ggplot2::unit(0.05, "in"),
            pad_y = ggplot2::unit(0.2, "in"),
            style = ggspatial::north_arrow_orienteering(text_size = 0)
          )
        )
      )
    }

    # logo
    if (include_logo) {
      img <- png::readPNG(logo_path)
      g <- grid::rasterGrob(img, interpolate=TRUE,
                            width = grid::unit(logo_dim[1], "pt"),
                             height = grid::unit(logo_dim[2], "pt"))
      ggList <- c(
        ggList,
        list(ggplot2::annotation_custom(g,
                                        xmin = logo_pos[1], xmax = logo_pos[2],
                                        ymin = logo_pos[3], ymax = logo_pos[4])
        )
      )
    }

    # logo
    if (include_text) {

      if (text_location == "bottom_right") {
        annotations <- data.frame(
          xpos = c(Inf),
          ypos = c(-Inf),
          annotateText = text_to_display,
          hjustvar = c(1),
          vjustvar = c(0)
        )
      } else if (text_location == "bottom_left") {
        annotations <- data.frame(
          xpos = c(-Inf),
          ypos = c(-Inf),
          annotateText = text_to_display,
          hjustvar = c(0),
          vjustvar = c(0)
        )
      } else if (text_location == "top_right") {
        annotations <- data.frame(
          xpos = c(Inf),
          ypos = c(Inf),
          annotateText = text_to_display,
          hjustvar = c(1),
          vjustvar = c(1)
        )
      } else if (text_location == "top_left") {
        annotations <- data.frame(
          xpos = c(-Inf),
          ypos = c(Inf),
          annotateText = text_to_display,
          hjustvar = c(0),
          vjustvar = c(1)
        )
      }

      #allow for manual adjustments when needed
      if (!is.null(move_horizontal)) {
        hjustvar = move_horizontal
      }

      if (!is.null(move_vertical)) {
        vjustvar = move_vertical
      }

      ggList <- c(
        ggList,
        list(
          ggplot2::geom_label(
            data = annotations,
            ggplot2::aes(
              x = .data$xpos, y = .data$ypos,
              hjust = hjustvar, vjust = vjustvar,
              label = .data$annotateText
            ),
            label.r = ggplot2::unit(0.01, "lines"),
            size = 3
          )
        )
      )
    }

    # theme
    if (theme == "default") {
      ggList <- c(
        ggList,
        list(
          ggplot2::theme_bw(),
          ggplot2::theme(
            legend.position = "right",
            legend.direction = "vertical",
            text = ggplot2::element_text(size = 9, colour = "black"),
            axis.text = ggplot2::element_text(size = 9, colour = "black"),
            plot.title = ggplot2::element_text(size = 9),
            axis.title = ggplot2::element_blank()
          )
        )
      )
    } else if (inherits(theme, "list")) {
      ggList <- c(ggList, theme)
    }
  } else { # tmap
    message("not added yet")
  }
}

#' Function to add additional data to an existing plot
#'
#' `elsar_plot_extra_data()` allows to add extra data to an existing `ggplot` or `tmap` plot, e.g. protected areas generated with [make_protected_areas()]. Function idea taken from [spatialplanr](https://mathmarecol.github.io/spatialplanr/) package.
#'
#' @param plot_type A character denoting whether "ggplot" or "tmap" is being used. Needs to match the main plot
#' @param include_pas An `sf` object with the PAs in the area, e.g. created with the [make_protected_areas()] function with the setting  return_sf = TRUE.
#' @param pas_look A character denoting whether to show "contours" or "area" of PAs.
#' @param alpha_pa A value (0-1) for the opacity of the locked in areas when plotted on top of other plots.
#' @param color_pa A color value for the locked in areas.
#' @param legend_pa A character value for the title of the legend of the locked in areas. Can be empty ("").
#' @param label_pa The legend label of the locked in area (e.g. MPAs)
#'
#' @return For `ggplot` returns a list that can simply be added to a `ggplot`, e.g. [elsar_plot_static_raster_c()].
#' @export
#'
#' @examples
#' \dontrun{
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#'
#' current_pas <- make_protected_areas(
#'   iso3 = "NPL",
#'   download_path = here::here(),
#'   buffer_points = TRUE,
#'   return_sf = TRUE,
#'   pus = pus
#' )
#'
#' wad_plot <- testPlot <- elsar_plot_static_raster_c(
#'   raster_in = wadOut,
#'   type = "ggplot_raster",
#'   background = wad_dat,
#'   legend_title = "wad"
#' ) +
#'   elsar_plot_extra_data(include_pas = current_pas, color_pa = "red") +
#'   elsar_plot_optics()
#'
#' wad_plot <- testPlot <- elsar_plot_static_raster_c(
#'   raster_in = wadOut,
#'   type = "ggplot_raster",
#'   background = wad_dat,
#'   legend_title = "wad"
#' ) +
#'   elsar_plot_extra_data(include_pas = current_pas, pas_look = "area", color_pa = "grey") +
#'   elsar_plot_optics()
#' }
elsar_plot_extra_data <- function(plot_type = "ggplot",
                                  # boundary_ctry = NULL, # add later
                                  include_pas = NULL, pas_look = "contours", # or area
                                  alpha_pa = 0.5, color_pa = "black",
                                  legend_pa = "", label_pa = "PAs"
                                  # other_locked_in = NULL
) {
  ggList <- list()

  if (inherits(include_pas, "sf")) {
    message("Adding PA layer.")

    if (plot_type == "ggplot") {
      if (pas_look == "area") {
        lockedInAreas <- include_pas %>%
          dplyr::mutate(lockedIn = 1) %>%
          dplyr::mutate(lockedIn = as.logical(.data$lockedIn))

        ggList <- c(
          ggList,
          list(
            ggnewscale::new_scale_fill(),
            ggnewscale::new_scale_colour(),
            ggplot2::geom_sf(
              data = lockedInAreas, ggplot2::aes(fill = .data$lockedIn),
              alpha = alpha_pa
            ),
            ggplot2::scale_fill_manual(
              name = legend_pa,
              values = c("TRUE" = color_pa),
              labels = label_pa,
              aesthetics = c("colour", "fill"),
              guide = ggplot2::guide_legend(
                override.aes = list(linetype = 0),
                nrow = 2,
                order = 1,
                direction = "horizontal",
                title.position = "top",
                title.hjust = 0.5
              )
            )
          )
        )
      } else if (pas_look == "contours") {
        lockedInAreas <- include_pas %>%
          sf::st_union() %>%
          sf::st_as_sf() %>%
          dplyr::rename(geometry = "x") %>%
          dplyr::mutate(lockedIn = 1) %>%
          dplyr::mutate(lockedIn = as.factor(.data$lockedIn))

        ggList <- c(
          ggList,
          list(
            ggnewscale::new_scale_fill(),
            ggnewscale::new_scale_colour(),
            ggplot2::geom_sf(
              data = lockedInAreas, colour = color_pa,
              fill = NA, ggplot2::aes(linetype = .data$lockedIn),
              size = 0.9, show.legend = "line"
            ),
            ggplot2::scale_linetype_manual("",
              values = 1,
              labels = label_pa,
              guide = ggplot2::guide_legend(
                override.aes = list(fill = NA),
                direction = "horizontal",
                keywidth = grid::unit(0.05, "npc")
              )
            )
          )
        )
      }
    }
  }
}

#' Function to create a plot with continuous background data for another plot
#'
#' @param plot_type  A character denoting whether "ggplot" or "tmap" is being used. Needs to match the main plot
#' @param background_dat A `SpatRaster` file that contains the data to be used as a background.
#' @param rescale_background Logical. If TRUE, rescales the `SpatRaster` to values between 0-1.
#' @param increase_extend A numerical value that allows to extend the background beyond the extent of `raster_in`. If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%). If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param main_data A `SpatRaster` file that contains the data that will be the main part of the main plot.
#' @param background_alpha A value (0-1) for the opacity of the locked in areas when plotted on top of other plots.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param NA_replace Logical. Whether to use na.omit() or not
#' @param layer_name A string of characters denoting the layer of interest. If nothing is provided (NULL), will use default.
#'
#' @return  A `list` of a `ggplot` or `tmap` object and a `SpatRaster` with the new background data.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#'
#' (background_plot <- elsar_plot_background_c(
#'   background_dat = wad_dat,
#'   main_data = wadOut
#' ))
elsar_plot_background_c <- function(plot_type = "ggplot",
                                    background_dat = NULL, # SpatRaster file
                                    layer_name = NULL,
                                    rescale_background = TRUE,
                                    NA_replace = TRUE,
                                    increase_extend = 0.05,
                                    main_data = NULL, # SpatRaster
                                    background_alpha = 0.2,
                                    color_map = "viridis",
                                    custom_palette = NULL) {
  assertthat::assert_that(inherits(background_dat, "SpatRaster"))
  message("Adding background layer.")

  # reproject
  bckgrnd_dat <- terra::project(background_dat, terra::crs(main_data))

  if (NA_replace) {
    bckgrnd_dat <- bckgrnd_dat %>%
      terra::na.omit()
  }

  if (!is.null(layer_name)) {
    background_dat <- background_dat %>% terra::subset(layer_name)
  }

  # rescale if wanted
  if (rescale_background) {
    bckgrnd_dat <- rescale_raster(bckgrnd_dat)
  }

  # crop data to a specifc extend around main data
  if (!is.null(increase_extend)) {
    bckgrnd_dat <- elsar_extend(
      raster_main = main_data,
      raster_to_crop = bckgrnd_dat,
      extend_by = increase_extend
    )
  }

  # plot
  if (plot_type == "ggplot") {
    col_interest <- terra::names(bckgrnd_dat)

    plot_background <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = bckgrnd_dat)

    if (is.null(custom_palette)) {
      plot_background <- plot_background +
        ggplot2::scale_colour_viridis_c(
          option = color_map, alpha = background_alpha,
          guide = "none",
          na.value = "transparent"
        ) +
        ggplot2::scale_fill_viridis_c(
          option = color_map, alpha = background_alpha,
          guide = "none",
          na.value = "transparent"
        )
    } else {
      plot_background <- plot_background + custom_palette
    }
    # prep for adding more data later
    plot_background <- plot_background +
      ggnewscale::new_scale_fill() +
      ggnewscale::new_scale_colour()
  } else if (plot_type == "tmap") {
    message("Will be added later.")
  }

  return(list(plot_background, bckgrnd_dat))
}

#' Function to create a plot with discrete background data for another plot
#'
#' @param plot_type  A character denoting whether "ggplot" or "tmap" is being used. Needs to match the main plot
#' @param background_dat A `SpatRaster` file that contains the data to be used as a background.
#' @param rescale_background Logical. If TRUE, rescales the `SpatRaster` to values between 0-1.
#' @param increase_extend A numerical value that allows to extend the background beyond the extent of `raster_in`. If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%). If extend_background > 1 all sides will be extended by the absolute value provided.
#' @param main_data A `SpatRaster` file that contains the data that will be the main part of the main plot.
#' @param background_alpha A value (0-1) for the opacity of the locked in areas when plotted on top of other plots.
#' @param color_map The name of the `viridis` palette to be used. Default is "viridis".
#' @param categorical logical. if data is categorical (TRUE), convert to factor (if not yet) and use the number of categories given.
#' @param custom_palette An optional custom palette for plotting. Default uses the `viridis` package.
#' @param number_categories If data does not have pre-defined categories, how many categories to split the continuous data into
#' @param data_layer The data layer with continuous data to be converted into categories.
#'
#' @return  A `list` of a `ggplot` or `tmap` object and a `SpatRaster` with the new background data.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#' (background_plot <- elsar_plot_background_d(
#'   background_dat = wad_dat,
#'   main_data = wadOut,
#'   increase_extend = 0.05,
#'   number_categories = 10,
#'   data_layer = "wad_final_cog",
#' ))
elsar_plot_background_d <- function(plot_type = "ggplot",
                                    background_dat = NULL, # SpatRaster file
                                    rescale_background = TRUE,
                                    increase_extend = 0.05,
                                    main_data = NULL, # SpatRaster
                                    background_alpha = 0.2,
                                    color_map = "viridis",
                                    custom_palette = NULL,
                                    categorical = FALSE,
                                    number_categories = 10,
                                    data_layer = NULL) {
  assertthat::assert_that(inherits(background_dat, "SpatRaster"))
  message("Adding background layer.")

  # reproject
  bckgrnd_dat <- terra::project(background_dat, terra::crs(main_data))

  # rescale if wanted
  if (rescale_background) {
    bckgrnd_dat <- rescale_raster(bckgrnd_dat)
  }

  # crop data to a specifc extend around main data
  if (!is.null(increase_extend)) {
    bckgrnd_dat <- elsar_extend(
      raster_main = main_data,
      raster_to_crop = bckgrnd_dat,
      extend_by = increase_extend
    )
  }

  # categorize

  if (categorical) {
    # message("Plotting input data that is already categorical.")

    assertthat::assert_that(
      terra::is.factor(bckgrnd_dat),
      msg = "Input is not a factor."
    )

    # get number of categories
    number_categories <- length(terra::levels(bckgrnd_dat)[[1]][[1]])

    bckgrnd_dat <- as.data.frame(bckgrnd_dat, xy = TRUE) %>%
      stats::na.omit() %>%
      dplyr::rename(interval = .data[[data_layer]]) %>%
      dplyr::mutate(interval = as.factor(.data$interval))
  } else {
    # message("Plotting input data that is continuous and will be split into categories.")

    assertthat::assert_that(
      !is.null(number_categories),
      msg = "Provide a valid number of categories to split your data into."
    )

    raster_cat <- elsar_continuous_to_categorical(
      raster_in = bckgrnd_dat,
      data_layer = data_layer,
      number_categories = number_categories,
      hist_breaks_out = FALSE
    )

    bckgrnd_dat <- raster_cat %>%
      dplyr::mutate(
        category = as.factor(.data$category),
        interval = as.factor(.data$interval)
      )
  }

  # plot
  if (plot_type == "ggplot") {
    # col_interest <- terra::names(bckgrnd_dat)
    #
    # bckgrnd_dat <- as.data.frame(bckgrnd_dat, xy = TRUE) %>%
    #   stats::na.omit()

    plot_background <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = bckgrnd_dat, ggplot2::aes(
        y = .data$y, x = .data$x,
        fill = .data$interval
      ), show.legend = FALSE)

    if (is.null(custom_palette)) {
      plot_background <- plot_background +
        ggplot2::scale_colour_viridis_d(
          option = color_map, alpha = background_alpha,
          guide = "none",
          expand = c(0, 0)
        ) +
        ggplot2::scale_fill_viridis_d(
          option = color_map, alpha = background_alpha,
          guide = "none",
          expand = c(0, 0)
        )
    } else {
      plot_background <- plot_background + custom_palette
    }
    # prep for adding more data later
    plot_background <- plot_background +
      ggnewscale::new_scale_fill() +
      ggnewscale::new_scale_colour()
  } else if (plot_type == "tmap") {
    message("Will be added later.")
  }

  return(list(plot_background, bckgrnd_dat))
}


#' Function to extend the spatial extend around a raster file for plotting
#'
#' When plotting (e.g. with [elsar_plot_static_raster_c()], the extent of the main data will be used. `elsar_extend()` allows to extract background data to be plotted with an extent that is a bit greater than the extent of the main data.
#'
#' @param raster_main A `SpatRaster` file of the main data of the original plot. The extent of this file will be used as the baseline for the new plot extent.
#' @param raster_to_crop A `SpatRaster` file of the data that the will be cropped using the new extent.
#' @param extend_by A numerical value that allows to extend the background beyond the extent of `raster_in`. If extend_background <= 1, the lat and lon extend will be extended by the ratio provided (e.g. 0.05 will extend it by 5%). If extend_background > 1 all sides will be extended by the absolute value provided.
#'
#' @return A `SpatRaster` file with the new dimensions.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#' )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#' )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#' )
#' wad_dat <- terra::project(wad_dat, terra::crs(wadOut))
#'
#' bckgrnd_dat <- elsar_extend(
#'   raster_main = wadOut,
#'   raster_to_crop = wad_dat,
#'   extend_by = 0.05
#' )
elsar_extend <- function(raster_main = NULL,
                         raster_to_crop = NULL,
                         extend_by = 0.05 # or 1000 or
) {
  assertthat::assert_that(
    inherits(raster_main, "SpatRaster"),
    inherits(raster_to_crop, "SpatRaster")
  )

  assertthat::is.number(extend_by)

  if (extend_by <= 1) {
    message("Extend based on ratio.")

    extent <- as.vector(terra::ext(raster_main))

    addX <- (extent[2] - extent[1]) * extend_by
    addY <- (extent[4] - extent[3]) * extend_by
    newExtent <- c(
      extent[1] - addX,
      extent[2] + addX,
      extent[3] - addY,
      extent[4] + addY
    )

    e <- terra::ext(newExtent)

    raster_out <- terra::crop(raster_to_crop, e)
  } else if (extend_by > 1) {
    message("Extend based on absolute values.")

    extent <- as.vector(terra::ext(raster_main))

    newExtent <- c(
      extent[1] - extend_by,
      extent[2] + extend_by,
      extent[3] - extend_by,
      extent[4] + extend_by
    )

    e <- terra::ext(newExtent)

    raster_out <- terra::crop(raster_to_crop, e)
  }

  return(raster_out)
}

#' Function to create categories for plotting out of continuous data
#'
#' @param raster_in The `SpatRaster` file to be plotted.
#' @param data_layer The data layer with continuous data to be converted into categories.
#' @param number_categories Number of categories to create from continuous data
#' @param manual_breaks A vector with breaks to be used as categories.
#' @param hist_breaks_out logical. If TRUE (default), returns the breaks used to categorise data.
#'
#' @return A list with a `df` that has categories and their interval labels, as well as the interval breaks for background data.
#' @export
#'
#' @examples
#' boundary_proj <- make_boundary(
#'   boundary_in = boundary_dat,
#'   iso3 = "NPL",
#'   iso3_column = "iso3cd"
#'   )
#'
#' pus <- make_planning_units(
#'   boundary_proj = boundary_proj,
#'   pu_size = NULL,
#'   pu_threshold = 8.5e5,
#'   limit_to_mainland = FALSE
#'   )
#' wad_dat <- get_wad_data()
#'
#' wadOut <- make_normalised_raster(
#'   raster_in = wad_dat,
#'   pus = pus,
#'   iso3 = "NPL"
#'   )
#'
#' wad_cat <- elsar_continuous_to_categorical(
#'  wadOut,
#'  data_layer = "Planning.Units",
#'  number_categories = 10
#' )
elsar_continuous_to_categorical <- function(raster_in,
                                            data_layer,
                                            number_categories,
                                            manual_breaks = NULL,
                                            hist_breaks_out = TRUE) {
  raster_df <- as.data.frame(raster_in, xy = TRUE) %>% # ADD this with terra::hist later on to export a SpatRaster
    stats::na.omit()

  # get the categories
  if (is.null(manual_breaks)) {
    hist_breaks <- graphics::hist(raster_df[[data_layer]],
      breaks = seq(min(raster_df[[data_layer]]),
        max(raster_df[[data_layer]]),
        length.out = number_categories + 1
      ), plot = FALSE
    )$breaks
  } else {
    hist_breaks <- manual_breaks
  }

  # apply categories to data
  raster_df <- raster_df %>%
    dplyr::mutate(category = cut(raster_df[[data_layer]],
      breaks = hist_breaks,
      include.lowest = TRUE,
      labels = FALSE
    ))

  # create extra column with labels
  interval_labels <- paste0(
    round(utils::head(hist_breaks, -1), 2), "-",
    round(utils::tail(hist_breaks, -1), 2)
  )

  raster_df <- raster_df %>%
    dplyr::mutate(interval = interval_labels[raster_df$category])

  if (hist_breaks_out) {
    return(list(raster_df, hist_breaks))
  } else {
    return(raster_df)
  }
}

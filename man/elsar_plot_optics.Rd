% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-plotting.R
\name{elsar_plot_optics}
\alias{elsar_plot_optics}
\title{Function to easily adapt the visual appearance of a plot}
\usage{
elsar_plot_optics(
  type = "ggplot",
  theme = "default",
  include_north_scale = FALSE,
  include_logo = FALSE,
  logo_path = "man/figures/elsaR_hex_sticker.png",
  logo_dim = c(60, 60),
  logo_pos,
  include_text = TRUE,
  text_to_display = "UNBL | www.unbiodiversitylab.org",
  text_location = "bottom_right",
  move_horizontal = NULL,
  move_vertical = NULL
)
}
\arguments{
\item{type}{A character denoting whether "ggplot" or "tmap" is being used. Needs to match the main plot}

\item{theme}{For \code{ggplot} this allows to use a pre-defined "deafult" plot theme or provide a custom one as a \code{list}.}

\item{include_north_scale}{Logical. Determines whether a north arrow and scale should be added.}

\item{include_logo}{Logical. Whether to include a logo (.png image) in the plot}

\item{logo_path}{A path to where the \code{png} file of the logo is saved (e.g. "man/figures/elsaR_hex_sticker.png")}

\item{logo_dim}{A vector with desired width and height of the image. Default is c(60, 60).}

\item{logo_pos}{A vector of the desired position of the image (xmin, xmax, ymin, ymax).}

\item{include_text}{Logical. Whether to include text in the plot (e.g. website).}

\item{text_to_display}{The text to display. Dafult is "UNBL | www.unbiodiversitylab.org",}

\item{text_location}{The location of the text. Options are "bottom_right" (default), "top_right", "top_left" and "bottom_left".}

\item{move_horizontal}{After setting a text location, this attribute allows to move the text block horizontally if needed.}

\item{move_vertical}{After setting a text location, this attribute allows to move the text block vertically if needed.}
}
\value{
For \code{ggplot} returns a list that can simply be added to a \code{ggplot}, e.g. \code{\link[=elsar_plot_static_raster_c]{elsar_plot_static_raster_c()}}.
}
\description{
\code{elsar_plot_optics()} allows to use specific themes for plotting and add north arrows and scales if wanted.
}
\examples{
boundary_proj <- make_boundary(
  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd"
)

pus <- make_planning_units(
  boundary_proj = boundary_proj,
  pu_size = NULL,
  pu_threshold = 8.5e5,
  limit_to_mainland = FALSE
)
wad_dat <- get_wad_data()

wadOut <- make_normalised_raster(
  raster_in = wad_dat,
  pus = pus,
  iso3 = "NPL"
)
wad_plot <- testPlot <- elsar_plot_static_raster_c(
  raster_in = wadOut,
  type = "ggplot_raster",
  background = wad_dat,
  legend_title = "wad"
) + elsar_plot_optics()
}

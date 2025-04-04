#' Wrapper function to get a raster file with all wanted features
#'
#' @param feature_list A `list` with all features that are wanted for the downstream ELSA workflow.
#' @param path_in A common path where all the data can be found.
#' @param pus A `SpatRaster` file that contains the reference spatial extent, crs etc.in form of the planning units
#' @param iso3 A string of the iso3 name of the data (country name)
#' @param output_path An optional output path for all created files.
#' @param ... Additional attributes needed for some functions if wanted.
#'
#' @return A `SpatRaster` with the length of the feature_list + 1 since the planning units are the first layer.
#' @export
#'
#' @examples
#' \dontrun{
#' feature_list <- c("fml", "cropsuit", "fi")
#'
#' all_feats <- get_all_features(
#'   feature_list = feature_list,
#'   path_in = "<yourpath>",
#'   pus = pus,
#'   iso3 = "NPL"
#'   )
#' }
get_all_features <- function (
    feature_list, # needs lyr info added in somewhere
    path_in,
    pus,
    iso3,
    output_path = NULL,
    ...) {
  # init out raster
  raster_out <- pus

  #probs could write a loop or purrr around all the make_normalised raster data sets to shorten this function
  if (any(c("FML", "fml", "certified_forests", "Certified_Forests") %in% feature_list)) {
    message("Processing certified forests data (FML)")

    list_dat <- extract_filename_filetype(
      data_name = "FML", #currently hard-coded; maybe change later; or make dictionary input that gives all the data options and takes e.g. certified_forests = "FML" or NULL when data not wanted.
      file_path = path_in)

    raster_fml <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    fml <- make_normalised_raster(
      raster_in = raster_fml,
      pus = pus,
      iso3 = iso3,
      output_path = output_path,
      name_out = "FML"
      )

    raster_out <- c(raster_out, fml)
  }

  if (any(c("cropsuit", "crop_suitability", "crop_suitability_difference") %in% feature_list)) {
    message("Processing crop suitability difference data (cropsuit)")

    list_dat <- extract_filename_filetype(
      data_name = "cropsuit", #currently hard-coded; maybe change later; or make dictionary input that gives all the data options and takes e.g. certified_forests = "FML" or NULL when data not wanted.
      file_path = path_in
      )

    raster_cropsuit <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    cropsuit <- make_normalised_raster(
      raster_in = raster_cropsuit,
      pus = pus,
      iso3 = iso3,
      invert = TRUE,
      output_path = output_path,
      name_out = "cropsuit"
      )

    raster_out <- c(raster_out, cropsuit)
  }

  if (any(c("drought_risk", "Drought_Risk") %in% feature_list)) {
    message("Processing drought risk data")

    list_dat <- extract_filename_filetype(data_name = " ", #currently hard-coded; maybe change later; or make dictionary input that gives all the data options and takes e.g. certified_forests = "FML" or NULL when data not wanted.
                                          file_path = path_in)

    raster_dr <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    dr <- make_normalised_raster(
      raster_in = raster_dr,
      pus = pus,
      iso3 = iso3,
      output_path = output_path,
      name_out = " "
      )

    raster_out <- c(raster_out, dr)
  }

  if (any(c("fi", "forest_integrity", "Forest_Integrity") %in% feature_list)) {
    message("Processing forest integrity data")

    list_dat <- extract_filename_filetype(
      data_name = "flii",
      file_path = path_in
      )

    raster_flii <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    list_dat <- extract_filename_filetype(
      data_name = "fsii",
      file_path = path_in
      )

    raster_fsii <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    fi <- make_forest_integrity(
      raster_flii = raster_flii,
      raster_fsii = raster_fsii,
      pus = pus,
      output_path = output_path
      )

    raster_out <- c(raster_out, fi)
  }

  if (any(c("soc_at_risk", "vulnerable_soc", "soc") %in% feature_list)) {
    message("Processing vulnerable soil organic carbon difference data")

    list_dat <- extract_filename_filetype(
      data_name = "soc", #currently hard-coded; maybe change later; or make dictionary input that gives all the data options and takes e.g. certified_forests = "FML" or NULL when data not wanted.
      file_path = path_in
      )

    raster_soc <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    soc <- make_normalised_raster(
      raster_in = raster_soc,
      pus = pus,
      iso3 = iso3,
      output_path = output_path,
      name_out = "soc_at_risk"
      )

    raster_out <- c(raster_out, soc)
  }

  if (any(c("wad") %in% feature_list)) {
    message("Processing WAD convergence evidence data")

    list_dat <- extract_filename_filetype(
      data_name = "wad", #currently hard-coded; maybe change later; or make dictionary input that gives all the data options and takes e.g. certified_forests = "FML" or NULL when data not wanted.
      file_path = path_in
      )

    raster_wad <- elsar_load_data(
      file_name = list_dat["filename"][[1]],
      file_path = path_in
      )

    wad <- make_normalised_raster(
      raster_in = raster_wad,
      pus = pus,
      iso3 = iso3,
      output_path = output_path,
      name_out = "wad"
      )

    raster_out <- c(raster_out, wad)
  }

  return(raster_out)
}


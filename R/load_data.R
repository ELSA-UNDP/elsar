#' Load different types of data
#'
#' @param file_name character of the file name. Needs to contain the file type ending (e.g. `.tif`) if loading from a local source.
#' @param file_path path where local file is stored. Needs to be `NULL` when using postgres.
#' @param file_lyr optional. Layer information of data.
#' @param file_type character of file type. Current options are: "postgres", "shp", "gpkg", "geojson", "tif", "tiff", "grd", "gri", "nc", "hdf"
#' @param db_info list in the style of a dictionary. Only needed when file_type = "postgres". Needs to have the following structure and information: postgres_dict <- c(host = "<yourhost>", dbname ="<yourdbname>", port = <portNumber>, user = "<yourusername>", password = "<yourpassword>")
#' @param iso3_column Only relevant when `file_type` "postgres" is selected. A string of the name of where iso3 information can be found in a dataset.
#' @param iso3 The iso3 country code (character) of the country of interest.
#'
#' @return The loaded data either as a `SpatRaster` or `sf` object
#' @export
#'
#' @examples
#' \dontrun{
#' load_tif <- load_data(file_name = "pu_nepal_450m.tif", file_path = localPath
#'                           file_type = "tif")
#'
#' load_geojson <- load_data(file_name = "nepal.geojson", file_path = localPath,
#'                           file_type = "geojson")
#'
#' postgres_dict <- c(host = "<yourhost>",
#'                    dbname ="<yourdbname>",
#'                    port = <portNumber>,
#'                    user = "<yourusername>",
#'                    password = "<yourpassword>")
#' load_postgres <- load_data(file_name = "bnda_simplified",file_type = "postgres",
#'                            db_info = postgres_dict,
#'                            iso3_column = "iso3cd",
#'                            iso3 = "NPL")
#' #' }
load_data <- function(file_name,
                      file_path = NULL,
                      file_lyr = NULL,
                      file_type,
                      db_info,
                      iso3_column = "iso3cd", # "iso_sov1",
                      iso3) {
  # create path to data
 if (is.null(file_path) & file_type == "postgres") {
    con <- RPostgres::dbConnect(RPostgres::Postgres(),
      host = db_info["host"][[1]],
      dbname = db_info["dbname"][[1]],
      port = db_info["port"][[1]],
      user = db_info["user"][[1]],
      password = db_info["password"][[1]]
    )
    loaded_data <- sf::st_read(
      dsn = con,
      query = glue::glue("SELECT * FROM {file_name} WHERE {iso3_column} = '{iso3}'")
    )
 } else if (!is.null(file_path) & file_type != "postgres") {
   to_load <- file.path(file_path, file_name)

   if (file_type %in% c("shp", "gpkg", "geojson")) {
     if (!is.null(file_lyr)) {
       loaded_data <- sf::read_sf(to_load, layer = file_lyr)
     } else {
       loaded_data <- sf::read_sf(to_load)
     }
   } else if (file_type %in% c("tif", "tiff", "grd", "gri", "nc", "hdf")) { #havent tested nc and hdf yet
     if (!is.null(file_lyr)) {
       loaded_data <- terra::rast(to_load, lyrs = file_lyr)
     } else {
       loaded_data <- terra::rast(to_load)
     }
   } else {
     message("Selected file_type might not be provided yet. Please add an
            issue on , so we can add it to this function. In the meantime,
            please load your data outside this function.")
   }

 } else {
    message("Please provide a file path for your local data.
            Remote accessing is currently only supported through postgres.
            If you wish to use postgres, set file_type = 'postgress'.")
  }

  return(loaded_data)
}

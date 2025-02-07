##### SCRIPT CURRENTLY NOT WORKING; JUST TRYING OUT INDIVIDUAL CODE SECTIONS

library(devtools)
load_all()
#library(elsar)
library(sf)
library(tidyverse)
library(exactextractr)
library(terra)
library(tidyterra)
library(here)
library(RPostgres)

#################################################################################

# Set local temp directory for `terra`
# terra::terraOptions(tempdir = here::here("~/Documents/"), steps = 4, todisk = TRUE)
# terraOptions()
# terra::tmpFiles(remove = TRUE)

# Set paths for data
input_path <- "C:/Users/sandr/Documents/Github/elsar"
output_path <- "C:/Users/sandr/Documents/Github/elsar/output_dat"

## Country info
iso3 <- "NPL"
iso3_column <- "iso3cd"

# Set actions to do
#create_boundary <- 1 #now in csv
#create_PUs <- 1 #now in csv
#create_featureStack <- 1 #now in csv
create_zones <- 0
create_lockedIn <- 0

#################################################################################
# Load data file
data_info <- read_delim(file.path(input_path, "input_data.csv"),
                        delim = ";", escape_double = FALSE,
                        trim_ws = TRUE) %>%
  dplyr::filter(include == 1) %>%
  dplyr::mutate(full_name =
  dplyr::case_when(
    file_type != "postgres" ~ paste0(file_name, ".", file_type),
    file_type == "postgres" ~ file_name,
    file_type == NA  ~ file_name
  ),
  full_path =
    dplyr::case_when(
      (file_path == "default" | is.na(file_path)) ~ input_path,
      file_type == "postgres" ~ NA,
      (!is.na(file_path) & file_path != "default" & file_type != "postgres")  ~ file_path
    )
    )

# Postgres check
if (nrow(data_info %>%
         dplyr::filter(file_type == "postgres")) > 0) {
  {
    host <- readline(prompt = "Enter host: ");
    dbname <- readline(prompt = "Enter database name: ");
    port <- readline(prompt = "Enter port: ");
    user <- readline(prompt = "Enter user name: ");
    password <- readline(prompt = "Enter password: ");
  }

  postgres_dict <- elsar::make_postgres_connection(
    host = host,
    dbname = dbname,
    port = as.integer(port),
    user = user,
    password = password
  )
} else {
  postgres_dict <- NULL
}

# Create data
## PUs
if (data_info %>%
    dplyr::filter(group == "pus") %>%
    dplyr::pull(include)) {

  message("Creating Planning Units")

  ### Load data
  boundary_info <- data_info %>%
    dplyr::filter(group == "boundary")

  boundary_dat <- elsar::elsar_load_data(
      file_name = boundary_info$file_name,
      file_path = (if (boundary_info$file_path == "NULL") NULL else boundary_info$file_path), #need this otherwise NULL is read as character
      file_type = boundary_info$file_type,
      db_info = postgres_dict,
      iso3 = iso3,
      iso3_column = iso3_column
    )

  ### Create boundary
  if (as.logical(data_info %>%
      dplyr::filter(group == "boundary") %>%
      dplyr::pull(include))) {
    boundary_proj <- make_boundary(
      boundary_in = boundary_dat,
      iso3 = iso3,
      iso3_column = iso3_column
    )
  } else {
    boundary_proj <- boundary_dat
  }

  ### Create PUs
  defaults_pu <- data_info %>%
    dplyr::filter(group == "pus") %>%
    dplyr::pull(default_parameters) #get whether to use default values or not

  if (defaults_pu) { #use default values
    pus <- elsar::make_planning_units(boundary_proj = boundary_proj,
                               pu_size = NULL,
                               pu_threshold = 8.5e5,
                               limit_to_mainland = FALSE)
  } else if (!defaults_pu ) { # ask user to supply inputs manually (either in pop up or console)

    {
      pu_size <- readline(prompt = "Enter a custom planning unit size; can be NULL if maximum number of PUs is provided: ");
      pu_threshold <- readline(prompt = "Enter a maximum PU number; affects all subsequent run time: ");
      limit_to_mainland <- readline(prompt = "Enter whether to include only mainland; TRUE or FALSE: ");
    }

    if (pu_size == "NULL") {
      pu_size <- as.null(pu_size)
    } else {
      pu_size <- as.integer(pu_size)
    }

    pu_threshold <- as.integer(pu_threshold)
    limit_to_mainland <- as.logical(limit_to_mainland)

    pus <- make_planning_units(boundary_proj = boundary_proj,
                               pu_size = pu_size,
                               pu_threshold = pu_threshold,
                               limit_to_mainland = limit_to_mainland)
  } else {
    message("Default input needs to be 0 (use custom settings) or 1 (default values)")
  }

  ### Visualise PUs
  if (as.logical(data_info %>%
      dplyr::filter(group == "pus") %>%
      dplyr::pull(visualise))) {
    print("TBA")
  }

}

## Create feature stack

feature_list <- data_info %>%
  dplyr::filter(group == "feature",
                include == 1)

## Create zones
zones_list
zones_data <- data_info %>%
  dplyr::filter(group == "zone",
                include == 1)

## Create locked-in areas
lockedIn_list <- c("avail")

library(devtools)
load_all()
# library(elsar)
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
sheet_path <- "C:/Users/sandr/Documents/Github/elsar"
input_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/input_dat"
output_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/output_dat"

## Country info
iso3 <- "NPL"
iso3_column <- "iso3cd"

# Set actions to do
# create_boundary <- 1 #now in csv
# create_PUs <- 1 #now in csv
# create_featureStack <- 1 #now in csv
create_zones <- 0
create_lockedIn <- 0

#################################################################################
# Load data file
data_info <- read_delim(file.path(sheet_path, "input_data.csv"),
  delim = ";", escape_double = FALSE,
  trim_ws = TRUE
) %>%
  dplyr::filter(include == 1) %>%
  dplyr::mutate(
    full_name =
      dplyr::case_when(
        file_type != "postgres" ~ paste0(file_name, ".", file_type),
        file_type == "postgres" ~ file_name,
        file_type == NA ~ file_name
      ),
    full_path =
      dplyr::case_when(
        (file_path == "default" | is.na(file_path)) ~ input_path,
        file_type == "postgres" ~ NA,
        (!is.na(file_path) & file_path != "default" & file_type != "postgres") ~ file_path
      )
  )

# Postgres check
if (nrow(data_info %>%
  dplyr::filter(file_type == "postgres")) > 0) {
  {
    host <- readline(prompt = "Enter host: ")
    dbname <- readline(prompt = "Enter database name: ")
    port <- readline(prompt = "Enter port: ")
    user <- readline(prompt = "Enter user name: ")
    password <- readline(prompt = "Enter password: ")
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
    file_path = (if (boundary_info$file_path == "NULL") NULL else boundary_info$file_path), # need this otherwise NULL is read as character
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
    dplyr::pull(default_parameters) # get whether to use default values or not

  if (defaults_pu) { # use default values
    pus <- elsar::make_planning_units(
      boundary_proj = boundary_proj,
      pu_size = NULL,
      pu_threshold = 8.5e5,
      limit_to_mainland = FALSE
    )
  } else if (!defaults_pu) { # ask user to supply inputs manually (either in pop up or console)

    {
      pu_size <- readline(prompt = "Enter a custom planning unit size; can be NULL if maximum number of PUs is provided: ")
      pu_threshold <- readline(prompt = "Enter a maximum PU number; affects all subsequent run time: ")
      limit_to_mainland <- readline(prompt = "Enter whether to include only mainland; TRUE or FALSE: ")
    }

    if (pu_size == "NULL") {
      pu_size <- as.null(pu_size)
    } else {
      pu_size <- as.integer(pu_size)
    }

    pu_threshold <- as.integer(pu_threshold)
    limit_to_mainland <- as.logical(limit_to_mainland)

    pus <- make_planning_units(
      boundary_proj = boundary_proj,
      pu_size = pu_size,
      pu_threshold = pu_threshold,
      limit_to_mainland = limit_to_mainland
    )
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

## Create feature stack #could have one master script and then extra feature generation scripts etc that are sourced + ADD visualisation option
# Assign data to functions
feature_list <- data_info %>% # get all included features
  dplyr::filter(
    group == "feature",
    include == 1
  )

dat_default <- feature_list %>% # get those feature names that run with make_normalised_raster()
  dplyr::filter(default_function == 1) %>%
  dplyr::select("data_name") %>%
  dplyr::pull()

dat_non_default <- feature_list %>% # get those features that have their own function
  dplyr::filter(default_function == 0) %>%
  dplyr::select("data_name") %>%
  dplyr::pull()

## Prep data using default methods
cat("Creating Default Features")

# init data stack with pus as first layer
raster_out <- pus
for (i in 1:length(dat_default)) { # for all the data that runs with make_normalised_raster()
  current_dat <- feature_list %>%
    dplyr::filter(data_name == dat_default[[i]])

  print(dat_default[[i]])

  # load data
  current_rast <- elsar_load_data(
    file_name = current_dat$full_name,
    file_type = current_dat$file_type, file_path = current_dat$full_path
  )

  if (current_dat$default_parameters != 1) { # if non default parameters in make_normalised_raster()
    answer <- readline("Non-default option selected. Do you want to use pre-saved options? (yes/no): ")
    answer <- tolower(trimws(answer)) # to streamline if someone says "Yes" or similar

    if (answer == "yes") { # go through pre-saved options that we need to list down here. Will add to this
      if (current_dat$data_name == "Crop Suitability Difference") {
        rast_norm <- make_normalised_raster(
          raster_in = current_rast,
          pus = pus,
          iso3 = iso3,
          invert = TRUE,
          output_path = output_path,
          name_out = dat_default[[i]]
        )
      } else if (current_dat$data_name == "Yield Gap") {
        cat("TBA")
      } else {
        cat("Your data might not have a pre-saved option yet. Please enter your processing options manually.")
        next # jump to else if to enter options manually. Not tested yet!!!
      }
    } else if (answer == "no") { # manually add processing options
      cat("Please enter your processing options manually\n")

      {
        invert <- readline(prompt = "Invert data (TRUE/FALSE; Default FALSE): ")
        rescaled <- readline(prompt = "Rescale data (TRUE/FALSE; Default TRUE): ")
        method_override <- readline(prompt = "Override projection method (any valid terra projection method or NULL): ")
        conditional_expression <- readline(prompt = "Enter a conditional expression (or NULL if not needed): ")
      }

      if (method_override == "NULL") {
        method_override <- as.null(method_override)
      }

      rast_norm <- make_normalised_raster(
        raster_in = current_rast,
        pus = pus,
        iso3 = iso3,
        invert = as.logical(invert),
        rescaled = as.logical(rescaled),
        method_override = method_override,
        conditional_expression = eval(parse(text = conditional_expression)),
        output_path = output_path,
        name_out = dat_default[[i]]
      )
    } else {
      cat("Invalid input. Please enter 'yes' or 'no'.\n")
    }
  } else { # everything default
    rast_norm <- make_normalised_raster(
      raster_in = current_rast,
      pus = pus,
      iso3 = iso3,
      output_path = output_path,
      name_out = dat_default[[i]]
    )
  }

  # assign(current_dat$data_name, rast_norm)
  raster_out <- c(raster_out, rast_norm)
}

### Prep data using non-default methods
cat("Creating Non-Default Features")

for (j in 1:length(dat_non_default)) { # for all the data that runs with non-default functions

  current_dat <- feature_list %>%
    dplyr::filter(data_name == dat_non_default[[j]])

  print(dat_default[[j]])

  # # load data
  # current_rast <- elsar_load_data(
  #   file_name = current_dat$full_name,
  #   file_type = current_dat$file_type, file_path = current_dat$full_path
  # )

  if (j == "Mangroves") {
    print("Mangroves")

    current_rast <- elsar_load_data(
      file_name = current_dat$full_name,
      file_type = current_dat$file_type, file_path = current_dat$full_path,
      wkt_filter = TRUE,
      bb_extend = pus
    )

    mangrove_raster <- make_mangroves(
      sf_in = current_rast,
      pus = pus,
      iso3 = iso3,
      output_path = output_path,
      name_out = dat_default[[i]]
    )
    raster_out <- c(raster_out, mangrove_raster)
  }

  if (j == "Forest Integrity Index") { # add saving option
    print("Forest Integrity Index")

    if (grepl(",", current_dat$file_name)) { # check if both flii and fsii should be used
      fii_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))

      file_labels <- c("flii", "fsii")
      named_files <- setNames(
        fii_names[sapply(
          file_labels,
          function(x) {
            any(grepl(x,
              fii_names,
              ignore.case = TRUE
            ))
          }
        )],
        file_labels
      )

      # load data
      raster_flii <- elsar_load_data(
        file_name = paste0(named_files[["flii"]], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      raster_fsii <- elsar_load_data(
        file_name = paste0(named_files[["fsii"]], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      # forest integrity
      forest_integrity <- make_forest_integrity(
        raster_flii = raster_flii,
        raster_fsii = raster_fsii,
        pus = pus
      )
    } else if ((!(grepl(",", current_dat$file_name))) & (grepl("flii", current_dat$file_name))) {
      # load data
      raster_flii <- elsar_load_data(
        file_name = current_dat$full_name,
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      # forest integrity
      forest_integrity <- make_forest_integrity(
        raster_flii = raster_flii,
        pus = pus
      )
    } else if ((!(grepl(",", current_dat$file_name))) & (grepl("fsii", current_dat$file_name))) {
      # load data
      raster_fsii <- elsar_load_data(
        file_name = current_dat$full_name,
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      # forest integrity
      forest_integrity <- make_forest_integrity(
        raster_fsii = raster_fsii,
        pus = pus
      )
    }
    raster_out <- c(raster_out, forest_integrity)
  }

  if (j == "Existing PAs") {
    print("Current PAs")

    if (current_dat$default_parameters != 1) {
      answer <- readline("Do you want to download current protected areas from wdpar package? (yes/no): ")
      answer <- tolower(trimws(answer)) # to streamline if someone says "Yes" or similar

      if (answer == "yes") {
        from_wdpa <- TRUE
        download_path <- output_path
        sf_in <- NULL
      } else {
        # load data
        sf_in <- elsar_load_data(
          file_name = current_dat$full_name,
          file_type = current_dat$file_type, file_path = current_dat$full_path
        )

        from_wdpa <- FALSE
        download_path <- NULL
      }

      {
        status <- readline(prompt = "Included wdpa database status as a list (e.g. c('Established', 'Inscribed', 'Designated')): ")
        # pa_def <- readline(prompt = "Enter PA category (1: PA; 0: OECM): ") #Not supported yet
        designation_mab <- readline(prompt = "Include UNESCO MAB areas (TRUE/FALSE): ")
        buffer_points <- readline(prompt = "Create circular buffer around POINT geometries (TRUE/FALSE): ")
        area_column <- readline(prompt = "Column name for buffer calculations (e.g. REP_AREA): ")
        nQuadSegs <- readline(prompt = "Number of segments to use for buffering (e.g. 50): ")
      }

      current_pas <- make_protected_areas(
        from_wdpa = from_wdpa,
        sf_in = sf_in,
        iso3 = iso3,
        download_path = download_path,
        status = eval(parse(text = status)),
        pa_def = 1,
        designation_mab = as.logical(designation_mab),
        buffer_points = as.logical(buffer_points),
        area_column = area_column,
        nQuadSegs = as.integer(nQuadSegs),
        pus = pus,
        output_path = output_path
      )
    } else {
      current_pas <- make_protected_areas(
        iso3 = iso3,
        download_path = output_path,
        buffer_points = TRUE,
        pus = pus,
        output_path = output_path
      )
    }
  }
}


## Create zones
zones_list
zones_data <- data_info %>%
  dplyr::filter(
    group == "zone",
    include == 1
  )

## Create locked-in areas
lockedIn_list <- c("avail")

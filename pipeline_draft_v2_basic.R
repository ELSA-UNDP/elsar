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
"%ni%" <- Negate("%in%")

#################################################################################

# Set local temp directory for `terra`
# terra::terraOptions(tempdir = here::here("~/Documents/"), steps = 4, todisk = TRUE)
# terraOptions()
# terra::tmpFiles(remove = TRUE)

# Set paths for data
sheet_path <- "C:/Users/sandr/Documents/Github/elsar"
input_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/input_dat"
output_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/output_dat"
figure_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/output_dat/figures"

## Country info
iso3 <- "NPL"
iso3_column <- "iso3cd"

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
if (as.logical(as.integer(data_info %>%
  dplyr::filter(group == "pus") %>%
  dplyr::pull(include)))) {
  message("Creating Planning Units")

  ### Load data
  boundary_info <- data_info %>%
    dplyr::filter(group == "boundary")

  boundary_dat <- elsar::elsar_load_data(
    file_name = (if (boundary_info$file_type == "postgres") boundary_info$file_name else boundary_info$full_name),
    file_path = (if (boundary_info$file_path == "NULL") NULL else if (boundary_info$file_path == "default") input_path else boundary_info$file_path), # need this otherwise NULL is read as character
    file_type = boundary_info$file_type,
    db_info = postgres_dict,
    iso3 = iso3,
    iso3_column = iso3_column,
    file_lyr = (if (boundary_info$file_type != "postgres") boundary_info$layer else NULL)
  )

  ### Create boundary
  if (as.logical(as.integer(data_info %>%
    dplyr::filter(group == "boundary") %>%
    dplyr::pull(include)))) {
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

  elsar_plot_feature(
    raster_in = pus,
    pus = pus,
    no_legend = TRUE,
    legend_title = "PUs",
    figure_path = figure_path
  )
}

# clean env
rm(boundary_info, boundary_dat)

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

if ("Productive Managed Forests" %in% dat_non_default) { # managed forests and productive managed forests handled within same function
  include_productive <- TRUE
  dat_non_default <- dat_non_default[dat_non_default %ni% c("Productive Managed Forests")]
  if ("Managed Forests" %ni% dat_non_default) {
    dat_non_default <- append(dat_non_default, "Managed Forests") # Managed Forests always need to be there if productive managed forests are needed
  }
} else {
  include_productive <- FALSE
}

#### Prep data using default methods ####
cat("Creating Default Features")

# init data stack with pus as first layer
raster_out <- pus
for (i in 1:length(dat_default)) { # for all the data that runs with make_normalised_raster()
  current_dat <- feature_list %>%
    dplyr::filter(data_name == dat_default[[i]])

  # PUT AN IF STATEMENT HERE THAT CHECKS THE NAMES FOR THE COMMON NON-DEFAULT DATASETS AND GIVES A WARNING WHEN THEY ARE SET TO DEFAULT

  print(dat_default[[i]])

  # load data
  current_rast <- elsar_load_data(
    file_name = current_dat$full_name,
    file_type = current_dat$file_type, file_path = current_dat$full_path
  )

  if (current_dat$default_parameters != 1) { # if non default parameters in make_normalised_raster()

    if (current_dat$data_name == "Crop Suitability Difference" | current_dat$data_name == "Agricultural Climate Stress") {
      rast_norm <- make_normalised_raster(
        raster_in = current_rast,
        pus = pus,
        iso3 = iso3,
        conditional_expression = function(r) ifel(r > 0, 0, -r)
        # output_path = output_path,
        # name_out = dat_default[[i]]
      )
    } else if (current_dat$data_name == "Yield Gap") {
      rast_norm <- make_normalised_raster(
        raster_in = current_rast,
        pus = pus,
        iso3 = iso3,
        # output_path = output_path,
        # name_out = dat_default[[i]],
        conditional_expression = function(r) 100 - r # based on % (so: 100% - r)
      )
    } else {
      cat("Your data might not have a pre-saved option yet. Please enter your processing options manually.")
      next # jump to else if to enter options manually. Not tested yet!!!
    }
  } else { # everything default
    rast_norm <- make_normalised_raster(
      raster_in = current_rast,
      pus = pus,
      iso3 = iso3,
      # output_path = output_path,
      # name_out = dat_default[[i]]
    )
  }

  # assign(current_dat$data_name, rast_norm)
  names(rast_norm) <- c(dat_default[[i]]) # set layer name
  elsar_plot_feature(
    raster_in = rast_norm,
    pus = pus,
    legend_title = dat_default[[i]],
    figure_path = figure_path
  )
  raster_out <- c(raster_out, rast_norm)
}

#### Prep data using non-default methods ####
cat("Creating Non-Default Features")

for (j in 1:length(dat_non_default)) { # for all the data that runs with non-default functions

  current_dat <- feature_list %>%
    dplyr::filter(data_name == dat_non_default[[j]])

  if (dat_non_default[[j]] == "Mangroves") {
    print("Mangroves")

    current_rast <- elsar_load_data(
      file_name = current_dat$full_name,
      file_type = current_dat$file_type, file_path = current_dat$full_path,
      wkt_filter = TRUE,
      bb_extend = pus
    )

    if (nrow(current_rast) == 0) {
      cat("No mangroves in the planning region.")

      raster_out <- raster_out
    } else {
      mangrove_raster <- make_mangroves(
        sf_in = current_rast,
        pus = pus,
        iso3 = iso3,
        output_path = output_path,
        name_out = dat_non_default[[j]]
      )
      names(mangrove_raster) <- c(dat_non_default[[j]]) # set layer name
      elsar_plot_feature(
        raster_in = mangrove_raster,
        pus = pus,
        legend_title = dat_non_default[[j]],
        figure_path = figure_path
      )
      raster_out <- c(raster_out, mangrove_raster)
    }
  }

  if (dat_non_default[[j]] == "Forest Integrity Index") { # add saving option
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
    names(forest_integrity) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = forest_integrity,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, forest_integrity)
  }

  if (dat_non_default[[j]] == "Existing PAs") {
    print("Existing PAs")

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
    names(current_pas) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = current_pas,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, current_pas)
  }

  if (dat_non_default[[j]] == "Managed Forests") {
    print("Managed Forests")

    # load data
    raster_mf <- elsar_load_data(
      file_name = current_dat$full_name,
      file_type = current_dat$file_type, file_path = current_dat$full_path
    )

    if (include_productive) {
      current_dat <- feature_list %>%
        dplyr::filter(data_name == "Productive Managed Forests")

      dat_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))

      file_labels <- c("npp", "FML")
      named_files <- setNames(
        dat_names[sapply(
          file_labels,
          function(x) {
            any(grepl(x,
              dat_names,
              ignore.case = TRUE
            ))
          }
        )],
        file_labels
      )
      # load NPP data
      # load data
      npp_in <- elsar_load_data(
        file_name = paste0(named_files[["npp"]], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      raster_mf <- elsar_load_data(
        file_name = paste0(named_files[["FML"]], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      # process data
      managed_forests <- make_managed_forests(
        raster_in = raster_mf,
        pus = pus,
        include_disturbed_forest = TRUE, # includes categories > 11
        make_productive = include_productive,
        raster_npp = npp_in
      )

      names(managed_forests) <- c(dat_non_default[[j]], "Productive Managed Forests") # set layer name
      elsar_plot_feature(
        raster_in = managed_forests[[1]],
        pus = pus,
        legend_title = dat_non_default[[j]],
        figure_path = figure_path
      )
      elsar_plot_feature(
        raster_in = managed_forests[[2]],
        pus = pus,
        legend_title = "Productive Managed Forests",
        figure_path = figure_path
      )
      raster_out <- c(raster_out, managed_forests)
    } else {
      # process data
      managed_forests <- make_managed_forests(
        raster_in = raster_mf,
        pus = pus,
        include_disturbed_forest = TRUE # includes categories > 11
      )

      names(managed_forests) <- c(dat_non_default[[j]]) # set layer name
      elsar_plot_feature(
        raster_in = managed_forests,
        pus = pus,
        legend_title = dat_non_default[[j]],
        figure_path = figure_path
      )
      raster_out <- c(raster_out, managed_forests)
    }
  }

  if (dat_non_default[[j]] == "Key Biodiversity Areas") {
    print("Key Biodiversity Areas")

    # load data
    kba_sf <- elsar_load_data(
      file_name = current_dat$full_name,
      file_type = current_dat$file_type, file_path = current_dat$full_path,
      file_lyr = (if (current_dat$layer != "NA") current_dat$layer else NULL)
    )

    kba_raster <- make_kbas(
      kba_in = kba_sf,
      pus = pus,
      iso3_in = iso3
    )

    names(kba_raster) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = kba_raster,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, kba_raster)
  }

  if (dat_non_default[[j]] == "Wetlands and Ramsar") { # add saving option
    print("Wetlands and Ramsar")

    if (grepl(",", current_dat$file_name)) { # check if both flii and fsii should be used
      dat_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))
      filetype_names <- unlist(strsplit(current_dat$file_type, ", ", fixed = TRUE))

      file_labels <- c("wetland", "ramsar")
      named_files <- setNames(
        dat_names[sapply(
          file_labels,
          function(x) {
            any(grepl(x,
              dat_names,
              ignore.case = TRUE
            ))
          }
        )],
        file_labels
      )

      # load data
      raster_wetlands <- elsar_load_data(
        file_name = paste0(named_files[["wetland"]], ".", filetype_names[[1]]),
        file_type = filetype_names[[1]], file_path = current_dat$full_path
      )

      sf_ramsar <- elsar_load_data(
        file_name = paste0(named_files[["ramsar"]], ".", filetype_names[[2]]),
        file_type = filetype_names[[2]], file_path = current_dat$full_path,
        file_lyr = (if (current_dat$layer != "NA") current_dat$layer else NULL)
      )

      # wetlands and ramsar
      wetlands_ramsar <- make_wetlands_ramsar(
        wetlands_in = raster_wetlands,
        ramsar_in = sf_ramsar,
        pus = pus,
        iso3_in = iso3
      )
    } else if ((!(grepl(",", current_dat$file_name))) & (grepl("wetland", current_dat$file_name))) {
      # load data
      raster_wetlands <- elsar_load_data(
        file_name = paste0(named_files[["wetland"]], ".", filetype_names[[1]]),
        file_type = filetype_names[[1]], file_path = current_dat$full_path
      )

      # wetlands and ramsar
      wetlands_ramsar <- make_wetlands_ramsar(
        wetlands_in = raster_wetlands,
        pus = pus,
        iso3_in = iso3
      )
    } else if ((!(grepl(",", current_dat$file_name))) & (grepl("ramsar", current_dat$file_name))) {
      # load data
      sf_ramsar <- elsar_load_data(
        file_name = paste0(named_files[["ramsar"]], ".", filetype_names[[2]]),
        file_type = filetype_names[[2]], file_path = current_dat$full_path,
        file_lyr = (if (current_dat$layer != "NA") current_dat$layer else NULL)
      )

      # wetlands and ramsar
      wetlands_ramsar <- make_wetlands_ramsar(
        ramsar_in = sf_ramsar,
        pus = pus,
        iso3_in = iso3
      )
    }
    names(wetlands_ramsar) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = wetlands_ramsar,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, wetlands_ramsar)
  }
}

#### Create zones ####
### Which zones to include
zones_list <- data_info %>%
  dplyr::filter(
    group == "zone",
    include == 1
  ) %>%
  dplyr::select("data_name") %>%
  dplyr::pull()

### Which zones data to prep
zones_data <- data_info %>%
  dplyr::filter(
    group == "zone_data",
    include == 1
  )

zones_data_incl <- zones_data %>%
  dplyr::select("data_name") %>%
  dplyr::pull()

#### Prep zones data ####
for (k in 1:length(zones_data_incl)) {
  current_zone_dat <- zones_data %>%
    dplyr::filter(data_name == zones_data_incl[[k]])

  if (zones_data_incl[[k]] == "Managed Forests") {
    print("Managed Forests")

    # load data
    raster_mf <- elsar_load_data(
      file_name = current_zone_dat$full_name,
      file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
    )

    # # process data ### should be done in zone function for restore
    # managed_forests <- make_managed_forests(
    #   raster_in = raster_mf,
    #   pus = pus,
    #   include_disturbed_forest = TRUE #includes categories > 11
    # )
  }

  if (zones_data_incl[[k]] == "Human Footprint") {
    print("Human Footprint")

    # load data
    raster_hfp <- elsar_load_data(
      file_name = current_zone_dat$full_name,
      file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
    )
  }

  if (zones_data_incl[[k]] == "Urban Areas") {
    print("Urban Areas")

    # load data
    raster_urbanareas <- elsar_load_data(
      file_name = current_zone_dat$full_name,
      file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
    )
  }
}

#### Prep zones ####
for (l in 1:length(zones_list)) {
  if (zones_list[[l]] == "Protection Zone") {
    print("Protection Zone")

    protection_zone <- make_protection_zone(
      hfp_in = raster_hfp,
      # crop_in = load_crop,
      # built_in = load_built,
      hfp_threshold = 22,
      pus = pus,
      iso3 = iso3
    )
  }

  # save and vsiualise zone
}

## Create locked-in areas
lockedIn_list <- c("avail")

#### Save raster stack ####
out_name <- file.path(glue::glue("{output_path}/data_stack_{iso3}.tif"))

writeRaster(raster_out, out_name,
  filetype = "COG",
  datatype = "FLT4S", # 32-bit float
  gdal = c("COMPRESS=DEFLATE"),
  overwrite = TRUE
)

# r_test <- terra::rast(out_name)

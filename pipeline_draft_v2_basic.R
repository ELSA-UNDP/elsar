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

############################################################################### #
# Set run-time parameters ####
# Set local temp directory for `terra`
terra::terraOptions(tempdir = here::here("~/Documents/"), steps = 4, todisk = TRUE)
terraOptions()
terra::tmpFiles(remove = TRUE)

## Set paths for data ####
# sheet_path <- "C:/Users/sandr/Documents/Github/elsar"
# input_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/input_dat"
# output_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/output_dat"
# figure_path <- "C:/Users/sandr/Documents/UNBL_work/Pipeline/output_dat/figures"
sheet_path <- "/home/scottca/gitrepos/elsar"
input_path <- "/home/scottca/Documents/elsar_data"
output_path <- "/home/scottca/Documents/elsar_outputs"
figure_path <- "/home/scottca/Documents/elsar_figures"



## Country info ####
iso3 <- "AND"
iso3_column <- "iso3cd" # for boundary data

############################################################################### #
# Load data file ####
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

# Create data ####
## PUs
if (as.logical(as.integer(data_info %>%
  dplyr::filter(group == "pus") %>%
  dplyr::pull(include)))) {

  log_msg("Creating Planning Units")

  ## Load data ####
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

  # Create boundary ####
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

  # Create PUs ####
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

# check if lulc data is ready for this country
download_lulc <- data_info %>%
  dplyr::filter(data_name %in% c(
    "Urban Greening Opportunities",
    "Restoration Zone",
    "Protection Zone",
    "Agriculture Areas",
    "Urban Areas"
  ))

if (nrow(download_lulc) > 0) {
  # search input data folder for lulc data
  all_dat <- list.files(input_path)
  lulc_there <- all_dat[grepl("lulc", all_dat) & grepl(iso3, all_dat, ignore.case = TRUE)]

  if (rlang::is_empty(lulc_there)) {
    log_msg(glue::glue("No LULC data available in input_path for {iso3}."))
    # {
    answer <- readline("Do you need to download country-specific LULC data? This is only needed once. Type to override or press Enter to confirm [default = yes] (yes/no): ")
    if (tolower(trimws(answer)) == "") answer <- "yes" # to streamline if someone says "Yes" or similar

    if (answer == "yes") { # go through pre-saved options that we need to list down here. Will add to this
      message("You need access to the unbl_misc gee repository. Please do this before trying to download the data. ")
      message("Make sure you have a working internet connection.")
      # {
      answer2 <- readline("Do you have access to the unbl-misc gee repository and a working internet connection? Type to override or press Enter to confirm [default = yes] (yes/no): ")
      if (tolower(trimws(answer2)) == "") answer2 <- "yes" # to streamline if someone says "Yes" or similar
      if (answer2 == "yes") {
        lulc <- elsar_download_esri_lulc_data(
          boundary = boundary_proj,
          iso3 = iso3,
          output_dir = input_path
        )
      } else {
        message("You can't download the data without access to the gee repository.")
      }

      # }
    } else {
      message("You either need to change the path of the data to where to LULC data is saved or change what data you want to process. LULC data is needed for some of the data selected in the spreadsheet.")
    }
    # }
  }
}

# clean env
rm(boundary_info, boundary_dat)
gc()

##### Create feature stack #####
# could have one master script and then extra feature generation scripts etc that are sourced + ADD visualisation option
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

if (("Alliance for Zero Extinction Sites") %in% dat_non_default & ("Key Biodiversity Areas") %in% dat_non_default) { # managed forests and productive managed forests handled within same function
  include_kbas <- TRUE
  include_aze <- TRUE
  dat_non_default <- dat_non_default[dat_non_default %ni% c("Alliance for Zero Extinction Sites")]
} else if (("Alliance for Zero Extinction Sites") %in% dat_non_default & !(("Key Biodiversity Areas") %in% dat_non_default)) {
  include_kbas <- FALSE
  include_aze <- TRUE
} else if ((!("Alliance for Zero Extinction Sites") %in% dat_non_default) & (("Key Biodiversity Areas") %in% dat_non_default)) {
  include_kbas <- TRUE
  include_aze <- FALSE
} else {
  include_kbas <- FALSE
  include_aze <- FALSE
}


#### Prep data using default methods ####
log_msg("Creating Default Features...")

# init data stack with pus as first layer
raster_out <- pus
for (i in 1:length(dat_default)) { # for all the data that runs with make_normalised_raster()
  gc()

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
      message("Your data might not have a pre-saved option yet. Please enter your processing options manually.")
      next # jump to else if to enter options manually. Not tested yet!!!
    }
  } else { # everything default
    rast_norm <- make_normalised_raster(
      raster_in = current_rast,
      pus = pus,
      iso3 = iso3
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

# Prep data using non-default methods ####
log_msg("Creating Non-Default Features...")

for (j in 1:length(dat_non_default)) { # for all the data that runs with non-default functions

  current_dat <- feature_list %>%
    dplyr::filter(data_name == dat_non_default[[j]])

## Mangroves ####
  if (dat_non_default[[j]] == "Mangroves") {
    print("Mangroves")

    current_rast <- elsar_load_data(
      file_name = current_dat$full_name,
      file_type = current_dat$file_type, file_path = current_dat$full_path,
      wkt_filter = TRUE,
      bb_extend = pus
    )

    if (nrow(current_rast) == 0) {
      message("No mangroves in the planning region.")
    }
    mangrove_raster <- make_mangroves(
      sf_in = current_rast,
      pus = pus,
      iso3 = iso3,
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

  # Forest Integrity Index ####
  if (dat_non_default[[j]] == "Forest Integrity Index") {
    print("Forest Integrity Index")

    if (grepl(",", current_dat$file_name)) { # check if both flii and fsii should be used
      fii_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))

      # load data
      raster_flii <- elsar_load_data(
        file_name = paste0(fii_names[grepl("flii", fii_names)], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      raster_fsii <- elsar_load_data(
        file_name = paste0(fii_names[grepl("fsii", fii_names)], ".", current_dat$file_type),
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


# Protected Area ####
  if (dat_non_default[[j]] == "Existing PAs") {
    print("Existing PAs")

    if (current_dat$default_parameters != 1) {
      message("You are using non-default Protected Area parameters")
      ## Non-Default Protected Area Parameters ####
      answer <- readline("Do you want to download current protected areas from Protected Planet using the wdpar package? Type to override or press Enter [default = yes]: ")
      if (tolower(trimws(answer)) == "") answer <- "yes"

      status <- readline("Included WDPA statuses [default = c('Designated', 'Inscribed', 'Established')]. Type to override or press Enter: ")
      if (trimws(status) == "") status <- "c('Designated', 'Inscribed', 'Established')"

      designation_mab <- readline("Include UNESCO MAB areas (TRUE/FALSE) [default = FALSE]? Type to override or press Enter: ")
      if (trimws(designation_mab) == "") designation_mab <- "FALSE"

      buffer_points <- readline("Buffer POINT geometries (TRUE/FALSE) [default = TRUE]? Type to override or press Enter: ")
      if (trimws(buffer_points) == "") buffer_points <- "TRUE"

      area_column <- readline("Attribute for points with reported area (e.g. REP_AREA) [default = REP_AREA]: ")
      if (trimws(area_column) == "") area_column <- "REP_AREA"

      nQuadSegs <- readline("Number of segments for geodesic buffering (e.g. 50) [default = 50]: ")
      if (trimws(nQuadSegs) == "") nQuadSegs <- "50"

      if (answer == "yes") {
        current_pas <- make_protected_areas(
          from_wdpa = TRUE,
          iso3 = iso3,
          input_path = input_path,
          status = eval(parse(text = status)),
          pa_def = 1,
          include_mab_designation = as.logical(designation_mab),
          buffer_points = as.logical(buffer_points),
          area_column = area_column,
          nQuadSegs = as.integer(nQuadSegs),
          pus = pus,
          output_path = output_path
        )
      } else {
        if (is.na(current_dat$full_name) | is.null(current_dat$full_name)) {
          stop("No protected areas provided and download declined. Please update `input_data.csv` or allow download from Protected Planet.")
        } else {
          sf_in <- elsar_load_data(
            file_name = current_dat$full_name,
            file_type = current_dat$file_type,
            file_path = current_dat$full_path
          )

          if (nrow(sf_in) == 0) {
            stop("Provided protected area file contains no features. Please update the file or allow download from Protected Planet.")
          }

          current_pas <- make_protected_areas(
            from_wdpa = FALSE,
            iso3 = iso3,
            sf_in = sf_in,
            input_path = NULL,
            status = eval(parse(text = status)),
            pa_def = 1,
            include_mab_designation = as.logical(designation_mab),
            buffer_points = as.logical(buffer_points),
            area_column = area_column,
            nQuadSegs = as.integer(nQuadSegs),
            pus = pus,
            output_path = output_path
          )
        }
      }
    } else {
      ## Default Protected Area Parameters ####
      if (is.na(current_dat$full_name) | is.null(current_dat$full_name)) {
        message("No existing Protected Area file provided. Downloading current WDPA data from Protected Planet and using default parameters...")

        current_pas <- make_protected_areas(
          from_wdpa = TRUE,
          iso3 = iso3,
          sf_in = NULL,
          input_path = input_path,
          buffer_points = TRUE,
          pus = pus,
          output_path = output_path
        )
      } else {
        sf_in <- elsar_load_data(
          file_name = current_dat$full_name,
          file_type = current_dat$file_type,
          file_path = current_dat$full_path
        )

        current_pas <- make_protected_areas(
          from_wdpa = FALSE,
          iso3 = iso3,
          sf_in = sf_in,
          input_path = NULL,
          buffer_points = TRUE,
          pus = pus,
          output_path = output_path
        )
      }
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

  # Managed Forests ####
  if (dat_non_default[[j]] == "Managed Forests") {
    print("Managed Forests")

    if (include_productive) {
      current_dat <- feature_list %>%
        dplyr::filter(data_name == "Productive Managed Forests")

      dat_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))

      # load NPP data
      # load data
      npp_in <- elsar_load_data(
        file_name = paste0(dat_names[grepl("npp", dat_names)], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      raster_mf <- elsar_load_data(
        file_name = paste0(dat_names[grepl("FML", dat_names)], ".", current_dat$file_type),
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

      # process data
      managed_forests <- make_managed_forests(
        raster_in = raster_mf,
        pus = pus,
        include_disturbed_forest = TRUE, # includes categories 20
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
      # load data
      raster_mf <- elsar_load_data(
        file_name = current_dat$full_name,
        file_type = current_dat$file_type, file_path = current_dat$full_path
      )

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

  # Key Biodiversity Areas | Alliance for Zero Extinction Sites ####
  if (dat_non_default[[j]] == "Key Biodiversity Areas" | dat_non_default[[j]] == "Alliance for Zero Extinction Sites") {
    print("Key Biodiversity Areas | Alliance for Zero Extinction Sites")

    # load data
    kba_sf <- elsar_load_data(
      file_name = current_dat$full_name,
      file_type = current_dat$file_type, file_path = current_dat$full_path,
      file_lyr = (if (current_dat$layer != "NA") current_dat$layer else NULL)
    )

    if (include_kbas & include_aze) {
      kba_out <- make_kbas(
        kba_in = kba_sf,
        pus = pus,
        iso3 = iso3
      )

      aze_raster <- make_kbas(
        kba_in = kba_sf,
        pus = pus,
        iso3 = iso3,
        aze_only = TRUE
      )

      kba_raster <- c(kba_out, aze_raster)

      names(kba_raster) <- c("Key Biodiversity Areas", "Alliance for Zero Extinction Sites") # set layer name

      elsar_plot_feature(
        raster_in = kba_raster[[1]],
        pus = pus,
        legend_title = "Key Biodiversity Areas",
        figure_path = figure_path
      )

      elsar_plot_feature(
        raster_in = kba_raster[[2]],
        pus = pus,
        legend_title = "Alliance for Zero Extinction Sites",
        figure_path = figure_path
      )
    } else if (!include_kbas & include_aze) {
      kba_raster <- make_kbas(
        kba_in = kba_sf,
        pus = pus,
        iso3 = iso3,
        aze_only = TRUE
      )

      names(kba_raster) <- c("Alliance for Zero Extinction Sites") # set layer name
      elsar_plot_feature(
        raster_in = kba_raster,
        pus = pus,
        legend_title = dat_non_default[[j]],
        figure_path = figure_path
      )
      raster_out <- c(raster_out, kba_raster)
    } else if (include_kbas & !include_aze) {
      log_msg("KBAs include Alliance for Zero Extinction Sites.")
      kba_raster <- make_kbas(
        kba_in = kba_sf,
        pus = pus,
        iso3 = iso3
      )

      names(kba_raster) <- c("Key Biodiversity Areas") # set layer name
      elsar_plot_feature(
        raster_in = kba_raster,
        pus = pus,
        legend_title = "Key Biodiversity Areas",
        figure_path = figure_path
      )
      raster_out <- c(raster_out, kba_raster)
    }
  }

  # Wetlands and Ramsar ####
  if (dat_non_default[[j]] == "Wetlands and Ramsar") { # add saving option
    print("Wetlands and Ramsar")

    if (grepl(",", current_dat$file_name)) { # check if both flii and fsii should be used
      dat_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))
      filetype_names <- unlist(strsplit(current_dat$file_type, ", ", fixed = TRUE))

      # load data
      raster_wetlands <- elsar_load_data(
        file_name = paste0(dat_names[grepl("wetland", dat_names)], ".", filetype_names[[1]]),
        file_type = filetype_names[[1]], file_path = current_dat$full_path
      )

      sf_ramsar <- elsar_load_data(
        file_name = paste0(dat_names[grepl("ramsar", dat_names)], ".", filetype_names[[2]]),
        file_type = filetype_names[[2]], file_path = current_dat$full_path,
        file_lyr = (if (current_dat$layer != "NA") current_dat$layer else NULL)
      )

      # wetlands and ramsar
      wetlands_ramsar <- make_wetlands_ramsar(
        wetlands_in = raster_wetlands,
        ramsar_in = sf_ramsar,
        pus = pus,
        iso3 = iso3,
        buffer_points = FALSE
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
        iso3_in = iso3,
        buffer_points = FALSE
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
        iso3_in = iso3,
        buffer_points = FALSE
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
  # Urban Greening Opportunities ####
  if (dat_non_default[[j]] == "Urban Greening Opportunities") {
    print("Urban Greening Opportunities")

    urb_green_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))
    filetype_names <- unlist(strsplit(current_dat$file_type, ", ", fixed = TRUE))

    # load data
    raster_ndvi <- elsar_load_data(
      file_name = paste0(urb_green_names[grepl("ndvi", urb_green_names)], ".", filetype_names[[1]]),
      file_type = filetype_names[[1]], file_path = current_dat$full_path
    )

    lulc_name <- urb_green_names[grepl("lulc", urb_green_names)]

    if (!grepl(iso3, lulc_name)) {
      lulc_name <- paste0(lulc_name, "_", iso3)
    }

    raster_lulc <- elsar_load_data(
      file_name = paste0(lulc_name, ".", filetype_names[[1]]),
      file_type = filetype_names[[1]], file_path = current_dat$full_path
    )

    sf_wbgtmax <- elsar_load_data(
      file_name = paste0(urb_green_names[grepl("wbgtmax", urb_green_names)], ".", filetype_names[[2]]),
      file_type = filetype_names[[2]], file_path = current_dat$full_path
    )

    # urban greening opportunities
    urban_out <- make_urban_greening_opportunities(
      ndvi_raster = raster_ndvi,
      lulc_raster = raster_lulc,
      sdei_statistics = sf_wbgtmax,
      pus = pus,
      iso3 = iso3,
      return_urban_areas = TRUE
    )

    urban_green_opps <- urban_out[[1]]
    urban_areas <- urban_out[[2]]

    names(urban_green_opps) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = urban_green_opps,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )

    names(urban_areas) <- c("Urban Areas") # set layer name
    elsar_plot_feature(
      raster_in = urban_areas,
      pus = pus,
      legend_title = "Urban Areas",
      figure_path = figure_path
    )

    raster_out <- c(raster_out, urban_out)
  }
  # Flood Abatement Opportunities ####
  if (dat_non_default[[j]] == "Flood Abatement Opportunities") {
    print("Flood Abatement Opportunities")

    # load data
    flood_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))
    filetype_names <- unlist(strsplit(current_dat$file_type, ", ", fixed = TRUE))

    # flood abatement
    raster_ndvi <- elsar_load_data(
      file_name = paste0(flood_names[grepl("ndvi", flood_names)], ".", filetype_names),
      file_type = filetype_names, file_path = current_dat$full_path
    )

    raster_flood <- elsar_load_data(
      file_name = paste0(flood_names[grepl("flood", flood_names)], ".", filetype_names),
      file_type = filetype_names, file_path = current_dat$full_path
    )

    flood_abate <- make_flood_abatement_opportunities(
      gfd_raster = raster_flood,
      ndvi_raster = raster_ndvi,
      pus = pus,
      iso3 = iso3
    )

    names(flood_abate) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = flood_abate,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, flood_abate)
  }
  # Indigenous Managed Lands ####
  if (dat_non_default[[j]] == "Indigenous Managed Lands") {
    print("Indigenous Managed Lands")

    # load data


    # process data


    names(indigenous_lands) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = indigenous_lands,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, indigenous_lands)
  }

  if (dat_non_default[[j]] == "Underepresented Ecosystems") {
    print("Underepresented Ecosystems")

    # process data
    underrep <- make_underrepresented_ecosystems(
      iucn_get_directory = file.path(current_dat$full_path, current_dat$file_name),
      current_protected_areas = current_protected_area_sf,
      iso3 = iso3,
      pus = pus,
      boundary_layer = boundary_proj
    )

    names(underrep) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = underrep,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, underrep)
  }

  if (dat_non_default[[j]] == "Threatened Ecosystems for Protection") {
    print("Threatened Ecosystems for Protection")

    #load data
    threat_names <- unlist(strsplit(current_dat$file_name, ", ", fixed = TRUE))
    filetype_names <- unlist(strsplit(current_dat$file_type, ", ", fixed = TRUE))

    raster_intact <- elsar_load_data(
      file_name = paste0(threat_names[grepl("intact|eii", threat_names)], ".", filetype_names[[2]]),
      file_type = filetype_names[[2]], file_path = current_dat$full_path
    )

    # process data
    threat_p <- make_threatened_ecosystems_protection(
        iso3 = iso3,
        pus = pus,
        boundary_layer = boundary_proj,
        intactness_input = raster_intact,
        iucn_get_directory = file.path(current_dat$full_path, threat_names[grepl("iucn", threat_names)])
      )

    names(threat_p) <- c(dat_non_default[[j]]) # set layer name
    elsar_plot_feature(
      raster_in = threat_p,
      pus = pus,
      legend_title = dat_non_default[[j]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, threat_p)
  }
}

#### Create zones ####
log_msg("Creating zones...")
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

# Prep zones data ####
for (k in 1:length(zones_data_incl)) {

  gc()

  current_zone_dat <- zones_data %>%
    dplyr::filter(data_name == zones_data_incl[[k]])

## Managed Forests ####
  if (zones_data_incl[[k]] == "Managed Forests") {
    print("Managed Forests")

    # load data
    if ("Managed Forests" %in% dat_non_default) {
      raster_mf <- managed_forests[[1]]
    } else {
      log_msg("Please include Managed Forests in the generated features in input spreadsheet.")
    }
  }

## Urban Areas ####
  if (zones_data_incl[[k]] == "Urban Areas") {
    print("Urban Areas")

    if (terra::nlyr(urban_out) == 2) {
      raster_urban <- urban_out[[2]]
    } else {
      raster_urbanareas <- elsar_load_data(
        file_name = paste0(current_zone_dat$file_name, "_", iso3, ".", current_zone_dat$file_type),
        file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
      )
    }
  }

## Agriculture Areas ####
  if (zones_data_incl[[k]] == "Agriculture Areas") {
    print("Agriculture Areas")

    # load data
    raster_agri_in <- elsar_load_data(
      file_name = paste0(current_zone_dat$file_name, "_", iso3, ".", current_zone_dat$file_type),
      file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
    )

    # process here because computationally expensive and we only want to do this once:
    raster_agri <- make_normalised_raster(
      raster_in = raster_agri_in,
      pus = pus,
      iso3 = iso3,
      method_override = "mean",
      input_raster_conditional_expression = function(x) terra::ifel(x == 4, 1, 0)
    )
  }

## Degraded Areas ####
  if (zones_data_incl[[k]] == "Degraded Areas") {
    print("Degraded Areas")

    # load data
    raster_degraded <- elsar_load_data(
      file_name = current_zone_dat$full_name,
      file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
    )
  }

## Human Industrial Footprint ####
  if (zones_data_incl[[k]] == "Human Industrial Footprint") {
    print("Human Industrial Footprint")

    # load data
    raster_hii <- elsar_load_data(
      file_name = current_zone_dat$full_name,
      file_type = current_zone_dat$file_type, file_path = current_zone_dat$full_path
    )
  }

## IUCN Forests ####
  if (zones_data_incl[[k]] == "IUCN Forests") {
    print("IUCN Forests")

    # process data
    raster_iucnForest <- get_iucn_forests(
      iucn_get_directory = file.path(current_zone_dat$full_path, current_zone_dat$file_name),
      pus = pus,
      iso3 = iso3,
      boundary_layer = boundary_proj,
      include_minor_occurrence = TRUE
    )

  }
}

# Prep zones ####
## Protection Zone ####
for (l in 1:length(zones_list)) {
  if (zones_list[[l]] == "Protection Zone") {
    print("Protection Zone")

    protection_zone <- make_protect_zone(
      hii_input = raster_hii,
      current_protected_areas = current_protected_area_sf,
      agricultural_areas_input = raster_agri,
      built_areas_input = raster_urban,
      pus = pus,
      iso3 = iso3
    )

    elsar_plot_feature(
      raster_in = protection_zone,
      pus = pus,
      legend_title = zones_list[[l]],
      figure_path = figure_path
    )
    raster_out <- c(raster_out, protection_zone)
  }

## Restoration Zone ####
  if (zones_list[[l]] == "Restoration Zone") {
    print("Restoration Zone")

    restoration_zone <- make_restore_zone(
      iso3 = iso3,
      pus = pus,
      sdg_degradation_input = raster_degraded,
      agricultural_areas_input = raster_agri,
      built_areas_input = raster_urban,
      iucn_get_forest_input = raster_iucnForest,
      hii_input = raster_hii
    )

    elsar_plot_feature(
      raster_in = restoration_zone[[1]],
      pus = pus,
      legend_title = "restore_zone_v1",
      figure_path = figure_path
    )
    elsar_plot_feature(
      raster_in = restoration_zone[[2]],
      pus = pus,
      legend_title = "restore_zone_v2",
      figure_path = figure_path
    )
    raster_out <- c(raster_out, restoration_zone)
  }

## Management Zone ####
  if (zones_list[[l]] == "Management Zone") {
    print("Management Zone")

    management_zone <- make_manage_zone(
      hii_input = raster_hii,
      agricultural_areas_input = raster_agri,
      built_areas_input = raster_urban,
      managed_forests_input = raster_mf,
      pus = pus,
      iso3 = iso3
    )

    elsar_plot_feature(
      raster_in = management_zone[[1]],
      pus = pus,
      legend_title = "manage_zone_v1",
      figure_path = figure_path
    )
    elsar_plot_feature(
      raster_in = management_zone[[2]],
      pus = pus,
      legend_title = "manage_zone_v2",
      figure_path = figure_path
    )
    raster_out <- c(raster_out, management_zone)
  }
}

#### add threatened
if ("Threatened Ecosystems for Restoration" %in% dat_non_default) {
  log_msg("Threatened Ecosystems for Restoration (created last because it makes use of the Restore Zone (V1))")

  gc()

  current_dat <- feature_list %>%
    dplyr::filter(data_name == "Threatened Ecosystems for Restoration")

  # process data
  threat_r <- make_threatened_ecosystems_restoration(
    iso3 = iso3,
    pus = pus,
    threatened_ecosystems_input = threat_p,
    degradation_input = restoration_zone[[1]]
  )

  names(threat_r) <- c("Threatened Ecosystems for Restoration") # set layer name
  elsar_plot_feature(
    raster_in = threat_r,
    pus = pus,
    legend_title = "Threatened Ecosystems for Restoration",
    figure_path = figure_path
  )
  raster_out <- c(raster_out, threat_r)
}

# Save raster stack ####
out_name <- file.path(glue::glue("{output_path}/data_stack_{iso3}.tif"))

log_msg(glue::glue("Datastack saved as a multiband geotiff to: {out_name}"))

terra::writeRaster(
  raster_out,
  filename = out_name,
  filetype = "COG",
  datatype = "FLT4S", # 32-bit float
  gdal = c(
    "COMPRESS=ZSTD",
    "PREDICTOR=3",
    "OVERVIEWS=NONE",
    "NUM_THREADS=ALL_CPUS"
    ),
  overwrite = TRUE
  )

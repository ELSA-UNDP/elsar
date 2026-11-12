#' Create a Normalised Raster Aligned with and masked by Planning Units
#'
#' This function takes an input raster, aligns it with specified planning units (PUs),
#' optionally inverts, rescales, applies conditional expressions, and saves the processed
#' raster to a specified output path.
#'
#' @param raster_in `SpatRaster` The input raster to be processed.
#' @param pus `SpatVector` The planning units (PUs) to align the raster to.
#' @param iso3 `character` ISO3 country code used for naming the output file.
#' @param invert `logical` If `TRUE`, inverts the raster values (default: `FALSE`).
#' @param rescaled `logical` If `TRUE`, rescales the raster using `rescale_raster()` (default: `TRUE`).
#' @param method_override `character` How input cells are combined into each
#'        planning unit (default `NULL`, i.e. `"mean"`): one of the
#'        [exactextractr::exact_resample()] summaries, e.g. `"mean"`, `"sum"`,
#'        `"max"`, `"min"` or `"mode"`. All are area-weighted (see Details).
#' @param crop_global_input  `logical` If true the input (large global) raster is cropped
#'        to the PU extent before applying an `input_raster_conditional_expression`, to reduce
#'        the are of processing (default: `TRUE`).
#' @param input_raster_conditional_expression `function` Optional method to apply a
#'        function to the raster before resampling to the PU layer (default: `NULL`).
#' @param conditional_expression `function` Optional method to apply a function to the
#'        raster after resampling to the PU layer (default: `NULL`).
#' @param fill_na `numeric` or `NULL`. The value given to planning units with
#'        no data (default: 0). It is applied *after* `invert` and `rescaled`,
#'        so with the defaults missing data gets the lowest score (0). `NULL`
#'        leaves those cells `NA`. Cells outside the planning units are always
#'        NoData.
#' @param name_out `character` The name of the output raster file (without the extension).
#' @param output_path `character` The directory path to save the output raster (default: `NULL`, i.e., not saved).
#' @param threads Optional method to use multi-core processing - to speed on some `terra`
#'        functions (default: `TRUE`).
#'
#' @details The input is aggregated to the planning-unit grid with `method_override`
#' (`"mean"` by default), weighting every input cell by the share of the
#' planning unit it covers:
#' * in the same CRS, with [exactextractr::exact_resample()];
#' * in another CRS, by warping onto a sub-grid of up to 10 x 10 cells per
#'   planning unit with the matching GDAL method (average, max, min, mode;
#'   every overlapping input cell counts) and aggregating back. Very fine
#'   inputs are first aggregated in their own CRS. Results are area-weighted to
#'   within 1/100 of a planning unit, memory is bounded by the planning-unit
#'   grid, and inputs crossing the antimeridian are handled. `"sum"` keeps
#'   totals.
#'
#' Steps, in order: `input_raster_conditional_expression` (on the input, before
#' aggregation; it can be slow for high-resolution inputs), aggregation,
#' `conditional_expression`, `invert`, rescaling to 0-1, `fill_na`, masking to
#' the planning units. A layer with no data at all returns 0 inside the
#' planning units.
#'
#' @return Returns a `SpatRaster` object that has been reprojected and processed.
#' If `output_path` is specified, saves the raster as a COG (Cloud Optimized GeoTIFF).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' raster_out <- make_normalised_raster(
#'   raster_in = my_raster,
#'   pus = my_pus,
#'   iso3 = "USA",
#'   invert = TRUE,
#'   rescaled = FALSE,
#'   name_out = "aligned_raster",
#'   output_path = "/path/to/output"
#' )
#'
#' # Applies a conditional expression to the layer that hase been projected to the
#'   planning unit layer.
#' inverted_ndvi <- make_normalised_raster(
#'   raster_in = ndvi_raster,
#'   pus = pus,
#'   iso3 = iso3,
#'   invert = TRUE,
#'   conditional_expression = function(x) terra::ifel(x < 0, NA, 1 - x)
#'   )
#'
#' # Convert a landcover classifgiation layer into a binary - applied a conditional
#'   function to the input layer, not the layer that has been projected to the planning
#'   units.
#' raster_out <- make_normalised_raster(
#'   raster_in = land_cover_raster,
#'   pus = my_pus,
#'   iso3 = "USA",
#'   crop_global_input = FALSE # ESRI LULC rasters are already export from at the PU extent
#'   input_raster_conditional_expression = function(r) ifel(r %in% c(1:4, 7, 9), 1, 0)
#'   )
#'
#' # For high resolution rasters (10m Sentinel based LULC for example) that cover a large
#'   area, it may be more efficient and much faster to pre-create binary class rasters
#'   (e.g., built areas, agriculture areas) using tools like gdal_calc or GEE.
#'
#' built_areas_raster <- terra::rast("built_areas_brazil.tif")
#'
#' urban_areas <- make_normalised_raster(
#'   raster_in = built_areas_raster,
#'   pus = my_pus,
#'   iso3 = "BRA"
#'   )
#' }
#'
make_normalised_raster <- function(raster_in,
                                   pus,
                                   iso3,
                                   invert = FALSE,
                                   rescaled = TRUE,
                                   method_override = NULL,
                                   crop_global_input = TRUE,
                                   input_raster_conditional_expression = NULL,
                                   conditional_expression = NULL,
                                   fill_na = 0,
                                   name_out,
                                   output_path = NULL,
                                   threads = TRUE) {
  # Valid methods for exactextractr::exact_resample
  valid_resampling_methods <- c(
    "mean",     # Area-weighted mean
    "sum",      # Area-weighted sum
    "min",      # Minimum value
    "max",      # Maximum value
    "mode",     # Most frequent value
    "majority", # Same as mode (alias)
    "minority", # Least frequent value
    "variety",  # Number of unique values
    "count",    # Count of raster cells
    "stdev",    # Standard deviation
    "coefficient_of_variation" # stdev / mean
  )

  if (!is.null(method_override)) {
    if (!(method_override %in% valid_resampling_methods)) {
      stop("Invalid method_override. Choose one of: ", paste(valid_resampling_methods, collapse = ", "))
    }
    method <- method_override
  } else {
    method <- "mean"
  }

  if (!is.null(output_path)) {
    assertthat::assert_that(dir.exists(output_path),
                            msg = glue::glue("'output_path' directory does not exist: {output_path}"))
  }

  # Check if raster was pre-aggregated in GEE (has pre_aggregated attribute)
  is_pre_aggregated <- isTRUE(attr(raster_in, "pre_aggregated"))

  if (is_pre_aggregated) {
    # Skip expensive resampling - data is already at PU resolution
    log_message("Input raster was pre-aggregated in GEE. Skipping local resampling.")

    # Just align and mask to PUs
    dat_aligned <- terra::project(raster_in, y = pus, threads = threads)

    # Apply input conditional expression if provided (for class extraction)
    if (!is.null(input_raster_conditional_expression)) {
      # Warn if applying class extraction to mode-aggregated categorical data
      aggregation_reducer <- attr(raster_in, "aggregation_reducer")
      if (!is.null(aggregation_reducer) && aggregation_reducer == "mode") {
        warning(
          "Applying conditional expression to pre-aggregated categorical data (mode reducer). ",
          "This will produce binary (0/1) values per PU, not class proportions. ",
          "For proper class proportions, use download_lulc_class_proportion() or pass ",
          "pre-computed class proportion rasters via agricultural_areas_input/built_areas_input parameters.",
          call. = FALSE
        )
      }
      log_message("Applying conditional expression to pre-aggregated raster...")
      dat_aligned <- input_raster_conditional_expression(dat_aligned)
    }

  } else {
    # Standard processing path with full resampling

    # Crop before reprojection if a conditional expression is to be applied and the raw input is a large global raster
    if (!is.null(input_raster_conditional_expression)) {
      if (crop_global_input) {
        log_message("Cropping input raster before applying conditional expression...")
        cropped <- crop_global_raster(raster_in, pus)
        log_message("Applying conditional expression to cropped input raster...")
        raster_in <- input_raster_conditional_expression(cropped)
      } else {
        log_message("Applying conditional expression to input raster...")
        raster_in <- input_raster_conditional_expression(raster_in)
      }
    }

    # Align to the PU grid, aggregating with `method` (area-weighted)
    dat_aligned <- .align_to_pus(raster_in, pus, method = method, threads = threads)
  }

  # Guard against unflagged NoData sentinels. An input with no proper NoData set
  # (e.g. an integer raster with NAflag unset) has its reprojected/edge cells
  # filled with the datatype extreme (e.g. -2147483648 for INT32). Left in, that
  # value dominates the min-max rescale and flattens every real value to ~1.
  # These sentinels are not plausible data, so mask them to NA and warn.
  # Covers the INT32 extreme used as a reprojection fill (-2147483648), the
  # common GDAL Int32 NoData (-2147483647), the positive extreme, and absurd
  # float sentinels (e.g. -3.4e38). None are plausible data values.
  n_before <- suppressWarnings(terra::global(dat_aligned, "notNA")[[1]])
  dat_aligned <- terra::ifel(
    dat_aligned == -2147483648 | dat_aligned == -2147483647 |
      dat_aligned == 2147483647 | abs(dat_aligned) > 1e30,
    NA, dat_aligned
  )
  n_after <- suppressWarnings(terra::global(dat_aligned, "notNA")[[1]])
  if (isTRUE(n_after < n_before)) {
    warning("make_normalised_raster: masked ", n_before - n_after,
            " cell(s) holding a NoData sentinel (e.g. -2147483648). Check the ",
            "input's NoData/NAflag - such values otherwise flatten the rescale.",
            call. = FALSE)
  }
  rng <- suppressWarnings(terra::minmax(dat_aligned))
  if (all(is.finite(rng)) && (rng[2] - rng[1]) > 1e9) {
    warning("make_normalised_raster: input value range is very large (",
            signif(rng[1], 4), " to ", signif(rng[2], 4), "); a stray sentinel ",
            "or bad NoData can flatten the 0-1 rescale - verify the input.",
            call. = FALSE)
  }

  if (!is.null(conditional_expression)) {
    dat_aligned <- conditional_expression(dat_aligned)
  }

  # Empty means no data at all. Checked after the conditional expression, so
  # layers whose values are all <= 0 (e.g. differences) are not mistaken for
  # empty ones.
  if (isTRUE(suppressWarnings(terra::global(dat_aligned, "notNA")[[1]]) == 0)) {
    log_message("No valid raster values found: returning empty raster.")
    dat_aligned <- terra::ifel(pus == 1, 0, NA)
    rescaled <- FALSE
    invert <- FALSE
  }

  # Invert and rescale the data first, then fill gaps, so missing data always
  # gets `fill_na` in the final units (0 = the lowest score after rescaling)
  # instead of becoming the maximum after inversion.
  if (invert) {
    dat_aligned <- -dat_aligned
  }

  if (rescaled) {
    dat_aligned <- rescale_raster(dat_aligned)
  } else {
    # A deliberate, common choice for presence/coverage layers - inform, don't
    # warn (warning() here fires dozens of times per pipeline run).
    log_message("Raster values are NOT rescaled (rescaled = FALSE).")
  }

  if (!is.null(fill_na)) {
    dat_aligned <- terra::ifel(is.na(dat_aligned), fill_na, dat_aligned)
  }
  dat_aligned <- terra::mask(dat_aligned, pus, maskvalues = NA)
  names(dat_aligned) <- if (!missing(name_out)) name_out else names(raster_in)[1]

  if (!is.null(output_path)) {
    data_type <- if (terra::is.int(dat_aligned)) "INT1S" else "FLT4S"
    # INT1S is signed (-128..127); 255 is out of range. Use -128 as the NoData
    # sentinel for integer output (the `invert` path can produce negatives).
    na_value <- if (terra::is.int(dat_aligned)) -128 else -9999

    gdal_options <- c(
      "COMPRESS=ZSTD",
      "NUM_THREADS=ALL_CPUS",
      "OVERVIEWS=NONE",
      if (data_type == "INT1S") "PREDICTOR=2" else if (data_type == "FLT4S") "PREDICTOR=3"
    )

    gdal_options <- gdal_options[!is.na(gdal_options)]

    .muffle_cog_warnings(terra::writeRaster(
      dat_aligned,
      glue::glue("{output_path}/{name_out}_{iso3}.tif"),
      gdal = gdal_options,
      datatype = data_type,
      NAflag = na_value,
      overwrite = TRUE,
      filetype = "COG"
    ))
  }

  return(dat_aligned)
}

# Align a raster to the planning-unit grid, aggregating with `method`.
#
# Same CRS: exactextractr::exact_resample() weights every input cell by the
# share of the planning unit it covers, for inputs finer or coarser than the
# PUs.
#
# Different CRS: the input is cropped to the PU footprint, pre-aggregated in
# its own CRS if it is much finer than needed, warped onto a sub-grid of n x n
# cells per PU (aligned with the PUs, so the warp is bounded by the PU extent,
# also across the antimeridian) with the GDAL method matching `method`
# (average, max, min, mode: every source cell overlapping a sub-cell counts,
# so small features are not skipped), and aggregated back to the PUs. The
# sub-grid makes the result area-weighted to within 1/n^2 of a PU. `sum` is
# carried as a density so totals are kept.
#
# This replaces two earlier versions: warping straight onto the PU grid with
# bilinear interpolation (which made the area-weighted step a no-op), and
# reprojecting at the input's native resolution (unbounded memory, and worse
# for coarse inputs).
.align_to_pus <- function(x, pus, method = "mean", threads = TRUE,
                          max_subcells = 10, max_cells = 1e8) {
  if (terra::compareGeom(x, pus, stopOnError = FALSE)) {
    return(x)
  }

  if (terra::same.crs(x, pus)) {
    x <- .crop_to(x, pus)
    if (is.null(x)) return(terra::ifel(pus == 1, NA, NA))
    return(exactextractr::exact_resample(x = x, y = pus, fun = method))
  }

  x <- .crop_to(x, pus)
  if (is.null(x)) return(terra::ifel(pus == 1, NA, NA))

  # Sub-cells per PU side: enough to resolve the input, bounded in total size
  pu_m <- terra::res(pus)[1] * .metres_per_unit(pus)
  in_m <- .cell_size_m(x)
  n <- if (in_m < pu_m) ceiling(pu_m / in_m) else 5
  n <- max(1, min(n, max_subcells, floor(sqrt(max_cells / terra::ncell(pus)))))

  agg_fun <- switch(method,
    mean = "mean", sum = "sum", min = "min", max = "max",
    mode = "modal", majority = "modal", NULL
  )

  # Inputs much finer than a sub-cell: aggregate them in their own CRS first,
  # so sampling does not skip small features or blow up memory
  fact <- floor(pu_m / n / in_m)
  if (!is.null(agg_fun) && fact >= 2) {
    x <- terra::aggregate(x, fact = fact, fun = agg_fun, na.rm = TRUE)
  }

  if (identical(method, "sum")) {
    # Carry totals as a density, so cells of different size add up correctly
    x <- x / terra::cellSize(x, unit = "m", transform = FALSE)
  }

  template <- terra::disagg(terra::rast(pus), fact = n)
  warp_method <- switch(method,
    mean = "average", sum = "average", min = "min", max = "max",
    mode = "mode", majority = "mode", "near"
  )
  sampled <- terra::project(x, template, method = warp_method, threads = threads)

  if (is.null(agg_fun)) {
    return(exactextractr::exact_resample(x = sampled, y = pus, fun = method))
  }
  if (identical(method, "sum")) {
    density <- terra::aggregate(sampled, fact = n, fun = "mean", na.rm = TRUE)
    return(density * terra::cellSize(pus, unit = "m", transform = FALSE))
  }
  terra::aggregate(sampled, fact = n, fun = agg_fun, na.rm = TRUE)
}

# Crop x to the planning-unit footprint (densified, so curved edges in x's CRS
# are covered). NULL when they do not overlap.
.crop_to <- function(x, pus) {
  footprint <- terra::as.polygons(terra::ext(pus), crs = terra::crs(pus))
  if (!terra::same.crs(x, pus)) {
    step <- max(diff(terra::ext(pus)[1:2]), diff(terra::ext(pus)[3:4])) / 100
    footprint <- terra::project(terra::densify(footprint, step), terra::crs(x))
  }
  if (!terra::relate(terra::ext(footprint), terra::ext(x), "intersects")[1, 1]) {
    return(NULL)
  }
  terra::crop(x, footprint, snap = "out")
}

# Metres per CRS unit (projected rasters; elsar's planning units are in metres)
.metres_per_unit <- function(x) {
  u <- terra::linearUnits(x)
  if (is.na(u) || u <= 0) 1 else u
}

# Approximate cell size of x in metres
.cell_size_m <- function(x) {
  if (terra::is.lonlat(x)) {
    lat <- mean(terra::ext(x)[3:4])
    sqrt(prod(terra::res(x)) * 111320 * max(cos(lat * pi / 180), 0.01) * 110574)
  } else {
    sqrt(prod(terra::res(x))) * .metres_per_unit(x)
  }
}

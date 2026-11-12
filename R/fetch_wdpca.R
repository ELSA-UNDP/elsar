#' Fetch WDPCA protected areas for one country from several sources
#'
#' Gets the World Database on Protected and Conserved Areas (WDPCA) records for
#' one country, trying sources in order and using the first that works, so a
#' change to the Protected Planet website (which \pkg{wdpar} scrapes) no
#' longer stops a run.
#'
#' Sources, in the default order:
#' \describe{
#'   \item{`"cache"`}{A per-country GeoPackage written by an earlier call
#'     (`WDPCA_<ISO3>_<MonYYYY>.gpkg` in `download_dir`), or a per-country zip
#'     left there by \pkg{wdpar} (`WDPA_<MonYYYY>_<ISO3>-shapefile.zip`). The
#'     newest release is used. Skipped when `force_update = TRUE`, and when
#'     `global_path` holds a newer release.}
#'   \item{`"global"`}{The monthly global release (the
#'     `WDPA_WDOECM_<MonYYYY>_Public` zip, or its unzipped `.gdb`), read
#'     without unzipping it. Give its path in `global_path`. With
#'     `download_global = TRUE` and no `global_path`, the current release
#'     (about 2 GB) is downloaded from the fixed UNEP-WCMC address into
#'     `download_dir` first. Reading one country from the zip takes a few
#'     minutes, which is why the result is cached.}
#'   \item{`"api"`}{The UNEP-WCMC ArcGIS feature services behind the Protected
#'     Planet map (no key needed). Live data, not a dated release, so it is
#'     labelled with the month it was retrieved.}
#'   \item{`"wdpar"`}{[wdpar::wdpa_fetch()], which scrapes the Protected Planet
#'     website for the download link (needs a Chromium browser).}
#' }
#'
#' Whatever the source, the result has the WDPCA column names in upper case
#' (`SITE_ID`, `STATUS`, `SITE_TYPE`, `DESIG_ENG`, `REP_AREA`, `ISO3`, ...)
#' and both polygon and point records, in WGS 84. It is not filtered by
#' status, site type or designation: [make_protected_areas()] does that.
#' Transboundary sites (e.g. `ISO3 = "DEU;DNK;NLD"`) are included for each
#' country they list. Every source except the cache writes the result to the
#' cache.
#'
#' @param iso3 Character. ISO3 code. A sub-national code (`"USA_CA"`) fetches
#'   the national data (`"USA"`).
#' @param download_dir Character. Directory for the per-country cache (and the
#'   global download when `download_global = TRUE`). Created if missing.
#' @param global_path Character or NULL. Path to the global release zip or
#'   `.gdb`. Defaults to `getOption("elsar.wdpca_global")`, then the
#'   `ELSAR_WDPCA_GLOBAL` environment variable.
#' @param sources Character vector. Sources to try, in order. Any of
#'   `"cache"`, `"global"`, `"api"`, `"wdpar"`.
#' @param site_type Character vector. `"PA"` and/or `"OECM"`. Only changes
#'   which `"api"` services are queried (protected areas and OECMs are served
#'   separately); the other sources always return both.
#' @param force_update Logical. Skip the cache and re-fetch. Default `FALSE`.
#' @param download_global Logical. Download the global release when no
#'   `global_path` is set. Default `FALSE` (it is about 2 GB).
#'
#' @return An `sf` object with attributes `wdpca_source` (the source used) and
#'   `wdpca_release` (e.g. `"Nov2026"`).
#'
#' @examples
#' \dontrun{
#' pas <- fetch_wdpca("DNK", download_dir = "data/wdpa_downloads",
#'                    global_path = "WDPA_WDOECM_Nov2026_Public.zip")
#' attr(pas, "wdpca_release")
#' }
#' @export
fetch_wdpca <- function(
    iso3,
    download_dir,
    global_path = NULL,
    sources = c("cache", "global", "api", "wdpar"),
    site_type = c("PA", "OECM"),
    force_update = FALSE,
    download_global = FALSE
) {
  assertthat::assert_that(assertthat::is.string(iso3))
  assertthat::assert_that(assertthat::is.string(download_dir))
  assertthat::assert_that(
    length(sources) > 0,
    all(sources %in% c("cache", "global", "api", "wdpar")),
    msg = "sources must be some of: cache, global, api, wdpar"
  )
  iso3 <- iso3_base(iso3)
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

  global_path <- global_path %||% getOption("elsar.wdpca_global")
  if (is.null(global_path) && nzchar(Sys.getenv("ELSAR_WDPCA_GLOBAL"))) {
    global_path <- Sys.getenv("ELSAR_WDPCA_GLOBAL")
  }
  if (!is.null(global_path) && !file.exists(global_path)) {
    log_message("WDPCA global release not found at {global_path}; ignoring it.")
    global_path <- NULL
  }

  # A cached copy older than the configured global release is stale
  global_release <- if (!is.null(global_path)) wdpca_global_release(global_path) else NA
  errors <- character()

  for (src in sources) {
    result <- tryCatch(
      switch(src,
        cache = {
          if (force_update) stop("skipped (force_update = TRUE)", call. = FALSE)
          read_wdpca_cache(iso3, download_dir, newer_than = global_release)
        },
        global = {
          if (is.null(global_path) && download_global) {
            global_path <- download_wdpca_global(download_dir)
          }
          if (is.null(global_path)) {
            stop("no global release given (global_path, option elsar.wdpca_global ",
                 "or ELSAR_WDPCA_GLOBAL)", call. = FALSE)
          }
          read_wdpca_global(iso3, global_path)
        },
        api = read_wdpca_api(iso3, site_type),
        wdpar = read_wdpca_wdpar(iso3, download_dir, force_update)
      ),
      error = function(e) {
        errors[[src]] <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(result)) {
      log_message("WDPCA source '{src}' not used: {errors[[src]]}")
      next
    }

    attr(result, "wdpca_source") <- src
    release <- attr(result, "wdpca_release")
    log_message("WDPCA for {iso3}: {nrow(result)} record(s), release {release}, from {src}")
    if (src != "cache") write_wdpca_cache(result, iso3, download_dir)
    return(result)
  }

  stop(
    "Could not get WDPCA data for ", iso3, " from any source:\n",
    paste0("  - ", names(errors), ": ", errors, collapse = "\n"),
    "\nDownload the global release from https://www.protectedplanet.net/en/",
    "thematic-areas/wdpa and pass its path as global_path.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------------
# Sources
# ---------------------------------------------------------------------------

#' Read the newest cached WDPCA copy for a country
#'
#' @param iso3 Upper-case ISO3 code.
#' @param download_dir Cache directory.
#' @param newer_than Release label (`"Nov2026"`) or NA. A cache that is not
#'   newer than or equal to this release is ignored.
#' @return `sf`, or an error if there is no usable cache.
#' @keywords internal
read_wdpca_cache <- function(iso3, download_dir, newer_than = NA) {
  gpkg <- list.files(download_dir, paste0("^WDPCA_", iso3, "_[A-Za-z]{3}[0-9]{4}\\.gpkg$"),
                     full.names = TRUE)
  zips <- list.files(download_dir, paste0("^WDPA_[A-Za-z]{3}[0-9]{4}_", iso3, "-shapefile\\.zip$"),
                     full.names = TRUE)
  files <- c(gpkg, zips)
  if (length(files) == 0) stop("no cached copy in ", download_dir, call. = FALSE)

  releases <- vapply(basename(files), wdpca_release_from_name, character(1))
  order_key <- wdpca_release_key(releases)
  newest <- which.max(order_key)
  if (!is.na(newer_than) && order_key[newest] < wdpca_release_key(newer_than)) {
    stop("cached release ", releases[newest], " is older than the global release ",
         newer_than, call. = FALSE)
  }

  path <- files[newest]
  x <- if (grepl("\\.gpkg$", path)) {
    normalise_wdpca_columns(sf::st_read(path, quiet = TRUE))
  } else {
    read_wdpca_gdb(wdpca_zip_gdb(path), iso3 = NULL)
  }
  attr(x, "wdpca_release") <- releases[[newest]]
  x
}

#' Read one country from the global WDPCA release
#'
#' @param iso3 Upper-case ISO3 code.
#' @param global_path Path to the global zip or `.gdb`.
#' @return `sf`.
#' @keywords internal
read_wdpca_global <- function(iso3, global_path) {
  gdb <- if (grepl("\\.zip$", global_path, ignore.case = TRUE)) {
    wdpca_zip_gdb(global_path)
  } else {
    global_path
  }
  log_message("Reading {iso3} from the global WDPCA release (can take a few minutes)")
  x <- read_wdpca_gdb(gdb, iso3 = iso3)
  attr(x, "wdpca_release") <- wdpca_global_release(global_path)
  x
}

#' Query the UNEP-WCMC ArcGIS feature services for one country
#'
#' @param iso3 Upper-case ISO3 code.
#' @param site_type `"PA"` and/or `"OECM"`.
#' @return `sf`.
#' @keywords internal
read_wdpca_api <- function(iso3, site_type = c("PA", "OECM")) {
  base <- "https://data-gis.unep-wcmc.org/server/rest/services/ProtectedSites/"
  services <- c(
    PA = "The_World_Database_of_Protected_Areas",
    OECM = "The_World_Database_on_other_effective_area_based_conservation_measures"
  )[intersect(c("PA", "OECM"), site_type)]

  where <- utils::URLencode(sprintf("iso3 LIKE '%%%s%%'", iso3), reserved = TRUE)
  parts <- list()
  for (svc in services) {
    # Layer 0 = points, 1 = polygons
    for (layer in 0:1) {
      query <- paste0(base, svc, "/FeatureServer/", layer, "/query?where=", where)
      # GDAL cannot open a query with no results, so count first
      count <- jsonlite::fromJSON(paste0(query, "&returnCountOnly=true&f=json"))
      if (!is.null(count$error)) {
        stop("the service returned an error: ", count$error$message, call. = FALSE)
      }
      if (is.null(count$count) || count$count == 0) next
      # GDAL's ESRIJSON driver follows the service's paging itself
      x <- suppressWarnings(sf::st_read(
        paste0("ESRIJSON:", query, "&outFields=*&outSR=4326&f=json"), quiet = TRUE
      ))
      parts[[length(parts) + 1]] <- x
    }
  }
  if (length(parts) == 0) {
    stop("the services have no records for ", iso3, call. = FALSE)
  }
  x <- bind_wdpca(lapply(parts, normalise_wdpca_columns))
  x <- filter_wdpca_iso3(x, iso3)
  # Pages can overlap; keep one record per site parcel
  key <- intersect(c("SITE_PID", "SITE_ID"), names(x))[1]
  if (!is.na(key)) x <- x[!duplicated(x[[key]]), ]
  attr(x, "wdpca_release") <- format_wdpca_release(Sys.Date())
  x
}

#' Fetch via wdpar (scrapes the Protected Planet website)
#'
#' @param iso3 Upper-case ISO3 code.
#' @param download_dir Download directory.
#' @param force_update Force a re-download.
#' @return `sf`.
#' @keywords internal
read_wdpca_wdpar <- function(iso3, download_dir, force_update = FALSE) {
  if (!requireNamespace("wdpar", quietly = TRUE)) {
    stop("the 'wdpar' package is not installed", call. = FALSE)
  }
  # wait = FALSE: when the site cannot be read wdpar would otherwise retry
  # every 5 minutes, forever
  x <- wdpar::wdpa_fetch(iso3, wait = FALSE, check_version = FALSE,
                         force_download = force_update, download_dir = download_dir)
  x <- normalise_wdpca_columns(x)
  zips <- list.files(download_dir, paste0("^WDPA_[A-Za-z]{3}[0-9]{4}_", iso3, "-shapefile\\.zip$"))
  attr(x, "wdpca_release") <- if (length(zips)) {
    rel <- vapply(zips, wdpca_release_from_name, character(1))
    rel[[which.max(wdpca_release_key(rel))]]
  } else {
    format_wdpca_release(Sys.Date())
  }
  x
}

#' Download the current global WDPCA release
#'
#' @param download_dir Directory to save into.
#' @return Path to the zip.
#' @keywords internal
download_wdpca_global <- function(download_dir) {
  url <- "https://wcmc.io/wdpa_current_release"
  existing <- list.files(download_dir, "^WDPA_WDOECM_[A-Za-z]{3}[0-9]{4}_Public\\.zip$",
                         full.names = TRUE)
  current <- format_wdpca_release(Sys.Date())
  hit <- existing[vapply(basename(existing), wdpca_release_from_name, character(1)) == current]
  if (length(hit)) return(hit[[1]])

  dest <- file.path(download_dir, paste0("WDPA_WDOECM_", current, "_Public.zip"))
  log_message("Downloading the global WDPCA release (~2 GB) to {dest}")
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  tmp <- paste0(dest, ".part")
  status <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  if (status != 0) stop("download of ", url, " failed", call. = FALSE)
  # Name the file after the release it actually contains
  gdb <- wdpca_zip_gdb(tmp)
  release <- wdpca_release_from_name(basename(gdb))
  if (is.na(release)) release <- current
  dest <- file.path(download_dir, paste0("WDPA_WDOECM_", release, "_Public.zip"))
  file.rename(tmp, dest)
  dest
}


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Read the polygon and point layers of a WDPCA file geodatabase
#'
#' @param gdb GDAL path to the `.gdb` (may be a `/vsizip/` path).
#' @param iso3 Upper-case ISO3 to filter on, or NULL to read everything (a
#'   per-country file).
#' @return `sf` with normalised columns.
#' @keywords internal
read_wdpca_gdb <- function(gdb, iso3 = NULL) {
  layers <- sf::st_layers(gdb)$name
  layers <- layers[grepl("_(poly|point)", layers, ignore.case = TRUE)]
  if (length(layers) == 0) stop("no polygon or point layer in ", gdb, call. = FALSE)
  parts <- lapply(layers, function(l) {
    if (is.null(iso3)) {
      sf::st_read(gdb, layer = l, quiet = TRUE)
    } else {
      # LIKE, not =: transboundary sites list several codes ("DEU;DNK;NLD")
      sf::st_read(gdb, quiet = TRUE, query = sprintf(
        "SELECT * FROM \"%s\" WHERE ISO3 LIKE '%%%s%%'", l, iso3
      ))
    }
  })
  x <- bind_wdpca(lapply(parts, normalise_wdpca_columns))
  if (!is.null(iso3)) x <- filter_wdpca_iso3(x, iso3)
  x
}

#' GDAL path to the `.gdb` inside a WDPCA zip
#' @param zip_path Path to the zip.
#' @return `/vsizip/` path.
#' @keywords internal
wdpca_zip_gdb <- function(zip_path) {
  gdb <- gdb_from_zip_entries(utils::unzip(zip_path, list = TRUE)$Name)
  if (length(gdb) == 0) stop("no .gdb inside ", zip_path, call. = FALSE)
  paste0("/vsizip/", normalizePath(zip_path), "/", gdb[[1]])
}

#' Names of the `.gdb` folders among zip entries
#'
#' `useBytes`: the global release bundles manuals whose names are not valid
#' UTF-8 ("Ressources_en_Fran\\x87ais"), which make plain `grepl()` fail.
#' @param entries Zip entry names.
#' @return Unique `.gdb` folder names.
#' @keywords internal
gdb_from_zip_entries <- function(entries) {
  entries <- entries[grepl("\\.gdb/", entries, useBytes = TRUE)]
  unique(sub("(\\.gdb)/.*$", "\\1", entries, useBytes = TRUE))
}

#' Upper-case WDPCA column names and name the geometry column "geometry"
#' @param x `sf`.
#' @return `sf`.
#' @keywords internal
normalise_wdpca_columns <- function(x) {
  geom <- sf::st_geometry(x)
  df <- sf::st_drop_geometry(x)
  names(df) <- toupper(names(df))
  drop <- c("OBJECTID", "SHAPE_LENGTH", "SHAPE_AREA", "SHAPE__LENGTH", "SHAPE__AREA")
  df <- df[, setdiff(names(df), drop), drop = FALSE]
  sf::st_sf(df, geometry = sf::st_transform(geom, 4326))
}

#' Row-bind WDPCA parts that may have different columns or geometry types
#' @param parts List of `sf` with normalised columns.
#' @return `sf`.
#' @keywords internal
bind_wdpca <- function(parts) {
  parts <- Filter(Negate(is.null), parts)
  # Empty layers (e.g. a country with no point records) add nothing
  non_empty <- Filter(function(p) nrow(p) > 0, parts)
  if (length(non_empty) > 0) parts <- non_empty
  cols <- unique(unlist(lapply(parts, function(p) setdiff(names(p), "geometry"))))
  parts <- lapply(parts, function(p) {
    for (col in setdiff(cols, names(p))) p[[col]] <- rep(NA, nrow(p))
    p[, c(cols, "geometry")]
  })
  do.call(rbind, parts)
}

#' Keep records whose ISO3 list contains the code
#' @param x `sf` with an `ISO3` column.
#' @param iso3 Upper-case ISO3 code.
#' @return `sf`.
#' @keywords internal
filter_wdpca_iso3 <- function(x, iso3) {
  if (!"ISO3" %in% names(x) || nrow(x) == 0) return(x)
  codes <- strsplit(toupper(as.character(x$ISO3)), "\\s*;\\s*")
  x[vapply(codes, function(cd) iso3 %in% cd, logical(1)), ]
}

#' Write a WDPCA result to the per-country cache
#' @param x `sf` with a `wdpca_release` attribute.
#' @param iso3 Upper-case ISO3 code.
#' @param download_dir Cache directory.
#' @return The cache path, invisibly (NULL if writing failed).
#' @keywords internal
write_wdpca_cache <- function(x, iso3, download_dir) {
  path <- file.path(download_dir, paste0("WDPCA_", iso3, "_", attr(x, "wdpca_release"), ".gpkg"))
  ok <- tryCatch({
    sf::st_write(x, path, delete_dsn = TRUE, quiet = TRUE)
    TRUE
  }, error = function(e) {
    log_message("Could not cache WDPCA to {path}: {conditionMessage(e)}")
    FALSE
  })
  invisible(if (ok) path else NULL)
}

#' Release label of a global WDPCA release
#'
#' From the file name when it carries one (`WDPA_WDOECM_Nov2026_Public.zip`),
#' otherwise from the `.gdb` inside the zip, so a renamed download
#' (`WDPCA_download_20261102.zip`) still says which release it is. Listing a
#' zip's entries does not read the data, so this is quick.
#' @param global_path Path to the global zip or `.gdb`.
#' @return Label, or NA.
#' @keywords internal
wdpca_global_release <- function(global_path) {
  release <- wdpca_release_from_name(basename(global_path))
  if (is.na(release) && grepl("\\.zip$", global_path, ignore.case = TRUE)) {
    gdb <- tryCatch(wdpca_zip_gdb(global_path), error = function(e) NA_character_)
    if (!is.na(gdb)) release <- wdpca_release_from_name(basename(gdb))
  }
  release
}

#' Release label ("Nov2026") from a WDPCA file or layer name
#' @param x File or layer name.
#' @return Label, or NA if there is none.
#' @keywords internal
wdpca_release_from_name <- function(x) {
  m <- regmatches(x, regexpr(paste0("(", paste(month.abb, collapse = "|"), ")[0-9]{4}"), x))
  if (length(m) == 0) NA_character_ else m
}

#' Sortable number for a release label ("Nov2026" -> 202611)
#' @param x Release labels.
#' @return Numeric (NA for unparseable labels, sorted last).
#' @keywords internal
wdpca_release_key <- function(x) {
  mon <- match(substr(x, 1, 3), month.abb)
  yr <- suppressWarnings(as.numeric(substr(x, 4, 7)))
  key <- yr * 100 + mon
  key[is.na(key)] <- -Inf
  key
}

#' Release label for a date, independent of the locale
#' @param date Date.
#' @return Label such as "Nov2026".
#' @keywords internal
format_wdpca_release <- function(date) {
  paste0(month.abb[as.integer(format(date, "%m"))], format(date, "%Y"))
}

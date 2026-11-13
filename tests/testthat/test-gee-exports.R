# Earth Engine export helpers (0.4.3). Drive, tasks and sleeping are mocked:
# these tests need no network, Earth Engine or Google Drive.

dribble <- function(names) data.frame(name = names, id = sprintf("id_%d", seq_along(names)))

# A fake Earth Engine task whose status() steps through `states`, staying on
# the last one
fake_task <- function(states, description = "exp", error_message = NULL) {
  i <- 0
  list(status = function() {
    i <<- min(i + 1, length(states))
    list(state = states[i], description = description, error_message = error_message)
  })
}

test_that("Drive matching is exact: the export's own file and tiles only", {
  rx <- .gee_export_regex("esri_10m_proportion_2024_ECU_agriculture")
  keep <- c("esri_10m_proportion_2024_ECU_agriculture.tif",
            "esri_10m_proportion_2024_ECU_agriculture-0000000000-0000065536.tif")
  drop <- c("esri_10m_proportion_2024_ECU_agriculture_old.tif",
            "esri_10m_proportion_2024_ECU_agriculture.tif.bak",
            "esri_10m_proportion_2024_ECU_agriculture-1.tif",
            "copy of esri_10m_proportion_2024_ECU_agriculture.tif")
  expect_true(all(grepl(rx, keep)))
  expect_false(any(grepl(rx, drop)))
  # A national code does not pick up the sub-national export
  expect_false(grepl(.gee_export_regex("esri_10m_lulc_2024_ECU"), "esri_10m_lulc_2024_ECU_REG.tif"))
  # Regex metacharacters in names are literal
  expect_false(grepl(.gee_export_regex("a.b"), "axb.tif"))
  expect_true(grepl(.gee_export_regex("a.b(1)"), "a.b(1).tif"))
})

test_that(".drive_export_files keeps only the export's files", {
  local_mocked_bindings(.drive_find = function(...) dribble(c(
    "lulc_2024_ECU.tif", "lulc_2024_ECU_REG.tif",
    "lulc_2024_ECU-0000000000-0000000000.tif", "lulc_2024_ECU.json"
  )))
  expect_equal(.drive_export_files("lulc_2024_ECU")$name,
               c("lulc_2024_ECU.tif", "lulc_2024_ECU-0000000000-0000000000.tif"))
})

test_that("wait_time is a positive number of minutes, fractions allowed", {
  expect_silent(.check_wait_time(2.5))
  for (bad in list(0, -1, NA_real_, Inf, "3", c(1, 2))) {
    expect_error(.check_wait_time(bad), "maximum wait in minutes")
  }
})

test_that("the wait follows the task and returns every tile", {
  sleeps <- 0
  calls <- 0
  local_mocked_bindings(
    .gee_sleep = function(s) sleeps <<- sleeps + 1,
    .drive_find = function(...) {
      calls <<- calls + 1
      # One tile is visible while the task runs; both once it completes
      if (calls < 3) dribble("exp-0000000000-0000000000.tif")
      else dribble(c("exp-0000000000-0000000000.tif", "exp-0000000000-0000065536.tif"))
    }
  )
  task <- fake_task(c("RUNNING", "RUNNING", "COMPLETED"))
  files <- suppressMessages(.wait_for_export("exp", task = task, wait_time = 5))
  expect_equal(nrow(files), 2)
  expect_equal(sleeps, 3) # two while running, one to confirm Drive's listing
})

test_that("after COMPLETED, files are taken only once Drive's listing is stable", {
  counts <- c(1, 2, 2)
  calls <- 0
  local_mocked_bindings(
    .gee_sleep = function(s) NULL,
    .drive_find = function(...) {
      calls <<- calls + 1
      dribble(sprintf("exp-0000000000-%010d.tif", seq_len(counts[min(calls, 3)])))
    }
  )
  files <- suppressMessages(.wait_for_export("exp", task = fake_task("COMPLETED"), wait_time = 5))
  expect_equal(nrow(files), 2)
})

test_that("a completed task keeps polling until Drive lists its files", {
  calls <- 0
  local_mocked_bindings(
    .gee_sleep = function(s) NULL,
    .drive_find = function(...) {
      calls <<- calls + 1
      if (calls < 3) dribble(character(0)) else dribble("exp.tif")
    }
  )
  files <- suppressMessages(.wait_for_export("exp", task = fake_task("COMPLETED"), wait_time = 5))
  expect_equal(files$name, "exp.tif")
})

test_that("a failed or cancelled export is an error with Earth Engine's message", {
  local_mocked_bindings(.gee_sleep = function(s) NULL,
                        .drive_find = function(...) dribble(character(0)))
  expect_error(
    suppressMessages(.wait_for_export("exp", fake_task("FAILED", error_message = "User memory limit exceeded."),
                                      wait_time = 5)),
    "exp' failed: User memory limit exceeded"
  )
  expect_error(suppressMessages(.wait_for_export("exp", fake_task("CANCELLED"), wait_time = 5)),
               "cancelled")
})

test_that("a timeout is an error, never a partial set of tiles", {
  local_mocked_bindings(.gee_sleep = function(s) Sys.sleep(0.01),
                        .drive_find = function(...) dribble("exp-0000000000-0000000000.tif"))
  expect_error(
    suppressMessages(.wait_for_export("exp", fake_task("RUNNING"), wait_time = 0.001)),
    "not finished after .*task state: RUNNING"
  )
})

test_that("without a task, files are returned once their number is stable", {
  counts <- c(1, 2, 2, 2)
  calls <- 0
  local_mocked_bindings(
    .gee_sleep = function(s) NULL,
    .drive_find = function(...) {
      calls <<- calls + 1
      n <- counts[min(calls, length(counts))]
      dribble(sprintf("exp-0000000000-%010d.tif", seq_len(n)))
    }
  )
  files <- suppressMessages(.wait_for_export("exp", task = NULL, wait_time = 5))
  expect_equal(nrow(files), 2)
})

test_that("listed tasks are read without a status request per task", {
  listed <- function(state, description) list(
    state = state, config = list(description = description),
    status = function() stop("status() should not be needed")
  )
  ee <- list(batch = list(Task = list(list = function() list(
    listed("COMPLETED", "exp"), listed("RUNNING", "exp")
  ))))
  expect_equal(.find_active_task(ee, "exp")$state, "RUNNING")
})

test_that("a running task is waited for before Drive is looked at", {
  # Tiles of a running export can already be in Drive: they must not be
  # downloaded as if the export were complete
  ee_with <- function(...) list(batch = list(Task = list(list = function() list(...))))
  trashed <- 0
  local_mocked_bindings(
    .drive_find = function(...) dribble("exp-0000000000-0000000000.tif"),
    .drive_trash = function(f) trashed <<- trashed + nrow(f)
  )
  r <- suppressMessages(.resolve_export(ee_with(fake_task("RUNNING", "exp")), "exp"))
  expect_equal(r$status, "running")
  r <- suppressMessages(.resolve_export(ee_with(fake_task("COMPLETED", "exp")), "exp"))
  expect_equal(r$status, "in_drive")
  expect_equal(nrow(r$files), 1)
  # force_download trashes the old export and exports again
  r <- suppressMessages(.resolve_export(ee_with(), "exp", force_download = TRUE))
  expect_equal(r$status, "export")
  expect_equal(trashed, 1)
})

test_that("nothing running and nothing in Drive means a new export", {
  local_mocked_bindings(.drive_find = function(...) dribble(character(0)))
  ee <- list(batch = list(Task = list(list = function() list())))
  expect_equal(.resolve_export(ee, "exp")$status, "export")
})

test_that("a running task with the same description is found for reuse", {
  ee <- list(batch = list(Task = list(list = function() list(
    fake_task("COMPLETED", "exp"), fake_task("RUNNING", "exp_other"), fake_task("RUNNING", "exp")
  ))))
  task <- .find_active_task(ee, "exp")
  expect_equal(task$status()$description, "exp")
  expect_null(.find_active_task(ee, "missing"))
})

tile <- function(xmin, crs = "EPSG:3857", res = 10) {
  terra::rast(nrows = 2, ncols = 2, xmin = xmin, xmax = xmin + 2 * res, ymin = 0, ymax = 2 * res,
              crs = crs, vals = 1:4)
}

test_that("merge_tiles merges only the given files and leaves no partial file", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.tif"); b <- file.path(dir, "b.tif")
  terra::writeRaster(tile(0), a); terra::writeRaster(tile(20), b)
  # A stray tile from another export in the same folder is not merged
  terra::writeRaster(tile(1000), file.path(dir, "stray.tif"))
  out <- file.path(dir, "merged.tif")
  r <- suppressMessages(merge_tiles(c(a, b), out, "INT1U"))
  expect_equal(as.vector(terra::ext(r)), c(xmin = 0, xmax = 40, ymin = 0, ymax = 20))
  expect_false(file.exists(paste0(out, ".part.tif")))
  # A directory still works (every .tif in it)
  sub <- file.path(dir, "sub"); dir.create(sub)
  file.copy(c(a, b), sub)
  r2 <- suppressMessages(merge_tiles(sub, file.path(dir, "merged2.tif"), "INT1U"))
  expect_equal(terra::ncell(r2), 8)
})

test_that("merge_tiles refuses tiles with another CRS or resolution", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.tif"); b <- file.path(dir, "b.tif"); c <- file.path(dir, "c.tif")
  terra::writeRaster(tile(0), a)
  terra::writeRaster(tile(20, crs = "EPSG:32631"), b)
  terra::writeRaster(tile(20, res = 5), c)
  expect_error(suppressMessages(merge_tiles(c(a, b), file.path(dir, "m.tif"), "INT1U")), "b.tif differ")
  expect_error(suppressMessages(merge_tiles(c(a, c), file.path(dir, "m.tif"), "INT1U")), "c.tif differ")
  expect_error(merge_tiles(file.path(dir, "nope.tif"), file.path(dir, "m.tif"), "INT1U"), "not found")
})

test_that("cached LULC proportions from a fallback year are used without Earth Engine", {
  dir <- withr::local_tempdir()
  pus <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2000, ymin = 0, ymax = 2000,
                     crs = "ESRI:54009", vals = 1)
  boundary <- sf::st_as_sf(terra::as.polygons(terra::ext(pus), crs = terra::crs(pus)))
  for (cn in c("agriculture", "built_area")) {
    terra::writeRaster(pus * 0.5, file.path(dir, glue::glue("esri_10m_proportion_2023_TST_{cn}.tif")))
  }
  local_mocked_bindings(initialize_earthengine = function(...) stop("Earth Engine was started"))
  props <- suppressMessages(download_lulc_proportions(
    boundary_layer = boundary, iso3 = "TST", gee_project = "none", output_dir = dir, pus = pus
  ))
  expect_named(props, c("agriculture", "built_area"))
  expect_true(attr(props$agriculture, "pre_aggregated"))
  # With an explicit year that is not cached, Earth Engine is needed
  expect_error(suppressMessages(download_lulc_proportions(
    boundary_layer = boundary, iso3 = "TST", gee_project = "none", output_dir = dir, pus = pus,
    year = 2024
  )), "Earth Engine was started")
})

test_that("download_lulc_data passes the year to ESRI and warns for ESA", {
  boundary <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  got <- NULL
  local_mocked_bindings(
    download_gee_layer = function(...) { got <<- list(...); terra::rast(nrows = 1, ncols = 1, vals = 1) },
    download_esa_worldcover_data = function(...) terra::rast(nrows = 1, ncols = 1, vals = 1)
  )
  suppressMessages(download_lulc_data(boundary, "TST", product = "esri_10m",
                                      gee_project = "none", year = 2019))
  expect_equal(got$year_override, 2019)
  # year_override given directly still works (no duplicate argument)
  suppressMessages(download_lulc_data(boundary, "TST", product = "esri_10m",
                                      gee_project = "none", year_override = 2018))
  expect_equal(got$year_override, 2018)
  expect_warning(download_lulc_data(boundary, "TST", product = "esa_worldcover",
                                    gee_project = "none", year = 2020), "only available for 2021")
})

test_that("a Drive download retries network errors, then succeeds", {
  tries <- 0
  waits <- numeric(0)
  local_mocked_bindings(
    .gee_sleep = function(s) waits <<- c(waits, s),
    .drive_download_once = function(file, path) {
      tries <<- tries + 1
      if (tries < 3) stop("OpenSSL SSL_read: error:0A000119:SSL routines::decryption failed or bad record mac")
      writeLines("ok", path)
      invisible(path)
    }
  )
  path <- withr::local_tempfile(fileext = ".tif")
  expect_no_error(.drive_download(dribble("exp.tif"), path))
  expect_equal(tries, 3)
  expect_equal(waits, c(10, 20))
  expect_true(file.exists(path))
})

test_that("a Drive download that keeps failing is an error and leaves no file", {
  tries <- 0
  local_mocked_bindings(
    .gee_sleep = function(s) NULL,
    .drive_download_once = function(file, path) {
      tries <<- tries + 1
      writeLines("partial", path)
      stop("Failure when receiving data from the peer")
    }
  )
  withr::local_options(elsar.drive_download_attempts = 2)
  path <- withr::local_tempfile(fileext = ".tif")
  expect_error(.drive_download(dribble("exp-0000000000-0000032768.tif"), path),
               "Could not download 'exp-0000000000-0000032768.tif' .* after 2 attempt")
  expect_equal(tries, 2)
  expect_false(file.exists(path))
})

test_that("one failed tile download is retried inside a full export download", {
  failed <- FALSE
  local_mocked_bindings(
    .gee_sleep = function(s) NULL,
    .drive_download_once = function(file, path) {
      if (grepl("0000032768", file$name) && !failed) {
        failed <<- TRUE
        stop("SSL_read: decryption failed")
      }
      writeLines(file$name, path)
    }
  )
  dir <- withr::local_tempdir()
  files <- dribble(c("exp-0000000000-0000000000.tif", "exp-0000000000-0000032768.tif"))
  paths <- .download_drive_export(files, dir)
  expect_true(failed)
  expect_true(all(file.exists(paths)))
})

test_that("GDAL's COG creation-option warnings are muffled, others are not", {
  expect_no_warning(.muffle_cog_warnings({
    warning("GDAL Message 6: driver GTiff does not support creation option OVERVIEWS")
    warning("GDAL Message 6: driver MEM does not support creation option COMPRESS")
    1
  }))
  expect_warning(.muffle_cog_warnings(warning("something else")), "something else")
  expect_equal(.muffle_cog_warnings(42), 42)
})

test_that("a failed Drive listing during the wait is retried, not fatal", {
  calls <- 0
  local_mocked_bindings(
    .gee_sleep = function(s) NULL,
    .drive_find = function(...) {
      calls <<- calls + 1
      if (calls == 1) stop("Failure when receiving data from the peer")
      dribble("exp.tif")
    }
  )
  files <- .wait_for_export("exp", task = fake_task("COMPLETED"), wait_time = 1)
  expect_equal(files$name, "exp.tif")
  expect_gte(calls, 3)
})

test_that(".gee_shard_size keeps each export block under Earth Engine's input limit", {
  # Planning units up to ~312 m keep the default 256-pixel blocks
  expect_equal(.gee_shard_size(225, 10), 256L)
  expect_equal(.gee_shard_size(10, 10), 256L)
  expect_equal(.gee_shard_size(5, 10), 256L)
  # 400 m from 10 m: 256 * 40 = 10240 native pixels failed; 200 * 40 = 8000 is under 8192
  expect_equal(.gee_shard_size(400, 10), 200L)
  expect_equal(.gee_shard_size(900, 10), 88L)
  for (r in c(330, 400, 700, 900, 2000, 5000)) {
    expect_lte(.gee_shard_size(r, 10) * r / 10, 8192)
  }
  # A projected grid at high latitude reads more degree-based pixels per block
  expect_lt(.gee_shard_size(400, 10, lat_stretch = 1 / cos(60 * pi / 180)),
            .gee_shard_size(400, 10))
  # Never below 1 or above 256, and safe on odd input
  expect_equal(.gee_shard_size(1e6, 10), 1L)
  expect_equal(.gee_shard_size(NA, 10), 256L)
})

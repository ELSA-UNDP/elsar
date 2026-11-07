# Shared Earth Engine export helpers: find an export's files in Google Drive,
# wait for the export task, and download every tile.
#
# Before 0.4.3 each download function had its own copy of this loop. The
# copies matched Drive files with an unanchored pattern (so "..._ECU" also
# matched "..._ECU_REG.tif"), waited for files to appear rather than for the
# task to finish, and either downloaded only the first tile or merged whatever
# tiles had arrived when the wait ran out.
#
# Drive, task and sleep calls go through the small wrappers below so the
# helpers can be tested without a network connection.

.drive_find <- function(...) suppressMessages(googledrive::drive_find(...))

.drive_download <- function(file, path) {
  suppressMessages(googledrive::drive_download(file, path = path, overwrite = TRUE))
}

.drive_trash <- function(file) suppressMessages(googledrive::drive_trash(file))

.gee_sleep <- function(seconds) Sys.sleep(seconds)

# Seconds between checks on a running export
.gee_poll_seconds <- function() getOption("elsar.gee_poll_seconds", 30)

# Escape regular-expression metacharacters in a literal string
.escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

# Regex for the files one export writes: "<name>.tif", or tiles named
# "<name>-0000000000-0000000000.tif" when Earth Engine splits a large export
.gee_export_regex <- function(name) {
  paste0("^", .escape_regex(name), "(-[0-9]{10}-[0-9]{10})?\\.tif$")
}

# The Drive files (a dribble) written by the export called `name`, and
# nothing else: not files whose names merely start with it
.drive_export_files <- function(name) {
  found <- .drive_find(q = paste0("name contains '", name, "'"), n_max = 1000)
  found[grepl(.gee_export_regex(name), found$name), ]
}

# Move the Drive files of an export to the bin (so a forced refresh cannot
# pick up the previous export). Returns the number of files trashed.
.trash_drive_export <- function(name) {
  files <- .drive_export_files(name)
  if (nrow(files) > 0) {
    log_message("Removing {nrow(files)} previous Drive file(s) for '{name}'")
    .drive_trash(files)
  }
  invisible(nrow(files))
}

# wait_time is the maximum wait in minutes; fractions are allowed
.check_wait_time <- function(wait_time) {
  assertthat::assert_that(
    is.numeric(wait_time), length(wait_time) == 1, is.finite(wait_time), wait_time > 0,
    msg = "'wait_time' must be a single positive number: the maximum wait in minutes."
  )
}

# State and description of a task from Task.list(). Listed tasks already
# carry both, so this avoids one status request per task; it falls back to
# task$status() if they cannot be read.
.listed_task_info <- function(task) {
  info <- tryCatch(
    list(state = as.character(task$state), description = as.character(task$config$description)),
    error = function(e) NULL
  )
  if (is.null(info) || length(info$state) != 1 || length(info$description) != 1) {
    info <- safe_task_status(task)
  }
  info
}

# The READY or RUNNING Earth Engine task with this description, or NULL. A
# rerun after a timeout then waits for that task instead of exporting again.
.find_active_task <- function(ee, description) {
  tasks <- tryCatch(ee$batch$Task$list(), error = function(e) list())
  purrr::detect(tasks, function(t) {
    info <- .listed_task_info(t)
    !is.null(info) &&
      isTRUE(info$state %in% c("READY", "RUNNING")) &&
      identical(info$description, as.character(description))
  })
}

# What to do for one export, in this order:
# 1. a task with this name still running: wait for it (its tiles may already
#    be partly in Drive, so Drive is not looked at first);
# 2. the export's files in Drive: download them (not with force_download,
#    which trashes them so the export runs again);
# 3. otherwise: export.
# Returns list(status = "running" | "in_drive" | "export", task, files).
.resolve_export <- function(ee, name, force_download = FALSE) {
  task <- .find_active_task(ee, name)
  if (!is.null(task)) {
    log_message("An export task for '{name}' is already running; waiting for it")
    return(list(status = "running", task = task, files = NULL))
  }
  if (force_download) {
    .trash_drive_export(name)
  } else {
    files <- .drive_export_files(name)
    if (nrow(files) > 0) {
      log_message("Found '{name}' in Drive ({nrow(files)} file(s)), will download")
      return(list(status = "in_drive", task = NULL, files = files))
    }
  }
  list(status = "export", task = NULL, files = NULL)
}

# Wait for the export called `name` and return its Drive files (every tile).
#
# With a task handle, the task state decides: once it is COMPLETED, the files
# are returned when Drive lists the same number twice in a row; FAILED or
# CANCELLED is an error with Earth Engine's message. Without one (or if its
# state cannot be read), the files are returned once their number has stayed
# the same for three checks in a row. Running past `wait_time` minutes is an error: a
# partial set of tiles is never returned.
.wait_for_export <- function(name, task = NULL, wait_time, start_time = Sys.time()) {
  .check_wait_time(wait_time)
  poll <- .gee_poll_seconds()
  previous_count <- -1
  stable_checks <- 0

  repeat {
    status <- if (!is.null(task)) safe_task_status(task) else NULL
    state <- if (is.null(status)) NA_character_ else status$state

    if (isTRUE(state %in% c("FAILED", "CANCELLED"))) {
      detail <- if (!is.null(status$error_message) && !is.na(status$error_message)) {
        paste0(": ", status$error_message)
      } else {
        ""
      }
      stop(glue::glue("Earth Engine export '{name}' {tolower(state)}{detail}"), call. = FALSE)
    }

    files <- .drive_export_files(name)
    count <- nrow(files)

    # Drive listings can lag behind the upload, so the files are taken only
    # once their number has stayed the same between checks: after the task
    # reports COMPLETED (one repeat), or, without a readable task state, two
    stable_checks <- if (count > 0 && count == previous_count) stable_checks + 1 else 0
    previous_count <- count
    needed <- if (!is.na(state)) 1 else 2
    if (count > 0 && (is.na(state) || state == "COMPLETED") && stable_checks >= needed) {
      log_message("Export '{name}' ready: {count} file(s)")
      return(files)
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    if (elapsed > wait_time) {
      stop(glue::glue(
        "Earth Engine export '{name}' was not finished after {round(wait_time, 1)} min",
        "{if (!is.na(state)) paste0(' (task state: ', state, ')') else ''}.\n",
        "It keeps running in Earth Engine: check https://code.earthengine.google.com/tasks ",
        "and run again later. The rerun waits for the same task instead of exporting again, ",
        "or downloads the files once they are in Drive."
      ), call. = FALSE)
    }

    log_message("Waiting for export '{name}'... ({round(elapsed, 1)} min{if (!is.na(state)) paste0(', ', state) else ''})")
    .gee_sleep(poll)
  }
}

# Download every file of an export into `dest_dir`; returns the local paths
.download_drive_export <- function(files, dest_dir) {
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(dest_dir, files$name)
  for (i in seq_len(nrow(files))) {
    .drive_download(files[i, ], paths[i])
    log_message("Downloaded: {files$name[i]}")
  }
  paths
}

# A fresh scratch directory for one export's tiles. Callers remove it with
# on.exit(). It sits under $HOME as before: a 10 m national export can be
# several GB, more than a small /tmp holds.
.gee_scratch_dir <- function(prefix) {
  dir <- tempfile(paste0(prefix, "_"), tmpdir = getOption("elsar.gee_tmpdir", Sys.getenv("HOME")))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

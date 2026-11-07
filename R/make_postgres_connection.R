#' Build PostgreSQL Connection Parameters
#'
#' Collects the parameters [elsar_load_data()] needs to read from a PostGIS
#' database (`file_name = "postgres"`, with `db_info = ` this list).
#'
#' This function was removed in 0.4.0 without a deprecation and is restored in
#' 0.4.2, because existing code (including the ELSA pipeline) calls it.
#'
#' @param host Character. Database host (default `"localhost"`).
#' @param dbname Character. Database name.
#' @param port Integer. Port (default `5432`).
#' @param user Character. User name.
#' @param password Character. Password. Avoid writing it in scripts: read it
#'   from an environment variable, e.g. `Sys.getenv("PGPASSWORD")`.
#'
#' @return A named list with elements `host`, `dbname`, `port`, `user` and
#'   `password`.
#' @seealso [elsar_load_data()]
#' @export
#'
#' @examples
#' db <- make_postgres_connection(
#'   host = "localhost", dbname = "elsa", user = "analyst",
#'   password = Sys.getenv("PGPASSWORD")
#' )
#' names(db)
make_postgres_connection <- function(host = "localhost",
                                     dbname,
                                     port = 5432,
                                     user,
                                     password) {
  assertthat::assert_that(!missing(dbname), assertthat::is.string(dbname),
                          msg = "The 'dbname' parameter is required.")
  assertthat::assert_that(!missing(user), assertthat::is.string(user),
                          msg = "The 'user' parameter is required.")
  assertthat::assert_that(!missing(password),
                          msg = "The 'password' parameter is required.")
  list(
    host = host,
    dbname = dbname,
    port = as.integer(port),
    user = user,
    password = password
  )
}

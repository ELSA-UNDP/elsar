#' Create a PostgreSQL Connection Configuration as a Dictionary
#'
#' This function generates a PostgreSQL connection configuration using a named list, which can be used directly for database connection functions.
#'
#' @param host The host of the database. Defaults to 'localhost' for a local PostgreSQL database, but can also accept an IP address (e.g., '192.168.1.1').
#' @param dbname The name of the PostgreSQL database. This parameter is required.
#' @param port The port on which the PostgreSQL server is running. Defaults to 5432.
#' @param user A valid database user name. This parameter is required.
#' @param password The password for the specified user. This parameter is required.
#'
#' @return A named list representing the PostgreSQL connection configuration.
#' @export
#'
#' @examples
#' # Example with explicit parameters
#' connection_dict <- make_postgres_connection(
#'   host = 'localhost',
#'   dbname = 'mydatabase',
#'   port = 5432,
#'   user = 'myuser',
#'   password = 'mypassword'
#' )
#'
#' # Use the connection dictionary in other functions
#' load_data(file_name = 'mytable', file_type = 'postgres', db_info = connection_dict)
make_postgres_connection <- function(host = "localhost",
                                     dbname,
                                     port = 5432,
                                     user,
                                     password) {
  # Check for required parameters
  if (is.null(dbname)) {
    stop("The 'dbname' parameter is required.")
  }

  if (missing(user)) {
    stop("The 'user' parameter is required.")
  }

  if (missing(password)) {
    stop("The 'password' parameter is required.")
  }

  # Construct the connection configuration as a named list (dictionary)
  connection_dict <- c(
    host = host,
    dbname = dbname,
    port = port,
    user = user,
    password = password
  )

  return(connection_dict)
}

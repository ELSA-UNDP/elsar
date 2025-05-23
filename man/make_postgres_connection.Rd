% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_postgres_connection.R
\name{make_postgres_connection}
\alias{make_postgres_connection}
\title{Create a PostgreSQL Connection Configuration as a Dictionary}
\usage{
make_postgres_connection(
  host = "localhost",
  dbname,
  port = 5432,
  user,
  password
)
}
\arguments{
\item{host}{The host of the database. Defaults to 'localhost' for a local PostgreSQL database, but can also accept an IP address (e.g., '192.168.1.1').}

\item{dbname}{The name of the PostgreSQL database. This parameter is required.}

\item{port}{The port on which the PostgreSQL server is running. Defaults to 5432.}

\item{user}{A valid database user name. This parameter is required.}

\item{password}{The password for the specified user. This parameter is required.}
}
\value{
A named list representing the PostgreSQL connection configuration.
}
\description{
This function generates a PostgreSQL connection configuration using a named list, which can be used directly for database connection functions.
}
\examples{
\dontrun{
# Example with explicit parameters
connection_dict <- make_postgres_connection(
  host = 'localhost',
  dbname = 'mydatabase',
  port = 5432,
  user = 'myuser',
  password = 'mypassword'
  )

# Use the connection dictionary in other functions
elsar_load_data(
  file_name = 'postgres',
  db_info = connection_dict
  )
}
}

#' Get all events records in BLOOM fleet register
#'
#' @return The function returns a dataframe containing all the events saved by BLOOM
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbGetQuery
#' @export


get_all_events <- function() {
    db <- Sys.getenv("POSTGRES_DB")
    if (db == '') {
      print("Variable POSTGRES_DB not found in .Renviron")
    }
    host <- Sys.getenv("POSTGRES_HOSTNAME")
    if (host == '') {
      print("Variable POSTGRES_HOSTNAME not found in .Renviron")
    }
    user <- Sys.getenv("POSTGRES_USER")
    if (user == '') {
      print("Variable POSTGRES_USER not found in .Renviron")
    }
    pwd <- Sys.getenv("POSTGRES_PASSWORD")
    port <- Sys.getenv("POSTGRES_PORT")
    if (port == '') {
      print("Variable POSTGRES_PORT not found in .Renviron")
    }

    conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
                           host = host,
                           port = port,
                           dbname = db,
                           user = user,
                           password = pwd)

    events <- dbGetQuery(conn,
                         "SELECT * FROM events")

    return(events)
}

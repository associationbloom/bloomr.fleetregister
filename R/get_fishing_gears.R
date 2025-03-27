#' Get MMSI information for all vessels
#'
#' @return The function returns a dataframe containing the MMSI with dates
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @export


get_fishing_gears <- function() {
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

  res = dbGetQuery(conn, "SELECT fg.cfr_id, fg.gear, fg.start_date, fg.end_date FROM fishing_gears AS fg
                          WHERE fg.order = 0
                          ORDER BY fg.start_date DESC")

  DBI::dbDisconnect(conn)

  return(res)
}

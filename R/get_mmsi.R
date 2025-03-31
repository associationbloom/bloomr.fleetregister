#' Get MMSI for all vessels
#'
#' @return The function returns a dataframe containing all the MMSI registered associated to CFR ids (found in CFR through `get_cfr`).
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @export

get_mmsi <- function() {
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

  res = dbGetQuery(conn, "SELECT cfr_id, mmsi, start_date, end_date FROM mmsi
                ORDER BY start_date DESC")

  DBI::dbDisconnect(conn)

  return(res)
}

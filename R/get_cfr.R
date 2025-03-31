#' Get CFR for all vessels
#'
#' @return The function returns a dataframe containing the CFR with ids.
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @export


get_cfr <- function() {
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

  res = dbGetQuery(conn, "SELECT id, cfr FROM cfr")

  DBI::dbDisconnect(conn)

  return(res)
}

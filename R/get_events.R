#' Get records in BLOOM fleet register
#'
#' @param country_iso3 A string ISO3 code only for European country. One of "BEL", "BGR", "HRV", "CYP", "DNK", "EST", "FIN", "FRA", DEU", "GRC", "GBR", "IRL", "ITA", "LVA", "LTU", "MLT", "NLD", "POL", "PRT", "ROU", "SVN", "ESP", "SWE"
#' @param start_date A string date in the following format: %Y-%m-%d
#' @param end_date A string date in the following format: %Y-%m-%d
#'
#' @return The function returns a dataframe containing all the events saved by BLOOM
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @export

get_events <- function(country_iso3 = NULL,
                       start_date = "1980-01-01",
                       end_date = Sys.Date()) {
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

    if (!is.null(country_iso3)) {
    events <- dbGetQuery(conn,
                         sprintf("SELECT * FROM events
                                 WHERE REGEXP_LIKE(country, '%s')
                                 AND event_start <= '%s'
                                 AND event_end >= '%s'",
                                 country_iso3, end_date, start_date))
    } else {
      events <- dbGetQuery(conn,
                           sprintf("SELECT * FROM events
                                 WHERE event_start <= '%s'
                                 AND event_end >= '%s'",
                                   end_date, start_date))
    }

    DBI::dbDisconnect(conn)
    return(events)
}

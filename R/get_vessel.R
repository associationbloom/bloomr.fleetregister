#' Get information associated to a vessel ID
#'
#' @param id A vessel identifier (CFR, MMSI, UVI, IRCS, external marking or registration number)
#' @param type identifier type, either "cfr", "mmsi", "uvi", "ircs", "external_marking" or "registration_number"
#'
#' @return The function returns a list containing dataframes with the historical information associated to a vessel ID
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbGetQuery
#' @import dplyr
#' @export

get_vessel <- function(id = 'FRA000669307', type = 'cfr') {
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

  if (type == 'cfr') {
    cfr_ids = dbGetQuery(conn,
                        sprintf("SELECT id FROM cfr
                                   WHERE cfr = '%s'", id))
  } else if (type == 'mmsi') {
    cfr_ids = dbGetQuery(conn,
                        sprintf("SELECT cfr.id FROM cfr
                        LEFT JOIN mmsi on cfr.id = mmsi.cfr_id
                        WHERE mmsi = %s", id))
  } else if (type == 'uvi') {
    cfr_ids = dbGetQuery(conn,
                         sprintf("SELECT cfr.id FROM cfr
                        LEFT JOIN uvi on cfr.id = uvi.cfr_id
                        WHERE uvi = '%s'", id))
  } else if (type == 'ircs') {
    cfr_ids = dbGetQuery(conn,
                         sprintf("SELECT cfr.id FROM cfr
                        LEFT JOIN ircs on cfr.id = ircs.cfr_id
                        WHERE ircs = '%s'", id))
  } else if (type == 'external_marking') {
    cfr_ids = dbGetQuery(conn,
                         sprintf("SELECT cfr.id FROM cfr
                        LEFT JOIN external_markings on cfr.id = external_markings.cfr_id
                        WHERE marking = '%s'", id))
  } else if (type == 'registration_number') {
    cfr_ids = dbGetQuery(conn,
                         sprintf("SELECT cfr.id FROM cfr
                        LEFT JOIN registration_numbers on cfr.id = registration_numbers.cfr_id
                        WHERE registration_number = '%s'", id))
  }

  res = list()

  for (cfr_id in cfr_ids$id) {

    res_cfr = list()

    # Identity

    cfr = dbGetQuery(conn,
                      sprintf("SELECT cfr FROM cfr
                              WHERE id = '%s'", cfr_id))

    res_cfr[['country']] =  dbGetQuery(conn,
                                       sprintf("SELECT country, start_date, end_date FROM countries
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = country)

    res_cfr[['vessel_name']] =  dbGetQuery(conn,
                                           sprintf("SELECT name, start_date, end_date FROM vessel_names
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = name)


    res_cfr[['mmsi']] = dbGetQuery(conn,
                        sprintf("SELECT mmsi, start_date, end_date FROM mmsi
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = mmsi)

    res_cfr[['uvi']] =  dbGetQuery(conn,
                      sprintf("SELECT uvi, start_date, end_date FROM uvi
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = uvi)

    res_cfr[['ircs']] =  dbGetQuery(conn,
                               sprintf("SELECT ircs, start_date, end_date FROM ircs
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = ircs)


    res_cfr[['external_marking']] =  dbGetQuery(conn,
                                   sprintf("SELECT marking, start_date, end_date FROM external_markings
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = marking)

    res_cfr[['registration_number']] =  dbGetQuery(conn,
                                       sprintf("SELECT registration_number, start_date, end_date FROM registration_numbers
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = registration_number)

    res_cfr[['registration_place']] =  dbGetQuery(conn,
                                               sprintf("SELECT registration_place, name, start_date, end_date FROM registration_places
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = registration_place)

    # Technical characteristics

    res_cfr[['vessel_type']] =  dbGetQuery(conn,
                                sprintf("SELECT type, start_date, end_date FROM vessel_types
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = type)

    res_cfr[['dimensions']] =  dbGetQuery(conn,
                                sprintf("SELECT length, type, start_date, end_date FROM dimensions
                                   WHERE cfr_id = '%s'
                                ORDER BY type DESC, start_date DESC", cfr_id)) %>% rename("value" = length)


    res_cfr[['tonnage']] =  dbGetQuery(conn,
                                      sprintf("SELECT tonnage, type, start_date, end_date FROM tonnages
                                   WHERE cfr_id = '%s'
                                ORDER BY type DESC, start_date DESC", cfr_id)) %>% rename("value" = tonnage)


    res_cfr[['engine_power']] =  dbGetQuery(conn,
                                      sprintf("SELECT power, type, start_date, end_date FROM engine_power
                                   WHERE cfr_id = '%s'
                                    ORDER BY CASE WHEN type = 'main' THEN 1
                                      ELSE 2 END,
                                      start_date DESC", cfr_id)) %>% rename("value" = power)

    res_cfr[['hull_material']] =  dbGetQuery(conn,
                                        sprintf("SELECT code, start_date, end_date FROM hull_material
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = code)

    # Fishing gear

    res_cfr[['fishing_gear']] =  dbGetQuery(conn,
                                            sprintf("SELECT fg.gear, fg.order, fg.start_date, fg.end_date FROM fishing_gears AS fg
                                   WHERE cfr_id = '%s'
                                ORDER BY fg.order ASC, start_date DESC", cfr_id)) %>% rename("value" = gear)


    res_cfr[['segment']] =  dbGetQuery(conn,
                                       sprintf("SELECT segment, start_date, end_date FROM segments
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = segment)

    # Administrative information

    res_cfr[['service_entry']] =  dbGetQuery(conn,
                                         sprintf("SELECT year, start_date, end_date FROM service_entries
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = year)

    res_cfr[['public_aid']] =  dbGetQuery(conn,
                                          sprintf("SELECT public_aid, start_date, end_date FROM public_aids
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = public_aid)

    res_cfr[['construction']] =  dbGetQuery(conn,
                                            sprintf("SELECT year, start_date, end_date FROM construction
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = year)

    res_cfr[['events']] =  dbGetQuery(conn,
                                            sprintf("SELECT event, start_date FROM sub_events
                                   WHERE cfr_id = '%s'
                                ORDER BY start_date DESC", cfr_id)) %>% rename("value" = event)

    # Import/export
    res_cfr[['import_export']] =  dbGetQuery(conn,
                                           sprintf("SELECT type, country, event_date FROM import_export
                                   WHERE cfr_id = '%s'
                                ORDER BY event_date DESC", cfr_id)) %>% rename("value" = country)


    res[[cfr[[1]]]] = res_cfr

  }

  return(res)
}

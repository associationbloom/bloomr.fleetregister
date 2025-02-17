#' Get fleet repartition
#' @description The classification is based solely on the main gear (order = 0).
#'
#' If a vessel shows multiple lengths (i.e. main gear) in the given period, the length (i.e. main gear) covering the longer period of time is used.
#'
#' The gear categorization follows the FAO ISSCFG standards (2016)
#' <https://openknowledge.fao.org/server/api/core/bitstreams/830259c5-cbba-49f8-ae0d-819cd54356d3/content>.
#'
#' @param country_iso3 A string ISO3 code. One of "BEL", "BGR", "HRV", "CYP", "DNK", "EST", "FIN", "FRA", DEU", "GRC", "GBR", "IRL", "ITA", "LVA", "LTU", "MLT", "NLD", "POL", "PRT", "ROU", "SVN", "ESP", "SWE"
#' @param start_date A string date in the following format: %Y-%m-%d
#' @param end_date A string date in the following format: %Y-%m-%d
#' @param size_breaks Vessel length breaks array (LOA)
#' @param categorized logical. If TRUE then gear are categorized (level 1 with subcategory when available) following the FAO ISSCFG standards (2016). If FALSE, level 2 is used.
#'
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_wider
#' @return Returns a size dataframe
#' @examples get_fleet_repartition('FRA', '2024-01-01', '2024-12-31', c(0, 12, 18, 24, 40, Inf))
#'
#' @export
#'
get_fleet_repartition <- function(country_iso3 = NULL,
                                  start_date = '2024-01-01',
                                  end_date = Sys.Date(),
                                  size_breaks = c(0, 12, 18, 24, 40, Inf),
                                  categorized = TRUE) {

  gear_codes <- readxl::read_excel(system.file("extdata", "MDR_Gear_Type.xls", package = "bloomr.fleetregister"), 1)


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
    cfr <- dbGetQuery(conn,
                         sprintf("SELECT * FROM countries AS c
                                 WHERE REGEXP_LIKE(country, '%s')
                                 AND start_date < '%s'
                                 AND end_date >= '%s'",
                                 country_iso3, end_date, start_date))
  } else {
    cfr <- dbGetQuery(conn,
                         sprintf("SELECT * FROM countries AS c
                        WHERE start_date < '%s'
                        AND end_date >= '%s'",
                        end_date, start_date))
  }

  str <- toString(cfr$cfr_id)
  str <- gsub(", ", "', '", str)
  str <- paste0("'", str, "'")

  size <- dbGetQuery(conn,
                       sprintf("SELECT cfr_id, length FROM (
                                  SELECT distinct cfr_id, length, days, ROW_NUMBER() OVER (PARTITION BY cfr_id) AS rn FROM (
                                    SELECT cfr_id, length, tmp_start_date, tmp_end_date, tmp_end_date - tmp_start_date AS days FROM (
                                      SELECT *,
                                      CASE WHEN start_date < '%s' THEN '%s'
                                      ELSE start_date
                                      END AS tmp_start_date,
                                      CASE WHEN end_date > '%s' THEN '%s'
                                      ELSE end_date
                                      END AS tmp_end_date
                                      FROM dimensions d
                                      WHERE d.start_date < '%s' and type = 'loa' AND  d.end_date >= '%s' AND d.length > 0 AND cfr_id IN (%s)) AS subq1
                                  ) AS subq2) AS query
                                WHERE rn = 1", start_date, start_date, end_date, end_date, end_date, start_date, str))


  fishing_gear =  dbGetQuery(conn,
                             sprintf("SELECT cfr_id, gear FROM (
                                  SELECT distinct cfr_id, gear, days, ROW_NUMBER() OVER (PARTITION BY cfr_id) AS rn FROM (
                                    SELECT cfr_id, gear, tmp_start_date, tmp_end_date, tmp_end_date - tmp_start_date AS days FROM (
                                      SELECT *,
                                      CASE WHEN start_date < '%s' THEN '%s'
                                      ELSE start_date
                                      END AS tmp_start_date,
                                      CASE WHEN end_date > '%s' THEN '%s'
                                      ELSE end_date
                                      END AS tmp_end_date
                                      FROM fishing_gears fg
                                      WHERE fg.start_date < '%s' AND fg.order = 0 AND fg.end_date >= '%s' AND cfr_id IN (%s)) AS subq1
                              ) AS subq2) AS query
                                WHERE rn = 1", start_date, start_date, end_date, end_date, end_date, start_date, str))

  DBI::dbDisconnect(conn)

  if (categorized) {
    gear_cleaned <- gear_codes %>%
      dplyr::mutate(category = dplyr::case_when(!is.na(SubCategory) ~ paste0(SubCategory),
                                         .default = stringr::str_to_title(Category))) %>%
      dplyr::select(Code, category)
  } else {
    gear_cleaned <- gear_codes %>%
      dplyr::select(Code, category = EnDescription)
  }


  df <- size %>%
    dplyr::full_join(fishing_gear, by = "cfr_id") %>%
    dplyr::mutate(size_class = cut(length,
                       breaks=size_breaks, include.lowest = T, right = F)) %>%
    dplyr::left_join(gear_cleaned, by = c("gear" = "Code")) %>%
    dplyr::group_by(category, size_class) %>%
    dplyr::summarise(n = dplyr::n())

  df <- df %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = size_class, values_from = n)

  ##TODO: intervertir colonnes class
  new_colnames <- gsub("\\[", "", colnames(df))
  new_colnames <- gsub("^0,", "< ", new_colnames)
  new_colnames <- gsub(",Inf", "", new_colnames)
  new_colnames <- gsub(",", "-", new_colnames)
  new_colnames <- gsub(")", " m", new_colnames)
  new_colnames <- gsub("]", " m", new_colnames)

  if (categorized) {
    new_colnames[1] <- "Gear category"
  } else {
    new_colnames[1] <- "Gear"
  }

  if (!stringr::str_detect(new_colnames[length(new_colnames)], "-")) {
    new_colnames[length(new_colnames)] <- paste0("≥ ", new_colnames[length(new_colnames)])
  }
  colnames(df) <- new_colnames

  if (categorized) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate("Total by gear category" = rowSums(dplyr::select(., -c(`Gear category`)), na.rm = TRUE))
  } else {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate("Total by gear" = rowSums(dplyr::select(., -c(Gear)), na.rm = TRUE))
  }

  totals <- NULL
  for (i_col in colnames(df)[2:ncol(df)]) {
    totals <- c(totals, as.integer(sum(df[i_col], na.rm = TRUE)))
  }

  names(totals) <- colnames(df)[2:ncol(df)]

  df <- df %>%
    dplyr::bind_rows(totals)

  df[nrow(df), 1] <- "Total by size class"

  return(df)
}


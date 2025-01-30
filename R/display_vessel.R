#' Display information associated to a vessel ID in pretty tables
#'
#' @param id A vessel identifier (CFR, MMSI, UVI, IRCS, external marking or registration number)
#' @param type identifier type (cfr, mmsi, uvi, ircs, external_marking, registration_number)
#' @importFrom kableExtra kbl kable_styling collapse_rows
#' @import dplyr
#' @import purrr
#' @importFrom stringr str_to_sentence
#' @importFrom readxl read_excel

#' @return The function prints tables, one per CFR associated to

#' @export

display_vessel <- function(id = 'FRA000669307', type = 'cfr') {

  hull_material_codes <- readxl::read_excel(system.file("extdata", "MDR_Vessel_Hull_Type.xls", package = "bloomr.fleetregister"), 1) %>% dplyr::mutate(Code = as.numeric(Code))
  segments_codes <- readxl::read_excel(system.file("extdata", "MDR_Vessel_Segment.xls", package = "bloomr.fleetregister"), 1)
  event_codes <- readxl::read_excel(system.file("extdata", "MDR_Vessel_Event.xls", package = "bloomr.fleetregister"), 1)
  export_type_codes <- readxl::read_excel(system.file("extdata", "MDR_Vessel_Export_Type.xls", package = "bloomr.fleetregister"), 1)
  public_aid_codes <- readxl::read_excel(system.file("extdata", "MDR_Vessel_Public_Aid_Type.xls", package = "bloomr.fleetregister"), 1)
  gear_codes <- readxl::read_excel(system.file("extdata", "MDR_Gear_Type.xls", package = "bloomr.fleetregister"), 1)
  vessel_type_codes <- readxl::read_excel(system.file("extdata", "MDR_Vessel_Type.xls", package = "bloomr.fleetregister"), 1)

  vessel_info <- get_vessel(id, type)

  cfr = names(vessel_info)

  index_cfr = 1
  for (info_list in vessel_info) {

    # Fishing gear code
    info_list[['fishing_gear']] <- info_list[['fishing_gear']] %>%
      dplyr::left_join(gear_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = value)) %>%
      dplyr::select(-EnDescription)

    # Vessel type code
    info_list[['vessel_type']] <- info_list[['vessel_type']] %>%
      dplyr::left_join(vessel_type_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = value)) %>%
      dplyr::select(-EnDescription)

    # Hull material code
    info_list[['hull_material']] <- info_list[['hull_material']] %>%
      dplyr::left_join(hull_material_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = as.character(value))) %>%
      dplyr::select(-EnDescription)

    # Segment code
    info_list[['segment']] <- info_list[['segment']] %>%
      dplyr::left_join(segments_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = value)) %>%
      dplyr::select(-EnDescription)

    # Event code
    info_list[['events']] <- info_list[['events']] %>%
      dplyr::left_join(event_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = value)) %>%
      dplyr::select(-EnDescription)

    # Public aid code
    info_list[['public_aid']] <- info_list[['public_aid']] %>%
      dplyr::left_join(public_aid_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = value)) %>%
      dplyr::select(-EnDescription)

    # Import / export code
    info_list[['import_export']] <- info_list[['import_export']] %>%
      dplyr::left_join(export_type_codes %>%
                         dplyr::select(Code, EnDescription), by = c("value" = "Code")) %>%
      dplyr::mutate(value = case_when(!is.na(EnDescription) ~ paste0(value, " - ", EnDescription),
                                      .default = value)) %>%
      dplyr::select(-EnDescription)


    empty_el <- sapply(info_list, function(x) dim(x)[1]) == 0
    names_empty <- names(empty_el[empty_el])

    for (name in names_empty) {
      info_list[[name]] <- info_list[[name]] %>% dplyr::add_row()
    }


    names_var <- names(info_list)

    names_var <- gsub('_', ' ', names_var)
    names_var <- stringr::str_to_sentence(names_var)
    names_var <- replace(names_var, names_var == "Mmsi", "MMSI")
    names_var <- replace(names_var, names_var == "Ircs", "IRCS")
    names_var <- replace(names_var, names_var == "Uvi", "UVI")


    df_repartition <- dplyr::tibble(names = names_var, i_start = NA, i_end = NA)
    j = 1
    index = 1
    for (el in info_list) {
      df_repartition$i_start[j] <- index
      df_repartition$i_end[j] <- index + nrow(el) - 1
      index = index + nrow(el)
      j = j +1
    }

    cfr_info_list <- purrr::imap(info_list, ~cbind(.x, variable = .y))

    cfr_info <- purrr::map_dfr(cfr_info_list, ~.x %>% dplyr::mutate(across(everything(), as.character)))

    cfr_info <- cfr_info %>%
      dplyr::select("Value" = value, "Type" = type, "Order" = order, "Name" = name, "Start date" = start_date, "End date" = end_date)

    cfr_info <- replace(cfr_info, is.na(cfr_info), "")
    cfr_info <- replace(cfr_info, cfr_info == '2100-12-31', "-")


    table <- cfr_info %>%
             kableExtra::kbl(caption = paste('CFR:', cfr[index_cfr])) %>%
             kableExtra::kable_styling(bootstrap_options = c("condensed", "responsive"), full_width = F)


    for(i in 1:nrow(df_repartition)) {
      row <- df_repartition[i,]
      table <- table %>%
        kableExtra::pack_rows(row$names, row$i_start, row$i_end)
    }

    print(table)

    index_cfr = index_cfr + 1
    }
}

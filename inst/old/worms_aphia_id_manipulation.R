library(worrms)
library(dplyr)
library(data.table)
table_2_1_linkage <- utils::read.csv(file = system.file("eumap_table_2_1_linkage_version_2022_v2.0.csv",
                                                        package = "rwptool"),
                                     sep = ';',
                                     header = TRUE,
                                     as.is = TRUE,
                                     encoding = 'UTF-8')
cat(format(x = Sys.time(),
           format = "%Y-%m-%d %H:%M:%S"),
    " - Warning, check the code below regarding non dynamic worms aphia id definition.\n",
    sep = "")
table_2_1_linkage <- dplyr::inner_join(x = table_2_1_linkage,
                                       y = tidyr::tibble("latin_name" = unique(x = table_2_1_linkage$latin_name)) %>%
                                         dplyr::rowwise() %>%
                                         dplyr::mutate(worms_aphia_id = as.character(x = tryCatch(worrms::wm_name2id(name = latin_name),
                                                                                                  error = function(a) {NA_character_}))),
                                       by = "latin_name") %>%
  dplyr::relocate(worms_aphia_id,
                  .after = x3a_code) %>%
  dplyr::mutate(worms_aphia_id = dplyr::case_when(
    latin_name == "Raja brachyura" ~ "105882",
    latin_name == "Plagioscion squamosissimus" ~ "990098",
    latin_name == "Pterois volitans" ~ "159559",
    latin_name == "Aphareus rutilans" ~ "218468",
    latin_name == "Illex illecebrosus" ~ "153087",
    latin_name == "Thunnus alalunga" ~ "127026",
    latin_name %in% c("Batoidimorpha (Hypotremata)",
                      "Selachimorpha (Pleurotremata)") ~ NA_character_,
    latin_name == "Dipturus batis, Dipturus intermedius" ~ "105869, 711846",
    latin_name == "Makaira nigricans, Makaira mazara" ~ "126950, 219729",
    TRUE ~ worms_aphia_id
  ))
if (nrow(x = unique(x = filter(.data = table_2_1_linkage,
                               is.na(worms_aphia_id) & (! latin_name %in% c("Batoidimorpha (Hypotremata)",
                                                                            "Selachimorpha (Pleurotremata)"))))) > 0) {
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Warning, at least one worms aphia id is NA. Start processing.\n",
      sep = "")
  asfis <- utils::read.table(system.file("asfis_sp_2022_rev1.txt",
                                         package = "rwptool"),
                             header = TRUE,
                             sep = ",",
                             as.is = TRUE)
  table_2_1_linkage_na_first_batch <- dplyr::filter(.data = table_2_1_linkage,
                                                    is.na(x = worms_aphia_id) & (! latin_name %in% c("Batoidimorpha (Hypotremata)",
                                                                                                     "Selachimorpha (Pleurotremata)"))) %>%
    dplyr::select(latin_name) %>%
    dplyr::distinct() %>%
    dplyr::left_join(asfis[, c("TAXOCODE",
                               "X3A_CODE",
                               "Scientific_Name")],
                     by = c("latin_name" = "Scientific_Name"))
  names(table_2_1_linkage_na_first_batch) <- tolower(x = names(table_2_1_linkage_na_first_batch))
  current_table_2_1_linkage_na_first_batch <- tidyr::tibble()
  for (current_taxocode in na.omit(table_2_1_linkage_na_first_batch$taxocode)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Start matching process on asfis taxocode ",
        current_taxocode,
        ".\n",
        sep = "")
    current_taxocode_global <- stringr::str_extract(string = current_taxocode,
                                                    pattern = "[:digit:]+")
    current_asfis <- dplyr::filter(.data = asfis,
                                   stringr::str_extract(string = TAXOCODE,
                                                        pattern = "[:digit:]+") %like% current_taxocode_global)
    current_asfis <- dplyr::left_join(x = current_asfis,
                                      y = suppressWarnings(expr = dplyr::tibble("Scientific_Name" = names(x = unlist(x = worrms::wm_name2id_(name = current_asfis$Scientific_Name))),
                                                                                "worms_aphia_id" = unlist(x = worrms::wm_name2id_(name = current_asfis$Scientific_Name)))),
                                      by = "Scientific_Name") %>%
      dplyr::relocate(worms_aphia_id,
                      .after = Scientific_Name)
    current_table_2_1_linkage_na_first_batch <- rbind(current_table_2_1_linkage_na_first_batch,
                                                      tidyr::tibble("scientific_name" = dplyr::filter(.data = current_asfis,
                                                                                                      TAXOCODE == current_taxocode)$Scientific_Name,
                                                                    "join_worms_aphia_id" = paste(na.omit(object = stringr::str_extract(string = current_asfis$TAXOCODE,
                                                                                                                                        pattern = "[:digit:]{12}")),
                                                                                                  collapse = ", "),
                                                                    "join_x3a_code" = paste(current_asfis$X3A_CODE,
                                                                                            collapse = ", "),
                                                                    "join_scientific_name" = paste(paste(dplyr::filter(.data = current_asfis,
                                                                                                                       TAXOCODE != current_taxocode)$Scientific_Name, 
                                                                                                         collapse = ", "),
                                                                                                   stringr::str_extract(string = dplyr::filter(.data = current_asfis,
                                                                                                                                               TAXOCODE == current_taxocode)$Scientific_Name,
                                                                                                                        pattern = "[:alpha:]+"),
                                                                                                   dplyr::filter(.data = current_asfis,
                                                                                                                 TAXOCODE == current_taxocode)$Scientific_Name,
                                                                                                   sep = ", ")))
  }
  table_2_1_linkage_na_first_batch <- dplyr::left_join(x = table_2_1_linkage_na_first_batch,
                                                       y = current_table_2_1_linkage_na_first_batch,
                                                       by = c("latin_name" = "scientific_name"))
  table_2_1_linkage_na_first_batch_final <- dplyr::filter(.data = table_2_1_linkage_na_first_batch,
                                                          ! is.na(join_worms_aphia_id))
  if (nrow(x = table_2_1_linkage_na_first_batch_final) > 0) {
    table_2_1_linkage <- left_join(x = table_2_1_linkage,
                                   y = dplyr::select(.data = table_2_1_linkage_na_first_batch_final,
                                                     - x3a_code,
                                                     -taxocode),
                                   by = "latin_name") %>%
      dplyr::mutate(latin_name = dplyr::case_when(
        ! is.na(join_scientific_name) ~ join_scientific_name,
        TRUE ~ latin_name
      ),
      x3a_code = dplyr::case_when(
        ! is.na(join_x3a_code) ~ join_x3a_code,
        TRUE ~ x3a_code
      ),
      worms_aphia_id = dplyr::case_when(
        ! is.na(join_worms_aphia_id) ~ join_worms_aphia_id,
        TRUE ~ as.character(x = worms_aphia_id)
      )) %>%
      dplyr::select(-join_worms_aphia_id,
                    -join_x3a_code,
                    -join_scientific_name)
  }
  table_2_1_linkage_na_second_batch <- dplyr::filter(.data = table_2_1_linkage,
                                                     is.na(x = worms_aphia_id) & (! latin_name %in% c("Batoidimorpha (Hypotremata)",
                                                                                                      "Selachimorpha (Pleurotremata)"))) %>%
    dplyr::select(latin_name,
                  x3a_code) %>%
    dplyr::distinct()
  if (nrow(x = table_2_1_linkage_na_second_batch) > 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Warning, at least one worms aphia id is again NA after the processing.\n",
        sep = "")
  }
}
write.csv2(x = table_2_1_linkage,
           file = "eumap_table_2_1_linkage_version_2022_v2.1.csv",
           row.names = FALSE)

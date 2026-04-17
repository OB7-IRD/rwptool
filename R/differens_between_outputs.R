#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
differens_between_outputs <- function(path = NULL,
                                      type = "control",
                                      output_version_old = NULL,
                                      output_version_new = NULL,
                                      rfmo = c("CCAMLR",
                                               "CECAF",
                                               "GFCM",
                                               "IATTC",
                                               "ICCAT",
                                               "ICES",
                                               "IOTC",
                                               "NAFO",
                                               "SEAFO",
                                               "SPRFMO",
                                               "WCPFC",
                                               "WECAFC")) {
  path <-
    "Q:/mynd/kibi/projects_wks_wgs_rcgs/ISSG_RWP/2024/table_2_1_development/personal/output/"

  output_version_new <- "20240517_094805"
  output_version_old <- "20240515_132424"
  rfmos <- c("ICES")
  type <- "control"

  # compare control output

  if (type == "control") {
    old <-
      read.csv(
        paste0(
          path,
          output_version_old,
          "_table_2_1_template_control_27_eu_countries.csv"
        ),
        sep = ";"
      )
    new <-
      read.csv(
        paste0(
          path,
          output_version_new,
          "_table_2_1_template_control_27_eu_countries.csv"
        ),
        sep = ";"
      )

    old <-
      dplyr::rename(old,
                    "tons_country_old" = "tons_country",
                    "tons_eu_old" = "tons_eu")
    new <-
      dplyr::rename(new,
                    "tons_country_new" = "tons_country",
                    "tons_eu_new" = "tons_eu")

    ## Check for duplicated records

    old_distinct <- dplyr::distinct(old, region, rfmo, spp, geo, area, year, data_source)
    old_distinct$id <- row.names(old_distinct)
    new_distinct <- dplyr::distinct(new, region, rfmo, spp, geo, area, year, data_source)
    new_distinct$id <- row.names(new_distinct)

    new <- dplyr::left_join(new, new_distinct)

    new_dup <- dplyr::summarise(dplyr::group_by(new, region, rfmo, spp, geo, area, year, data_source), no = length(id))
    old_dup <- dplyr::summarise(dplyr::group_by(old_distinct, region, rfmo, spp, geo, area, year, data_source), no = length(id))



    comb <- dplyr::full_join(old, new)

    comb_diff <-
      subset(comb,
             tons_country_old != tons_country_new )#| tons_eu_old != tons_eu_new)

    comb_diff <-
      subset(comb_diff,
             rfmo %in% rfmos)

    write.table(
      comb_diff,
      paste0(
        path,
        format(x = Sys.time(),
               format = "%Y%m%d_%H%M%S_"),
        "difference_between_control_files.csv"
      ),
      row.names = F,
      sep = ";"
    )



  }


  # compare table 2.1 input

  old <- read.csv(paste0(path, output_version_old, "_table_2_1_template_27_eu_countries.csv"), sep = ";")
  new <- read.csv(paste0(path, output_version_new, "_table_2_1_template_27_eu_countries.csv"), sep = ";")

  old <- dplyr::select(dplyr::rename(old, "landings_old" = "landings", "tac_old" = "tac", "share_landing_old" = "share_landing"),
                -source_national, -share_landing_old, -source_eu, -thresh, -reg_coord, -covered_length, -selected_bio, -comments)
  new <- dplyr::select(dplyr::rename(new, "landings_new" = "landings", "tac_new" = "tac", "share_landing_old" = "share_landing"),
                       -source_national, -share_landing_old, -source_eu, -thresh, -reg_coord, -covered_length, -selected_bio, -comments)

  comb <- dplyr::full_join(old, new)

  comb_diff <- subset(comb, landings_old != landings_new)

  return(comb_diff)




}

#' @name global_load_cl_landing_rdb_data
#' @title Load CL landing RDB data
#' @description Process for for load the data from CL landing RDB data file.
#' @param input_path_file_rcg_stats {\link[base]{character}} expected. Input rcg stats data file path (.csv format expected).
#' @return A tibble.
#' @importFrom readr read_csv
#' @importFrom dplyr group_by summarise rename
#' @importFrom tidyr spread
#' @export
global_load_cl_landing_rdb_data <- function(input_path_file_rcg_stats) {
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start process for load data from CL landings RDB data.\n",
      sep = "")
  # global arguments verifications ----
  Year <- NULL
  Species <- NULL
  Area <- NULL
  geo <- NULL
  OfficialLandingCatchWeight <- NULL
  FlagCountry <- NULL
  TLW <- NULL
  # process ----
  # Define EU countries
  EU27_2020 <- c("AUT",
                 "BEL",
                 "BGR",
                 "HRV",
                 "CYP",
                 "CZE",
                 "DNK",
                 "EST",
                 "FIN",
                 "FRA",
                 "DEU",
                 "GRC",
                 "HUN",
                 "IRL",
                 "ITA",
                 "LVA",
                 "LTU",
                 "LUX",
                 "MLT",
                 "NLD",
                 "POL",
                 "PRT",
                 "ROU",
                 "SVK",
                 "SVN",
                 "ESP",
                 "SWE")

  cl_landing_rdb_data <- withCallingHandlers(expr = readr::read_csv(paste0(input_path_file_rcg_stats),
                                                                    na = c("", "NA", "NULL")),

                                               warning = function(input_path_cl_landing_rdbes_data) {
                                                 stop("Troubles with the input file, check the format associated.\n")
                                               })
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Successful process for load data from CL landings RDB data.\n",
      sep = "")
  # transform RDB data into the same format as EUROSTAT
  #3 Add country codes from geo data file
  geo_data <- utils::read.table(file = system.file("geo.def",
                                                   package = "rwptool"),
                                header = TRUE,
                                sep = ";")

  cl_landing_rdb_data_1 <- dplyr::left_join(cl_landing_rdb_data, geo_data,
                                            by = c("FlagCountry" = "level_description"),
                                            keep = T)
  # Summarise and create a EU27_2020 line
  cl_landing_rdb_data_ctry <-
    dplyr::summarise(
      dplyr::group_by(cl_landing_rdb_data_1, Year, Species, Area, geo),
      TLW = sum(OfficialLandingCatchWeight / 1000, na.rm = T),
      .groups = "drop"
    )
  cl_landing_rdb_data_eu27 <-
    dplyr::summarise(
      dplyr::group_by(
        dplyr::filter(cl_landing_rdb_data_1, FlagCountry %in% !!EU27_2020) ,
        Year,
        Species,
        Area
      ),
      geo = "EU27_2020",
      TLW = sum(OfficialLandingCatchWeight / 1000, na.rm = T),
      .groups = "drop"
    )
  # Include code for EU28 + warning if UK countries are missing
  cl_landing_rdb_data_2 <- rbind(cl_landing_rdb_data_ctry, cl_landing_rdb_data_eu27)

  cl_landing_rdb_data_t <- tidyr::spread(cl_landing_rdb_data_2, key = Year, value = TLW, fill = 0) # To make sure the mean get right!
  cl_landing_rdb_data_t <- dplyr::rename(cl_landing_rdb_data_t, Scientific_Name  = Species)
  return(cl_landing_rdb_data_t)
}

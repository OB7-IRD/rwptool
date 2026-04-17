#' @name global_load_cl_landing_rdbes_data
#' @title Load CL landing RDB data
#' @description Process for for load the data from CL landing RDB data file.
#' @param input_path_file_rcg_stats {\link[base]{character}} expected. Input rcg stats data file path (.csv format expected).
#' @return A tibble.
#' @importFrom readr read_csv
#' @importFrom dplyr group_by summarise rename
#' @importFrom tidyr spread
#' @export
global_load_cl_landing_rdbes_data <- function(input_path_file_rdbes_stats) {
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


  cl_landing_rdbes_data <-
    data.table::fread(paste0(input_path_file_rdbes_stats, "CommercialLanding.csv"),
          sep = ",",
          quote = "") # If read.csv, then set quote="" otherwise it skips a lot of data without warning

  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Successful process for load data from CL landings RDBES data.\n",
      sep = "")

  # Only include BMS and LAN

  unique(cl_landing_rdbes_data$CLcatchCategory)

  cl_landing_rdbes_data <- subset(cl_landing_rdbes_data, CLcatchCategory %in% c("Lan", "BMS"))

  # transform RDBES data into the same format as EUROSTAT
  #3 Add country codes from geo data file
  geo_data <- utils::read.table(file = system.file("geo.def",
                                                   package = "rwptool"),
                                header = TRUE,
                                sep = ";")

  cl_landing_rdbes_data_1 <- dplyr::left_join(cl_landing_rdbes_data, geo_data,
                                            by = c("CLvesselFlagCountry" = "geo"),
                                            keep = T)

  # Add scientific name
  rdbes_spp <- icesVocab::getCodeList("SpecWoRMS")
  cl_landing_rdbes_data_2 <- left_join(cl_landing_rdbes_data_1, rdbes_spp, by = c("CLspeciesCode" = "Key"))
  cl_landing_rdbes_data_2$CLspeciesName <- cl_landing_rdbes_data_2$Description

  # Summarise and create a EU27_2020 line
  cl_landing_rdbes_data_ctry <-
    dplyr::summarise(
      dplyr::group_by(cl_landing_rdbes_data_2, CLyear, CLspeciesName, CLarea, geo),
      TLW = sum(CLofficialWeight / 1000, na.rm = T),
      .groups = "drop"
    )
  cl_landing_rdbes_data_eu27 <-
    dplyr::summarise(
      dplyr::group_by(
        dplyr::filter(cl_landing_rdbes_data_2, level_description %in% !!EU27_2020) ,
        CLyear, CLspeciesName, CLarea
      ),
      geo = "EU27_2020",
      TLW = sum(CLofficialWeight / 1000, na.rm = T),
      .groups = "drop"
    )
  # Include code for EU28 + warning if UK countries are missing
  cl_landing_rdbes_data_3 <- rbind(cl_landing_rdbes_data_ctry, cl_landing_rdbes_data_eu27)

  cl_landing_rdbes_data_t <- tidyr::spread(cl_landing_rdbes_data_3, key = CLyear, value = TLW, fill = 0) # To make sure the mean get right!
  cl_landing_rdbes_data_t <- dplyr::rename(cl_landing_rdbes_data_t, Scientific_Name  = CLspeciesName)
  return(cl_landing_rdbes_data_t)
}

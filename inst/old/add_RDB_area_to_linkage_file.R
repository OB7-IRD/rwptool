
#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples

library(dplyr)
library(stringr)
library(data.table)


path <- "./inst/"

linkage_file_name_in <- "eumap_table_2_1_linkage_version_2022_v2.3"
linkage_file_name_out <- "eumap_table_2_1_linkage_version_2022_v2.4"


# Get area file from the RDB and keep only relevant info

rdb_areas <- read.csv(paste0(path, "Area.csv"), sep = ";")
names(rdb_areas)

unique(rdb_areas$X_AreaTypeCode)

rdb_areas <- subset(rdb_areas, X_AreaTypeCode != "Area" | Code == "27.5") # Area 27.5 has wrongly been coded as a area

unique(rdb_areas$Code)

# Create a list of areas
rdb_areas <- unique(str_subset(rdb_areas$Code, "[:digit:]"))

# Load linkage file

linkage <- read.csv(paste0(path, linkage_file_name_in, ".csv"), sep = ";", header = T)

names(linkage)

# Removing blanks from areaBis

unique(linkage$area_bis)

linkage$area_bis <- gsub(" ", "", linkage$area_bis)


# Coding RDB areas ----
## Start with a copy of the EUROSTAT areas - to stop the loop
linkage$area_rdb <- paste0(linkage$area_bis, ",")
unique(linkage$area_rdb)

# linkage$area_rdb <- paste0(",", gsub('_', '.', linkage$area_rdb), ",") # , add to make the gsub a bit easier


# Replace areas on upper level with all underlying areas


for (i in rdb_areas) {


  j <- paste0(toupper(gsub("\\.", "\\_", i)), ",")

  k <- paste0(i, ".")

  k <- gsub("\\.", "\\\\.", k)


  linkage$area_rdb <- gsub(j, gsub(" ", "", paste(i, paste(str_subset(rdb_areas, k), collapse = ","), sep = ",")), linkage$area_rdb)

}

test <- distinct(linkage, area_bis, area_rdb)

# Remove trailing comma

linkage$area_rdb <- gsub(",$", "", linkage$area_rdb)


write.table(linkage, paste0(path, linkage_file_name_out, ".csv"), sep = ";")


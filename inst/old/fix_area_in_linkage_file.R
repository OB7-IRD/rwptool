


library(dplyr)
library(stringr)

path <- "./inst/"
linkage_file_name_in <- "eumap_table_2_1_linkage_version_2022_v2.2"
linkage_file_name_out <- "eumap_table_2_1_linkage_version_2022_v2.3"


# Load linkage file

linkage <- read.csv(paste0(path, linkage_file_name_in, ".csv"), sep = ";", header = T)

names(linkage)

## Re-code

# Cod areas

cod <- subset(linkage, latin_name == "Gadus morhua")

linkage$area_bis[linkage$latin_name == "Gadus morhua" & linkage$area_bis == "27_3_A-4_AB_NK"] <- "27_3_A_20"
linkage$area_bis[linkage$latin_name == "Gadus morhua" & linkage$area_bis == "27_3_A"] <- "27_3_A_21"


write.table(linkage, paste0(path, linkage_file_name_out, ".csv"), sep = ";")

# ---
# title: "maintain_eumap_table_2_1_linkage_2024"
# author: "Kirsten Birch Håkansson, DTU Aqua"
# date: "`r Sys.Date()`"
# output: html_document
# ---

# Why ----

# It takes a long time to get classification info from the worrms package, so to speed up the maintainace this is moved to a seperate file

# Setup ----

library(stringr)
library(dplyr)
library(icesVocab)
library(worrms)

knitr::opts_chunk$set(echo = F)

path <- "./inst/"

linkage_file_name <- "eumap_table_2_1_linkage_version_2024_v1.2"

# Load data ----

linkage <- read.csv(paste0(path, linkage_file_name, ".csv"), sep = ";", header = T)

SpecWoRMS <- icesVocab::getCodeList("SpecWoRMS")

# Assign classification to RDBES SpecWoRMS ----

SpecWoRMS_aphiaIds <- unique(SpecWoRMS[!is.na(SpecWoRMS$Key), "Key"])

SpecWoRMS_classi <- worrms::wm_record_(SpecWoRMS_aphiaIds)

SpecWoRMS_classi_1 <-
  data.frame(do.call(rbind,SpecWoRMS_classi))

saveRDS(SpecWoRMS_classi_1, paste0(path, "SpecWoRMS_with_classifcation.rds"))



# Assign taxonomical info to RDBES SpecWoRMS ----
SpecWoRMS_names <- unique(SpecWoRMS[!is.na(SpecWoRMS$Description), "Description"])


SpecWoRMS_tax <- worrms::wm_records_names(name = SpecWoRMS_names[c(1:200)])
SpecWoRMS_tax_2 <- data.frame(do.call(rbind,SpecWoRMS_classi))

# Assign classification to linkage ----

linkage_aphiaIds_0 <- unique(paste(linkage$worms_aphia_id, collapse = ","))
linkage_aphiaIds <- data.frame(x = str_split(linkage_aphiaIds_0, ","))
names(linkage_aphiaIds) <- "x"

linkage_classi <- worrms::wm_record_(as.numeric(linkage_aphiaIds$x))
linkage_classi_1 <- data.frame(do.call(rbind,linkage_classi))









fak <- as.factor(linkage$region):as.factor(linkage$rfmo)

linkage_2 <- c()

for (i in levels(fak)) {
  linkage_sub <- linkage[fak == i,]

  raj_worms <- subset(classi_1, Family == "Rajidae")[, "id"]
  raj_latin <-
    subset(rdbes_spp, Key %in% raj_worms$id)[, "Description"]

  worms_aphia_id_raj_there <-
    distinct(
      subset(
        linkage_sub,
        latin_name != "Rajidae" &
          worms_aphia_id %in% raj_worms$id
      ),
      worms_aphia_id
    )
  latin_name_join_raj_there <-
    distinct(
      subset(
        linkage_sub,
        latin_name != "Rajidae" &
          worms_aphia_id %in% raj_worms$id
      ),
      latin_name_join
    )

  raj_worms_2 <-
    subset(raj_worms,
           !(id %in% worms_aphia_id_raj_there$worms_aphia_id) &
             !(id %in% c("105869", "711846")))
  raj_latin_2 <-
    subset(
      raj_latin,
      !(raj_latin %in% latin_name_join_raj_there$latin_name_join) &
        !(raj_latin %in% c(
          "Dipturus batis", "Dipturus intermedius"
        ))
    )

  linkage_sub$worms_aphia_id[linkage_sub$latin_name == "Rajidae"] <-
    paste(raj_worms_2$id, collapse = ",")
  linkage_sub$latin_name_join[linkage_sub$latin_name == "Rajidae"] <-
    paste(raj_latin_2, collapse = ",")

  linkage_2 <- rbind(linkage_2, linkage_sub)

}

```


```{r export}

write.table(linkage_2, paste0(path, linkage_file_name_out, ".csv"), sep = ";")

```



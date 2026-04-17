


library(dplyr)
library(stringr)

path <- "./inst/"
linkage_file_name_in <- "eumap_table_2_1_linkage_version_2022_v2.1"
linkage_file_name_out <- "eumap_table_2_1_linkage_version_2022_v2.2"


# Load linkage file

linkage <- read.csv(paste0(path, linkage_file_name_in, ".csv"), sep = ";", header = T)

names(linkage)

# Fix latin_name so it match table 1 - sometimes this is a list of species, so create a latin_name_join

linkage$latin_name_join <- linkage$latin_name

latin_name_with_comma <- subset(linkage, str_detect(latin_name, ","))

## Re-code
### North Sea and Eastern Arctic

linkage$latin_name[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$latin_name_join == "Mustelus antarcticus, Mustelus californicus, Mustelus canis, Mustelus dorsalis, Mustelus fasciatus, Mustelus griseus, Mustelus henlei, Mustelus higmani, Mustelus lenticulatus, Mustelus lunulatus, Mustelus manazo, Mustelus schmitti, Mustelus mustelus, Mustelus norrisi, Mustelus asterias, Mustelus mento, Mustelus mosis, Mustelus palumbes, Mustelus punctulatus, Mustelus whitneyi, Mustelus, Mustelus spp" &
                     linkage$spp_name == "Smooth-hound" & linkage$area == "1, 2, 14"] <- "Mustelus spp."


linkage$latin_name[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$latin_name_join == "Anarhichas lupus, Anarhichas denticulatus, Anarhichas minor, Anarhichas, Anarhichas spp" &
                     linkage$spp_name == "Catfish" & linkage$area == "4"] <- "Anarhichas spp."


linkage$latin_name[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$latin_name_join == "Argentina elongata, Argentina kagoshimae, Argentina silus, Argentina sphyraena, Argentina, Argentina spp" &
                     linkage$spp_name == "Argentine" & linkage$area == "4"] <- "Argentina spp."

linkage$latin_name[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$latin_name_join == "Lepidorhombus whiffiagonis, Lepidorhombus boscii, Lepidorhombus, Lepidorhombus spp" &
                     linkage$spp_name == "Megrim" & linkage$area == "4, 7d"] <- "Lepidorhombus spp."

linkage$latin_name[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$spp_name == "Anglerfish" & linkage$area == "3a, 4, 7d" &
                     linkage$latin_name_join == "Lophius piscatorius, Lophius budegassa, Lophius americanus, Lophius vaillanti, Lophius vomerinus, Lophius gastrophysus, Lophius litulon, Lophius, Lophius spp"] <- "Lophius spp."


linkage$latin_name[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$latin_name_join == "Mustelus antarcticus, Mustelus californicus, Mustelus canis, Mustelus dorsalis, Mustelus fasciatus, Mustelus griseus, Mustelus henlei, Mustelus higmani, Mustelus lenticulatus, Mustelus lunulatus, Mustelus manazo, Mustelus schmitti, Mustelus mustelus, Mustelus norrisi, Mustelus asterias, Mustelus mento, Mustelus mosis, Mustelus palumbes, Mustelus punctulatus, Mustelus whitneyi, Mustelus, Mustelus spp" &
                     linkage$spp_name == "Smooth-hound" & linkage$area == "3a, 4, 7d"] <- "Mustelus spp."

latin_name_with_comma <- subset(linkage, str_detect(latin_name, ","))

### North-East Atlantic

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Apristurus brunneus, Apristurus canutus, Apristurus gibbosus, Apristurus herklotsi, Apristurus indicus, Apristurus investigatoris, Apristurus japonicus, Apristurus kampae, Apristurus laurussonii, Apristurus longicephalus, Apristurus macrorhynchus, Apristurus macrostomus, Apristurus manis, Apristurus microps, Apristurus micropterygeus, Apristurus nasutus, Apristurus parvipinnis, Apristurus platyrhynchus, Apristurus profundorum, Apristurus riveri, Apristurus saldanha, Apristurus sibogae, Apristurus sinensis, Apristurus spongiceps, Apristurus stenseni, Apristurus, Apristurus spp" &
                     linkage$spp_name == "Deep water catsharks" & linkage$area == "5, 6, 7, 8, 9, 10"] <- "Apristurus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Beryx decadactylus, Beryx splendens, Beryx, Beryx spp" &
                     linkage$spp_name == "Alfonsinos" & linkage$area == "mars-14"] <- "Beryx spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Centrophorus atromarginatus, Centrophorus granulosus, Centrophorus harrissoni, Centrophorus isodon, Centrophorus lusitanicus, Centrophorus moluccensis, Centrophorus squamosus, Centrophorus tessellatus, Centrophorus uyato, Centrophorus, Centrophorus spp" &
                     linkage$spp_name == "Gulper shark" & linkage$area == "5, 6, 7, 8, 9, 10"] <- "Centrophorus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Pandalus hypsinotus, Pandalus platyceros, Pandalus borealis, Pandalus jordani, Pandalus montagui, Pandalus danae, Pandalus goniurus, Pandalus kessleri, Pandalus nipponensis, Pandalus amplus, Pandalus, Pandalus spp" &
                     linkage$spp_name == "Pandalid shrimps" & linkage$area == "5, 14"] <- "Pandalus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Trisopterus esmarkii, Trisopterus minutus, Trisopterus luscus, Trisopterus, Trisopterus spp" &
                     linkage$spp_name == "Pouting" & linkage$area == "All areas"] <- "Trisopterus spp."


linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Mustelus antarcticus, Mustelus californicus, Mustelus canis, Mustelus dorsalis, Mustelus fasciatus, Mustelus griseus, Mustelus henlei, Mustelus higmani, Mustelus lenticulatus, Mustelus lunulatus, Mustelus manazo, Mustelus schmitti, Mustelus mustelus, Mustelus norrisi, Mustelus asterias, Mustelus mento, Mustelus mosis, Mustelus palumbes, Mustelus punctulatus, Mustelus whitneyi, Mustelus, Mustelus spp" &
                     linkage$spp_name == "Smooth-hound" & linkage$area == "5-10, 12, 14"] <- "Mustelus spp."

## Blue skate is ok with comma

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Lepidorhombus whiffiagonis, Lepidorhombus boscii, Lepidorhombus, Lepidorhombus spp" &
                     linkage$spp_name == "Four-spot megrim" & linkage$area == "8c, 9a"] <- "Lepidorhombus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Lepidorhombus whiffiagonis, Lepidorhombus boscii, Lepidorhombus, Lepidorhombus spp" &
                     linkage$spp_name == "Four-spot megrim" & linkage$area == "8c, 9a"] <- "Lepidorhombus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Lepidorhombus whiffiagonis, Lepidorhombus boscii, Lepidorhombus, Lepidorhombus spp" &
                     linkage$spp_name == "Megrim" & linkage$area == "7, 8abd"] <- "Lepidorhombus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$latin_name_join == "Lepidorhombus whiffiagonis, Lepidorhombus boscii, Lepidorhombus, Lepidorhombus spp" &
                     linkage$spp_name == "Megrim" & linkage$area == "5b,6,12,14"] <- "Lepidorhombus spp."

linkage$latin_name[linkage$region == "North-East Atlantic" &
                     linkage$spp_name == "Anglerfish" &
                     linkage$latin_name_join == "Lophius piscatorius, Lophius budegassa, Lophius americanus, Lophius vaillanti, Lophius vomerinus, Lophius gastrophysus, Lophius litulon, Lophius, Lophius spp"] <- "Lophius spp."


latin_name_with_comma <- subset(linkage, str_detect(latin_name, ","))

# Output ----

write.table(linkage, paste0(path, linkage_file_name_out, ".csv"), sep = ";")

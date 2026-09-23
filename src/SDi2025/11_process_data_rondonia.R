# title: SoilData Integration
# subtitle: Process data from Rondônia
# author: Alessandro Samuel-Rosa
# date: 2026
# licence: MIT
# description: This script processes soil data from the Socioeconomic-Ecological
# State Zoning of Rondônia. It downloads and merges event and layer data from
# datasets ctb0033 and ctb0034 in the FEBR repository. It also uses dataset
# ctb0032 to obtain soil classification information. The script standardizes
# column names and measurement units and manually corrects the coordinates of
# two mislocated events. It handles duplicated layers (extra samples for
# fertility assessment) by creating new event identifiers and jittering their
# coordinates. Finally, it removes existing data from Rondônia in the main
# dataset and merges the newly processed data, saving the result.
rm(list = ls())

# Load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library("openxlsx")
}
if (!require("sf")) {
  install.packages("sf")
  library("sf")
}
if (!require("febr")) {
  if (!require(remotes)) {
    install.packages(pkgs = "remotes")
  }
  remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
}

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Zoneamento Socioeconômico-Ecológico do Estado de Rondônia (ctb0033 and 
# ctb0034)
# Download current version from FEBR: events
# ctb0032
event32 <- febr::observation("ctb0032", "all")
event32 <- data.table::as.data.table(event32)
event32 <- event32[, .(evento_id_febr, LOCALSERIE)]
# drop all columns, except evento_id_febr and LOCALSERIE
# Read file with soil classification codes and names
# Read "~/ownCloud/febr-repo/processamento/ctb0032/base/LOCALSERIE.xlsx"
LOCALSERIE <- read.xlsx(
  "~/ownCloud/febr-repo/processamento/ctb0032/base/LOCALSERIE.xlsx",
  sheet = 1, colNames = TRUE
)
LOCALSERIE <- data.table::as.data.table(LOCALSERIE)
# LOCALSERIE_C_3: soil code (LOCALSERIE)
# LOCSERIESD_C_70: soil name
# Match LOCALSERIE in event32 with LOCALSERIE to get soil names
event32 <- merge(
  event32, LOCALSERIE,
  by.x = "LOCALSERIE", by.y = "LOCALSERIE_C_3", all.x = TRUE
)
# Rename LOCSERIESD_C_70 to taxon_sibcs
event32[, taxon_sibcs := LOCSERIESD_C_70]
event32[, LOCALSERIE := NULL]
event32[, LOCSERIESD_C_70 := NULL]
# For some codes, there is no matching soil classification. So we check the
# source documentation to fill in the missing soil classification.
event32[is.na(taxon_sibcs), sort(evento_id_febr)]
# 312 events with missing soil classification.
# We create a list of soil codes and their corresponding names based on the
# source documentation.
taxon <- list(
  RO1020 = "Latossolo Amarelo distrófico",
  RO1030 = "Solos Glei distróficos",
  RO1049 = "Solos Glei distróficos",
  RO1057 = "Solos Glei distróficos",
  RO1071 = "Solos Aluviais distróficos",
  RO1112 = "Solos Aluviais distróficos",
  RO1144 = "Solos Litólicos distróficos",
  RO1147 = "Podzólico Vermelho-Amarelo tb distrófico A moderado",
  RO1148 = "Solos Litólicos distróficos",
  RO1149 = "Podzólico Vermelho-Amarelo tb eutrófico A fraco",
  RO1175 = "Podzólico Vermelho-Amarelo tb distrófico A moderado",
  RO1188 = "Podzólico Vermelho-Amarelo tb eutrófico A moderado",
  RO1198 = "Latossolo Vermelho-Amarelo eutrófico A moderado",
  RO1219 = "Podzólico Vermelho-Escuro tb eutrófico A moderado",
  RO1223 = "Solos Glei distróficos",
  RO1235 = "Solos Glei distróficos",
  RO1242 = "Solos Glei Húmicos",
  RO1251 = "Solos Glei Húmicos",
  RO1253 = "Solos Glei distróficos",
  RO1272 = "Solos Glei eutróficos",
  RO1279 = "Solos Glei distróficos",
  RO1280 = "Areias Quartzosas distróficas",
  RO1293 = "Solos Glei distróficos",
  RO1307 = "Podzólico Vermelho-Amarelo tb distrófico A moderado",
  RO1312 = "Cambissolo tb distrófico A proeminente",
  RO1319 = "Latossolo Vermelho-Amarelo distrófico A moderado",
  RO1322 = "Latossolo Vermelho-Amarelo distrófico A moderado",
  RO1328 = "Podzólico Amarelo distrófico",
  RO1334 = "Latossolo Vermelho-Amarelo eutrófico A proeminente",
  RO1351 = "Podzólico Vermelho-Amarelo tb distrófico A moderado",
  RO1352 = "Podzólico Vermelho-Amarelo tb eutrófico A moderado",
  RO1369 = "Cambissolo distrófico",
  RO1372 = "Cambissolo tb distrófico A proeminente",
  RO1375 = "Cambissolo eutrófico",
  RO1396 = "Cambissolo tm eutrófico A moderado",
  RO1510 = "Solos Aluviais distróficos",
  RO1513 = "Solos Aluviais distróficos",
  RO1522 = "Podzólico Vermelho-Escuro tb eutrófico A proeminente",
  RO1525 = "Solos Litólicos eutróficos",
  RO1532 = "Cambissolo ta eutrófico A fraco",
  RO1545 = "Solos Aluviais distróficos",
  RO1546 = "Cambissolo tb distrófico A proeminente",
  RO1560 = "Latossolo Amarelo distrófico",
  RO1568 = "Areias Quartzosas distróficas",
  RO1583 = "Latossolo Vermelho-Amarelo distrófico",
  RO1592 = "Areias Quartzosas distróficas",
  RO1611 = "Cambissolo eutrófico",
  RO1616 = "Brunizem",
  RO1622 = "Solos Litólicos eutróficos"
)
# Apply the taxon list to fill in missing soil classification
for (id in names(taxon)) {
  event32[evento_id_febr == id, taxon_sibcs := taxon[[id]]]
}
event32[is.na(taxon_sibcs), .N]
# 263 still are missing soil classification. We will leave them as NA for now.
# ctb0033
event33 <- febr::observation("ctb0033", "all")
event33 <- data.table::as.data.table(event33)
event33[, data_coleta := as.Date(data_coleta, origin = "1899-12-30")]
# ctb0034
event34 <- febr::observation("ctb0034", "all")
event34 <- data.table::as.data.table(event34)
event34[, dataset_id34 := dataset_id]
event34[, dataset_id := NULL]
event34[, data_coleta := as.Date(data_coleta, origin = "1899-12-30")]
# event34[, data_coleta := NULL]
sapply(list(event33 = event33, event34 = event34), nrow)
# 2998 and 107 events
eventRO <- merge(event33, event34, all = TRUE)
eventRO <- merge(eventRO, event32, by = "evento_id_febr", all.x = TRUE)
nrow(eventRO)
# 2999 events after merge

# Standardize column names and data types
eventRO[, dataset_id := "ctb0033"]
eventRO[, coord_datum_epsg := NULL]
eventRO[, coord_datum := "EPSG:4326"]
new_names <- c(
  evento_id_febr = "observacao_id",
  coord_longitude = "coord_x",
  coord_latitude = "coord_y",
  coord_municipio_nome = "municipio_id",
  coord_estado_sigla = "estado_id",
  coord_pais_id = "pais_id"
)
data.table::setnames(
  eventRO,
  old = names(new_names), new = new_names, skip_absent = TRUE
)
eventRO[, estado_id := "RO"]
eventRO[, pais_id := "BR"]
cols <- intersect(names(eventRO), tolower(names(eventRO)))
eventRO <- eventRO[, ..cols]
rm(event32, event33, event34, LOCALSERIE, taxon, new_names)

# Standardize the sampling date
eventRO[, data_ano := as.integer(format(data_coleta, "%Y"))]
nrow(eventRO[is.na(data_ano), ])
# 87 events missing the sampling date. These are not soil profiles and thus are
# not listed in the ANEX F of the source documentation. They may be extra
# samples collected for soil fertility assessment, and the sampling date was not 
# recorded. We will set the sampling date to 1996 for these events.
# Set the sampling date to 1996 for events with missing data
target_year <- 1996
eventRO[, data_ano_fonte := NA_character_]
eventRO[!is.na(data_ano), data_ano_fonte := "original"]
eventRO[is.na(data_ano), data_ano := target_year]
eventRO[is.na(data_ano_fonte), data_ano_fonte := "estimativa"]
eventRO[, .N, by = data_ano_fonte]
#    data_ano_fonte     N
#            <char> <int>
# 1:       original  2912
# 2:     estimativa    87
rm(target_year)

# Check spatial distribution of events in Rondônia
if (FALSE) {
  x11()
  plot(eventRO[, c("coord_x", "coord_y")])
}
str(eventRO)

# Attribute new coordinates to events falling in water bodies or outside the
# state of Rondônia
# Create a column named observacao_cura to store information about the
# correction (in Portuguese), as well as a copy of the original coordinates, the
# data of the collection, and the accronym of the author (ASR).

# RO2656
# By consulting the original coordinates of point RO2656, recorded in SoilData,
# and visualizing them on Google Maps, we verified that the point indeed falls
# within a watercourse on the border between Brazil and Bolivia. The study of 
# the work's documentation revealed that there may be a positional error of
# approximately 100 m. According to the textual description of the location, the
# soil profile was collected at the "Beira Rio Guapore". New coordinates,
# collected on Google Maps, will be manually assigned to the point.
# More information about the location can be found at:
# https://github.com/Laboratorio-de-Pedometria/mapbiomas-soil-train-prep/issues/5
# RO2656: -61.306907, -13.485739
id <- "RO2656"
eventRO[observacao_id == id, coord_x := -61.306907]
eventRO[observacao_id == id, coord_y := -13.485739]
eventRO[observacao_id == id, coord_fonte := "Google Maps (curadoria)"]
# Add 100 m to coord_precisao
eventRO[observacao_id == id, coord_precisao := coord_precisao + 100]

# RO2953: -9.765833 -65.73528 (original)
# The sample location is in Bolivia, near the Brazilian border.
# It is possible that the authors collected the soil samples in Bolivian
# territory, possibly for easier access. The original coordinates for point 
# RO2953 are in Bolivian territory, close to the border with Brazil.
# Documentation review suggests a positional error of approximately 100 m.
# New coordinates, obtained from Google Maps, have been manually assigned to
# this point. There is no additional information in the dataset to confirm this
# hypothesis. The coordinates were changed to a location in Rondônia, Brazil.
# -9.764905, -65.735686
# google_maps(eventRO[observacao_id == "RO2953", ])
id <- "RO2953"
eventRO[observacao_id == id, coord_x := -65.735686]
eventRO[observacao_id == id, coord_y := -9.764905]
eventRO[observacao_id == id, coord_fonte := "Google Maps (curadoria)"]
# Add 100 m to coord_precisao
eventRO[observacao_id == id, coord_precisao := coord_precisao + 100]
rm(id)

# Download current version from FEBR: layers ###################################
# ctb0033
# This dataset contains data on chemical soil properties measured over layers
# that correspond to the layers sampled for chemical analysis. The depth
# intervals of these layers are not necessarily the same as those of the layers
# sampled for morphological description.
layer33 <- febr::layer("ctb0033", "all")
layer33 <- data.table::as.data.table(layer33)
layer33[, camada_id_sisb := NULL]

# Create a new column named thickness to store the thickness of each layer,
# calculated as profund_inf - profund_sup. If the thickness is negative, it 
# indicates an error in the depth intervals.
layer33[, thickness := profund_inf - profund_sup]
layer33[thickness < 0, .N]
# There are 4 layers with negative thickness.
layer33[
  thickness < 0,
  .(evento_id_febr, camada_id_febr, profund_sup, profund_inf, thickness)
]

# RO1154: C 80-70 cm. The error comes from the source. We reverse the depth
# intervals to 70-80 cm.
layer33[
  evento_id_febr == "RO1154" & camada_id_febr == "C",
  profund_sup := ifelse(profund_sup == 80, 70, profund_sup)
]
layer33[
  evento_id_febr == "RO1154" & camada_id_febr == "C",
  profund_inf := ifelse(profund_inf == 70, 80, profund_inf)
]

# RO2463: C 80-70 cm. The error comes from the source. We reverse the depth 
# intervals to 70-80 cm.
layer33[
  evento_id_febr == "RO2463" & camada_id_febr == "C",
  profund_sup := ifelse(profund_sup == 80, 70, profund_sup)
]
layer33[
  evento_id_febr == "RO2463" & camada_id_febr == "C",
  profund_inf := ifelse(profund_inf == 70, 80, profund_inf)
]

# RO2826: E 140-60 cm. The error comes from the source. The correct depth
# intervals most likely are 140-160 cm. We correct the depth intervals to
# 140-160 cm.
layer33[
  evento_id_febr == "RO2826" & camada_id_febr == "E",
  profund_inf := ifelse(profund_inf == 60, 160, profund_inf)
]

# RO3542: D 110-80 cm. The error comes from the source. The correct depth 
# intervals most likely are 110-120 cm. We correct the depth intervals to 
# 110-120 cm.
layer33[
  evento_id_febr == "RO3542" & camada_id_febr == "D",
  profund_inf := ifelse(profund_inf == 80, 120, profund_inf)
]
layer33[, thickness := NULL]

# Check if there are layers where camada_id_febr == A and profund_sup > 5. In
# principle, the upper limit of the first layer (A) should be 0 cm (or near the
# surface).
cols <- c("evento_id_febr", "camada_id_febr", "profund_sup", "profund_inf")
layer33[camada_id_febr == "A" & profund_sup > 5, ..cols]
#     evento_id_febr camada_id_febr profund_sup profund_inf
#             <char>         <char>       <int>       <int>
#  1:         RO1006              A         100         110 Error
#  2:         RO1083              A          10          20
#  3:         RO1106              A          10          20
#  4:         RO1126              A          20          30
#  5:         RO1311              A           6          10
#  6:         RO1312              A           8          12
#  7:         RO1315              A           6          12
#  8:         RO1423              A          10          20
#  9:         RO1518              A          10          20
# 10:         RO1612              A          10          20
# 11:         RO1626              A          10          20
# 12:         RO1687              A          10          20
# 13:         RO2509              A          30          40 Error
# 14:         RO2607              A          10          30
# 15:         RO3213              A         110         120 Error

# RO1006 is likely wrong and the analytical data confirms it. We correct to
# 0-10 cm.
layer33[
  evento_id_febr == "RO1006" & camada_id_febr == "A",
  profund_sup := ifelse(profund_sup == 100, 0, profund_sup)
]
layer33[
  evento_id_febr == "RO1006" & camada_id_febr == "A",
  profund_inf := ifelse(profund_inf == 110, 10, profund_inf)
]

# RO2509 has A: 30-40 and B: 0-10. The limits are probably reversed. We correct
# to A: 0-10 and B: 30-40.
layer33[
  evento_id_febr == "RO2509" & camada_id_febr == "A",
  profund_sup := ifelse(profund_sup == 30, 0, profund_sup)
]
layer33[
  evento_id_febr == "RO2509" & camada_id_febr == "A",
  profund_inf := ifelse(profund_inf == 40, 10, profund_inf)
]

# RO3213 has layers A: 110-120, B: 0- 15, C: 30-40, and D: 110-120 cm. The
# analytical data for layer A: 110-120 cm is that of topsoil, matching the
# expected values accoding to the soil profile descrition. Soil horizons are
# A: 0-15, AB: 15-65, Bw1: 65-90 (apparently not sampled?), and Bw2: 90-120 cm.
# Maybe the layer limits are incorrect and the limits of the third layer is
# not known. For instance, the following could be the correct sequence:
# A: 0- 15, B: 30-40, C: ???, and D: 110-120 cm, with C between 65-90 cm. We
# will set these values:
layer33[
  evento_id_febr == "RO3213" & camada_id_febr == "A",
  profund_sup := ifelse(profund_sup == 110, 0, profund_sup)
]
layer33[
  evento_id_febr == "RO3213" & camada_id_febr == "A",
  profund_inf := ifelse(profund_inf == 120, 15, profund_inf)
]
layer33[
  evento_id_febr == "RO3213" & camada_id_febr == "B",
  profund_sup := ifelse(profund_sup == 0, 30, profund_sup)
]
layer33[
  evento_id_febr == "RO3213" & camada_id_febr == "B",
  profund_inf := ifelse(profund_inf == 15, 40, profund_inf)
]
layer33[
  evento_id_febr == "RO3213" & camada_id_febr == "C",
  profund_sup := ifelse(profund_sup == 30, 65, profund_sup)
]
layer33[
  evento_id_febr == "RO3213" & camada_id_febr == "C",
  profund_inf := ifelse(profund_inf == 40, 90, profund_inf)
]
rm(cols)

# ctb0034
# This dataset contains data on physical soil properties measured over thin
# layers (most of them are 5 cm thick) that do not necessarily correspond to the
# layers sampled in ctb0033.
layer34 <- febr::layer("ctb0034", "all")
layer34 <- data.table::as.data.table(layer34)
layer34[, dataset_id34 := dataset_id]
layer34[, dataset_id := NULL]
layer34[, camada_id_febr := camada_id_alt]

# Create a new column named thickness to store the thickness of each layer,
# calculated as profund_inf - profund_sup. If the thickness is negative, it
# indicates an error in the depth intervals.
layer34[, thickness := profund_inf - profund_sup]
layer34[thickness < 0, .N]
# There are 2 layers with negative thickness.
layer34[
  thickness < 0,
  .(evento_id_febr, camada_id_febr, profund_sup, profund_inf, thickness)
]

# RO1590: A 15-10 cm. The error comes from the source spreadsheet. Soil samples
# for chemical analysis were collected at 0- 20 cm and morphological 
# descriptions were made at 0-20 cm. Three solutions are possible: 5-10 cm, 
# 10-15 cm, or 15-20 cm. We check the number of layers in each depth interval to
# determine the most likely depth interval: 
layer34[profund_sup == 5 & profund_inf == 10, .N]
# 38 layers
layer34[profund_sup == 10 & profund_inf == 15, .N]
# 48 layers
layer34[profund_sup == 15 & profund_inf == 20, .N]
# 2
# The data reveals that there are more layers with depth intervals of 10-15 cm
# than with depth intervals of 5-10 cm or 15-20 cm. Therefore, we conclude that
# the most likely depth interval for the layer in question is 10-15 cm.
layer34[
  evento_id_febr == "RO1590" & camada_id_febr == "A",
  profund_sup := ifelse(profund_sup == 15, 10, profund_sup)
]
layer34[
  evento_id_febr == "RO1590" & camada_id_febr == "A",
  profund_inf := ifelse(profund_inf == 10, 15, profund_inf)
]

# RO1836: C 65-55 cm. The error comes from the source spreadsheet. We notice
# that the layer sampled for chemical analysis was collected at 60-70 cm, while
# the morphological description was made at 50-90 cm. So the most likely depth
# interval for the layer in question is 60-65 cm, as it falls within the limits
# of the layer sampled for chemical analysis and the morphological description,
# as well as honours the 5 cm thickness used for physical analysis across most
# of the dataset.
layer34[
  evento_id_febr == "RO1836" & camada_id_febr == "C",
  profund_sup := ifelse(profund_sup == 65, 60, profund_sup)
]
layer34[
  evento_id_febr == "RO1836" & camada_id_febr == "C",
  profund_inf := ifelse(profund_inf == 55, 65, profund_inf)
]

# Check for thickness different from 5 cm
layer34[, thickness := profund_inf - profund_sup]
layer34[, .N, by = thickness]
#    thickness     N
#        <int> <int>
# 1:         5   401
# 2:        23     1
# 3:        15     2
# 4:         4     3
# 5:         8     3
# 6:        16     2
# 7:        10     5
# 8:         6     2
layer34[
  thickness > 5,
  .(evento_id_febr, camada_id_febr, profund_sup, profund_inf, thickness)
]
#     evento_id_febr camada_id_febr profund_sup profund_inf thickness
#             <char>         <char>       <int>       <int>     <int>
#  1:         RO1836              B          17          40        23 Error
#  2:         RO1863              B          20          35        15 Error
#  3:         RO2580              B          28          36         8 Error
#  4:         RO3701              A           2          10         8
#  5:         RO3623              A           5          21        16
#  6:         RO3623              B          10          26        16
#  7:         RO2101              A           0          10        10
#  8:         RO3901              C          50          65        15
#  9:         RO3369              A           2          10         8
# 10:         RO3512              C          70          76         6
# 11:         RO3261              B          30          36         6
# 12:         RO3492              A           0          10        10
# 13:         RO3492              B          30          40        10
# 14:         RO3492              C          70          80        10
# 15:         RO3492              D         110         120        10

# RO1836. In the source spreadshet, the limits of the first layer for physical
# analysis are 13-18 cm, while that of the second are 17-40 cm, which is odd.
# The layers sampled for chemical analysis were collected at 0-15 cm and 30-40
# cm. The morphological description was made at 0-15 cm and 15-50 cm. Perhaps
# the limits of the first layer for physical analysis trully are 13-18 cm, as
# they are very close to the limits of the layer sampled for chemical analysis
# and the morphological description, and is 5 cm thick. The limits of the second
# layer for physical analysis (23 cm thick), however, are odd. As the lower
# limit of 40 cm is the same as that of the layer sampled for chemical analysis,
# we find that it could be correct and keep it as is. If we target a thickness
# of 5 cm, the upper limit of the layer should be 35 cm, and so we change it.
layer34[
  evento_id_febr == "RO1836" & camada_id_febr == "B",
  profund_sup := ifelse(profund_sup == 17, 35, profund_sup)
]

# RO1863. The second layer (B) for physical analysis reports a depth interval of 
# 20-35 cm. Samples for chemical analysis were collected at 0-10 cm and 30-40 
# cm, while the morphological description was made at 0-16 cm and 16-55 cm. The
# correct limits for the layer could be 20-25 cm or 30-35 cm. We check the 
# number of layers in each depth interval to determine the most likely depth
# interval:
layer34[profund_sup == 20 & profund_inf == 25, .N]
# 15 layer
layer34[profund_sup == 30 & profund_inf == 35, .N]
# 47 layers
# The data reveals that there are more layers with depth intervals of 30-35 cm
# than with depth intervals of 20-25 cm. It also matches the upper limit of the 
# layer sampled for chemical analysis and the morphological description. So we 
# conclude that the most likely depth interval for the layer in question is 
# 30-35 cm.
layer34[
  evento_id_febr == "RO1863" & camada_id_febr == "B",
  profund_sup := ifelse(profund_sup == 20, 30, profund_sup)
]

# RO2580. The second layer (B) for physical analysis reports a depth interval of
# 28-36 cm. The second layer sampled for chemical analysis was collected at B:
# 22-32 cm, while the morphological description was made at 15-45 cm. In
# Portuguese, the numbers 3 and 6 sound similar, and it is possible that the
# upper limit of 36 cm was a typo in the source spreadsheet. The correct limits
# for the layer could be 28-33 cm, yielding a thickness of 5 cm. This depth
# interval more or less matches the lower limit of the layer sampled for
# chemical analysis (32 cm). So we conclude that the most likely depth interval
# for the layer in question is 28-33 cm.
layer34[
  evento_id_febr == "RO2580" & camada_id_febr == "B",
  profund_inf := ifelse(profund_inf == 36, 33, profund_inf)
]

# We will keep the remaining layers with thickness different from 5 cm for a 
# future check. One thing that we noticed is that a few cases are Latossolos.
layer34[, thickness := NULL]

# Merge the two datasets
sapply(list(layer33, layer34), nrow)
# 10779 and 419 layers

# Join layers from ctb0033 and ctb0034 #########################################
# Apply overlap joint to merge layers from ctb0033 and ctb0034
# ctb0033 has data on chemical soil properties measured over soil layers
# corresponding to entire or part of pedological horizons. Some layers appear to
# cross the limits of pedological horizons, while others are entirely within a
# single pedological horizon.
# ctb0034 has data on physical soil properties measured over thin layers (most
# of them are 5 cm thick) that do not necessarily correspond to the layers
# sampled in ctb0033. Some layers appear to cross the limits of pedological
# horizons, while others are entirely within a single pedological horizon.
# This is some messy data!
data.table::setkey(layer33, evento_id_febr, profund_sup, profund_inf)
nrow(layer33)
# 10779 layers
data.table::setkey(layer34, evento_id_febr, profund_sup, profund_inf)
nrow(layer34)
# 419 layers
layerRO <- data.table::foverlaps(
  x = layer34, # the thin layers from ctb0034
  y = layer33, # the thick layers from ctb0033
  type = "within", # only keep rows from x (layer34) that are within y (layer33)
  # keep all matches, even if there are multiple matches for a single row in x
  mult = "all"
)
nrow(layerRO)
# 419 layers
# foverlaps() only keeps all x (layer34) rows, matched or not. To also keep
# layer33 rows that were never matched by any layer34 row, identify the
# unmatched layer33 row indices and append them.
overlap_id <- data.table::foverlaps(layer34, layer33,
  type = "within", mult = "all", which = TRUE
)
unmatched33 <- layer33[setdiff(seq_len(nrow(layer33)), unique(overlap_id$yid))]
layerRO <- data.table::rbindlist(
  list(layerRO, unmatched33),
  fill = TRUE
)
nrow(layerRO)
# 10942 layers
rm(overlap_id, unmatched33)

# Fill-in missing values: if profund_sup, profund_inf, and camada_id_febr are
# NA, get the values from i.profund_sup, i.profund_inf, and i.camada_id_febr
# respectively.
layerRO[
  is.na(profund_sup) & !is.na(i.profund_sup),
  profund_sup := i.profund_sup
]
layerRO[
  is.na(profund_inf) & !is.na(i.profund_inf),
  profund_inf := i.profund_inf
]
layerRO[
  is.na(camada_id_febr) & !is.na(i.camada_id_febr),
  camada_id_febr := i.camada_id_febr
]

# Sort by evento_id_febr, camada_id_febr, profund_sup, profund_inf
layerRO <- layerRO[
  order(evento_id_febr, camada_id_febr, profund_sup, profund_inf)
]
nrow(layerRO)
# 10942 layers

if (FALSE) {
  cols <- c(
    "evento_id_febr", "camada_id_febr", "profund_sup", "profund_inf",
    "ph_2.5h2o_eletrodo", "carbono_xxx_xxx",
    "i.profund_sup", "i.profund_inf", "densidade_solo_xxx"
  )
  View(layerRO[, ..cols])
}

# Standardize column names
str(layerRO)
layerRO[, dataset_id := "ctb0033"]
layerRO[, i.camada_id_febr := NULL]
layerRO[, i.profund_sup := NULL]
layerRO[, i.profund_inf := NULL]
layerRO[, dataset_id34 := NULL]
new_names <- c(
  evento_id_febr = "observacao_id",
  camada_id_febr = "camada_nome",
  ph_2.5h2o_eletrodo = "ph",
  carbono_xxx_xxx = "carbono",
  areia.05mm2_xxx_xxx = "areia",
  silte.002mm.05_xxx_xxx = "silte",
  argila0mm.002_xxx_xxx = "argila",
  terrafina_xxx_xxx = "terrafina",
  ctc_soma_calc = "ctc",
  densidade_solo_xxx = "dsi"
)
data.table::setnames(layerRO, old = names(new_names), new = new_names)
cols <- intersect(names(layerRO), tolower(names(layerRO)))
layerRO <- layerRO[, ..cols]
layerRO[, dataset_id := NULL]

# Merge events and layers ######################################################
rondonia <- merge(eventRO, layerRO, all = TRUE)
summary_soildata(rondonia)
# Layers: 10946
# Events: 2998
# Georeference: 2911 (yes) / 87 (no)
# Date: 2998 (yes) / 0 (no)
# Datasets: 1
# 10789 layers
rm(eventRO, layerRO, layer33, layer34, new_names, cols)

# Standardize measurement units
rondonia[, areia := areia * 10]
rondonia[, argila := argila * 10]
rondonia[, silte := silte * 10]
rondonia[, terrafina := terrafina * 10]
rondonia[, carbono := carbono * 10]

# Duplicated layers ############################################################
# Deal with the identification of events containing duplicated layers. These are
# extra samples for soil fertility assessment collected nearby the soil profile.
# According to the source documentation, these extra samples were collected in
# the 0-20 cm layer.
rondonia[, EXTRA := duplicated(profund_sup), by = observacao_id]
nrow(rondonia[EXTRA == TRUE, ])
# 64 duplicated layers
nrow(unique(rondonia[EXTRA == TRUE, "observacao_id"]))
# 25 events with duplicated layers
# Append the layer name (camada_nome) to the observation id (observacao_id) for
# duplicated layers. This will create a new event for each duplicated layer,
# enabling to identify the source of the sample. First check if there is any
# duplicated layer with missing layer name (camada_nome).
problem <- rondonia[EXTRA == TRUE & is.na(camada_nome), .N]
if (problem > 0) {
  stop("There are duplicated layers with missing layer name (camada_nome).")
} else {
  rondonia[EXTRA == TRUE, observacao_id := paste0(observacao_id, camada_nome)]
  rondonia[, id := paste0(dataset_id, "-", observacao_id)]
}
# Next we add a random perturbation to the coordinates of those extra samples 
# only to pass checks for duplicated events. We use a small perturbation of 1 m, 
# which is negligible for most practical purposes. The coordinates are
# transformed to UTM zone 20S (EPSG:32720) before applying the perturbation and
# then transformed back to WGS84 (EPSG:4326).
# Use sf::st_jitter() with amount = 1 m, where runif(1, -amount, amount)
amount <- 1
extra_coords <- rondonia[
  EXTRA == TRUE & !is.na(coord_x) & !is.na(coord_y),
  c("id", "coord_x", "coord_y")
]
extra_coords <- sf::st_as_sf(
  extra_coords,
  coords = c("coord_x", "coord_y"), crs = 4326
)
extra_coords <- sf::st_transform(extra_coords, crs = 32720)
set.seed(1984)
extra_coords <- sf::st_jitter(extra_coords, amount = amount)
extra_coords <- sf::st_transform(extra_coords, crs = 4326)
extra_coords <- sf::st_coordinates(extra_coords)
rondonia[
  EXTRA == TRUE & !is.na(coord_x) & !is.na(coord_y),
  coord_x := extra_coords[, "X"]
]
rondonia[
  EXTRA == TRUE & !is.na(coord_x) & !is.na(coord_y),
  coord_y := extra_coords[, "Y"]
]
# In coord_fonte, append " + amount m jitter" to the existing text.
rondonia[
  EXTRA == TRUE & !is.na(coord_x) & !is.na(coord_y),
  coord_fonte := paste0(coord_fonte, " + ", amount, " m jitter")
]
rondonia[, .N, by = coord_fonte]
#                coord_fonte     N
#                     <char> <int>
# 1:                     GPS 10787
# 2:        GPS + 1 m jitter    52
# 3:                    <NA>    99
# 4: Google Maps (curadoria)     8
# In coord_precisao, add 1 to the existing value if it a number larger than 0.
rondonia[
  EXTRA == TRUE & !is.na(coord_x) & !is.na(coord_y) & !is.na(coord_precisao) &
    coord_precisao > 0, 
  coord_precisao := coord_precisao + 1
]
rondonia[, summary(coord_precisao)]
rondonia[, EXTRA := NULL]
rm(extra_coords, amount, problem)

# Depth limits #################################################################
# Check for missing depth limits
rondonia[is.na(profund_sup) | is.na(profund_inf), .N, by = observacao_id]
# 0 layers with missing depth limits.

# Identification metadata ######################################################
# Create missing columns in the data from Rondônia
title <- "Dados de 'Zoneamento Socioeconômico-Ecológico do Estado de Rondônia'"
rondonia[, dataset_titulo := title]
rondonia[, dataset_licenca := "CC-BY-4.0"]
rondonia[, organizacao_nome := "Governo do Estado de Rondônia"]
summary_soildata(rondonia)
# Layers: 10946
# Events: 3062
# Georeference: 2963 (yes) / 99 (no)
# Date: 3062 (yes) / 0 (no)
# Datasets: 1

# Read SoilData data processed in the previous script
soildata <- data.table::fread(
  input = "data/10_soildata.txt",
  sep = "\t", na.strings = c("", "NA")
)
summary_soildata(soildata)
# Layers: 49769
# Events: 13859
# Georeference: 10859 (yes) / 3000 (no)
# Date: 13707 (yes) / 152 (no)
# Datasets: 235

# Add a column to indicate the coordinate reference system (CRS)
soildata[, coord_datum := 4326] # EPSG code for WGS84

# Manually correct the depth limits for specific events in ctb0032 after
# checking the source documentation. This is necessary for the overlap join
# performed later.
soildata[
  dataset_id == "ctb0032" & observacao_id == "RO1174" & camada_nome == "R",
  profund_sup := ifelse(is.na(profund_sup), 0, profund_sup)
]
soildata[
  dataset_id == "ctb0032" & observacao_id == "RO1174" & camada_nome == "R",
  profund_inf := ifelse(is.na(profund_inf), 20, profund_inf)
]
soildata[
  dataset_id == "ctb0032" & observacao_id == "RO2740" & camada_nome == "A",
  profund_sup := ifelse(is.na(profund_sup), 0, profund_sup)
]
soildata[
  dataset_id == "ctb0032" & observacao_id == "RO2740" & camada_nome == "A",
  profund_inf := ifelse(is.na(profund_inf), 20, profund_inf)
]
soildata[
  dataset_id == "ctb0032" & observacao_id == "RO2322" & camada_nome == "Cg2",
  profund_sup := ifelse(profund_sup == 50, 90, profund_sup)
]
soildata[
  dataset_id == "ctb0032" & observacao_id == "RO2322" & camada_nome == "Cg2",
  profund_inf := ifelse(profund_inf == 90, 120, profund_inf)
]

# From observacao_id == "RO1687", drop layer with camada_nome == "Bw3"
# After checking the documentation, we decided that this is a possible duplicate
# of the morphological description. There is no additional layer with chemical
# or physical properties for this layer, so we will drop it.
soildata <- soildata[
  !(dataset_id == "ctb0032" & observacao_id == "RO1687" & camada_nome == "Bw3")
]

# From observacao_id == "RO1238", drop layer with camada_nome == "Bt3"
# After checking the documentation, we decided that this is a possible duplicate
# of the morphological description. There is no additional layer with chemical
# or physical properties for this layer, so we will drop it.
soildata <- soildata[
  !(dataset_id == "ctb0032" & observacao_id == "RO1238" & camada_nome == "Bt3")
]

# Check the spatial distribution of events in Brazil
if (FALSE) {
  x11()
  plot(soildata[, c("coord_x", "coord_y")])
}

# Morphological descriptions ###################################################
# ctb0032 (soildata) contains morphological descriptions of soil horizons for
# various soil profiles, while ctb0033 and ctb0034 (rondonia) contain chemical
# and physical properties of soil layers for various soil profiles as well,
# respectively. Not all soil profiles have morphological descriptions and 
# laboratory analyses. The layers in rondonia do not necessarily correspond to
# the soil horizons in soildata. To merge the two datasets, we will perform an
# overlap join based on the depth limits of the layers and horizons.
# Extract data from Rondônia (ctb0032)
ctb0032_cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf")
ctb0032 <- soildata[dataset_id == "ctb0032", ..ctb0032_cols]
nrow(ctb0032)
# 10872

# Perform a join between the analythical data from Rondônia (rondonia) and the
# morphological descriptions from ctb0032
data.table::setkey(ctb0032, observacao_id, profund_sup, profund_inf)
data.table::setkey(rondonia, observacao_id, profund_sup, profund_inf)
rondonia_overlap <- data.table::foverlaps(
  x = rondonia, # analytical data from Rondônia (ctb0033 and ctb0034)
  y = ctb0032, # morphological descriptions from ctb0032
  type = "within", mult = "all"
)
# In principle, there should not be any unmatched rows in rondonia, as all
# layers should have a corresponding morphological description in ctb0032.
# However, we will check for any unmatched rows and correct them if possible.
overlap_id <- data.table::foverlaps(rondonia, ctb0032,
  type = "within", mult = "all", which = TRUE
)
unmatched_ro <- rondonia[setdiff(seq_len(nrow(rondonia)), unique(overlap_id$xid))]
nrow(unmatched_ro)
# 0 
rm(overlap_id, unmatched_ro)
nrow(rondonia)
# 10946 layers before the overlap join
nrow(rondonia_overlap)
# 10957 layers after the overlap join. Why?
# The increase in the number of rows is due to the use of mult = "all", which
# keeps all matches, even if there are multiple matches for a single row in x.
# These results in duplicated layers (morphological descriptions) for some
# events. Within these layers we may find multiple thiner layers with data on
# chemical and physical properties. For example, for observacao_id == "RO1012",
# camada_nome == "AB", with profund_sup == 25 and profund_inf == 60, was
# duplicated to accomodate two sub layers, one at 30-35 cm and another at 40-50
# cm depth. Notice that the sub layers not necessarily cover the entire depth
# of the pedogenetic horizon.

# First fill-in missing values
# If camada_nome, profund_sup, and profund_inf are NA, get it from
# i.camada_nome, i.profund_sup, and i.profund_inf respectively.
rondonia_overlap[
  is.na(camada_nome) & !is.na(i.camada_nome),
  camada_nome := i.camada_nome
]
rondonia_overlap[
  is.na(profund_sup) & !is.na(i.profund_sup),
  profund_sup := i.profund_sup
]
rondonia_overlap[
  is.na(profund_inf) & !is.na(i.profund_inf),
  profund_inf := i.profund_inf
]

# Identify events that have any duplicated layers after the overlap join.
# We check for duplicated layers based on both the upper and lower depth limits
# (profund_sup and profund_inf) of the morphological descriptions within each
# event (observacao_id).
rondonia_overlap[,
  any_copied := any(duplicated(profund_sup) | duplicated(profund_inf)),
  by = observacao_id
]
rondonia_overlap[any_copied == TRUE, .N, by = observacao_id]
# 107 events with duplicated layers after the overlap join.
if (FALSE) {
  View(rondonia_overlap[any_copied == TRUE, .(
    observacao_id, camada_nome, profund_sup, profund_inf,
    i.camada_nome, i.profund_sup, i.profund_inf
  )])
}
# We notice that a layer may have been duplicated once (n_copied == 2) or twice
# (n_copied == 3). We need to identify the number of times each layer is
# duplicated within each event. So we will create a new column named n_copied to
# store the number of times each layer is duplicated within each event.
rondonia_overlap[, n_copied := .N,
  by = .(observacao_id, camada_nome, profund_sup, profund_inf)
]
rondonia_overlap[, .N, by = n_copied]
#    n_copied     N
#       <int> <int>
# 1:        1 10607
# 2:        2   314
# 3:        3    36
if (FALSE) {
  View(rondonia_overlap[n_copied > 1, .(
    observacao_id, camada_nome, profund_sup, profund_inf,
    i.camada_nome, i.profund_sup, i.profund_inf, n_copied
  )])
}
# We solve the duplication problem by tweeking the depth limits of the
# duplicated layers. The strategy depends on how many times a layer is
# duplicated in the event:
# n_copied == 2: If a layer is duplicated once, we set the depth limits of the
#   first layer to profund_sup == profund_sup and profund_inf == i.profund_inf.
#   For the second layer, we set profund_sup == i.profund_inf of the first layer
#   and profund_inf == profund_inf. We identify the first and second layers by
#   using data.table .I.
# n_copied == 3: If a layer is duplicated twice, we set the depth limits of the
#   first layer to profund_sup == profund_sup and profund_inf == i.profund_inf.
#   For the second layer, we set profund_sup == i.profund_inf of the first layer
#   and profund_inf == i.profund_inf. For the third layer, we set profund_sup ==
#   i.profund_inf of the second layer and profund_inf == profund_inf. We
#   identify the first, second and third layers by using data.table .I.

# Order duplicated analytical layers by their analytical depth intervals.
cols <- c(
  "observacao_id", "camada_nome", "profund_sup", "profund_inf",
  "i.profund_sup", "i.profund_inf"
) 
data.table::setorderv(rondonia_overlap, cols, na.last = TRUE)
if (any(rondonia_overlap$n_copied > 3L)) {
  stop("More than three analytical layers match one morphological horizon.")
}

# Reconstruct non-overlapping intervals separately for each duplicated
# morphological horizon. The outer boundaries remain the horizon boundaries.
rondonia_overlap[
  n_copied == 2L,
  `:=`(
    profund_sup = c(profund_sup[1L], i.profund_inf[1L]),
    profund_inf = c(i.profund_inf[1L], profund_inf[1L])
  ),
  by = .(observacao_id, camada_nome, profund_sup, profund_inf)
]
rondonia_overlap[
  n_copied == 3L,
  `:=`(
    profund_sup = c(profund_sup[1L], i.profund_inf[1L], i.profund_inf[2L]),
    profund_inf = c(i.profund_inf[1L], i.profund_inf[2L], profund_inf[1L])
  ),
  by = .(observacao_id, camada_nome, profund_sup, profund_inf)
]
# Check for negative thickness after the reconstruction of non-overlapping
if (rondonia_overlap[
  !is.na(profund_sup) & !is.na(profund_inf) & profund_inf < profund_sup,
  .N
] > 0L) {
  stop("Depth reconstruction created intervals with negative thickness.")
}
if (FALSE) {
  View(rondonia_overlap[n_copied > 1, .(
    observacao_id, camada_nome, profund_sup, profund_inf,
    i.camada_nome, i.profund_sup, i.profund_inf, n_copied
  )])
}
# The main assumption for using the previous strategy is that chamical and 
# physical soil properties are homogeneous within each pedological horizon. We
# know that this is not always true, and a more elegant solution should be used
# in the future.























# What we have to do is merge the two rows into one, keeping the
# analytical data from both layers and the morphological description.

# Consolidate duplicated matched horizons: one row per ctb0032 horizon,
# keeping the first non-missing value per column across duplicate rows.
key_cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf")
pick_first_non_na <- function(x) x[which.max(!is.na(x))]

matched_overlap <- rondonia_overlap[complete.cases(rondonia_overlap[, ..key_cols])]
rondonia_overlap <- data.table::rbindlist(
  list(
    matched_overlap[
      , lapply(.SD, pick_first_non_na),
      by = key_cols,
      .SDcols = setdiff(names(matched_overlap), key_cols)
    ],
    rondonia_overlap[!complete.cases(rondonia_overlap[, ..key_cols])]
  ),
  use.names = TRUE,
  fill = TRUE
)

data.table::setorderv(rondonia_overlap, c("observacao_id", "profund_sup", "profund_inf"))
rm(key_cols, pick_first_non_na, matched_overlap)

nrow(rondonia_overlap)

write.csv(rondonia_overlap, "tmp/rondonia_overlap_join.csv", row.names = FALSE)



# Remove existing data from Rondônia (morphological descriptions)
length(unique(soildata[, id]))
# 13859 events
soildata <- soildata[dataset_id != "ctb0032", ]
length(unique(soildata[, id]))
# 10945 events

# Merge data from Rondônia with the SoilData snapshot
col_ro <- intersect(names(soildata), names(rondonia_overlap))
soildata <-
  data.table::rbindlist(
    list(soildata, rondonia_overlap[, ..col_ro]),
    fill = TRUE
  )
# ATTENTION: ctb0032 has morphological descriptions and soil horizons are
# designated by camada_nome like "A", "B1", "B2", "C", etc. In ctb0033 and
# ctb0034, the layers are not necessarily coincident with soil horizons, and
# camada_nome is letter A, B, C, or D. For example, observacao_id = RO0607 has
# three pedological horizons (A: 0 - 10 cm; Bw1: 10 - 50 cm; Bw2: 50 - 120 cm)
# and three sampled layers (A: 0- 10 cm; B: 10- 20 cm; C: 50-120 cm). So, after
# merging the datasets, we remain with the A-B-C-D names for layers.

# In the future, we need to harmonize this.
# Here what we will do is replace A-B-C-D with the depth intervals.
soildata[
  dataset_id == "ctb0033",
  camada_nome := paste0(profund_sup, "-", profund_inf)
]

# Write data to disk ###########################################################
summary_soildata(soildata)
# Layers: 49684
# Events: 14006
# Georeference: 10907 (yes) / 3099 (no)
# Date: 13854 (yes) / 152 (no)
# Datasets: 235
data.table::fwrite(soildata, "data/11_soildata.txt", sep = "\t")

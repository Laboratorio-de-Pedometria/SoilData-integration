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
eventRO[, dataset_id := "ctb0033"]
eventRO[, coord_datum_epsg := NULL]
eventRO[, coord_datum_epsg := "EPSG:4326"]
new_names <- c(
  evento_id_febr = "observacao_id",
  coord_longitude = "coord_x",
  coord_latitude = "coord_y",
  coord_municipio_nome = "municipio_id",
  coord_estado_sigla = "estado_id"
)
data.table::setnames(
  eventRO,
  old = names(new_names), new = new_names, skip_absent = TRUE
)
eventRO[, estado_id := "RO"]
cols <- intersect(names(eventRO), tolower(names(eventRO)))
eventRO <- eventRO[, ..cols]
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

# Download current version from FEBR: layers
# ctb0033
layer33 <- febr::layer("ctb0033", "all")
layer33 <- data.table::as.data.table(layer33)
layer33[, camada_id_sisb := NULL]
# ctb0034
layer34 <- febr::layer("ctb0034", "all")
layer34 <- data.table::as.data.table(layer34)
layer34[, dataset_id34 := dataset_id]
layer34[, dataset_id := NULL]
layer34[, camada_id_febr := camada_id_alt]
sapply(list(layer33, layer34), nrow)
# 10779 and 419 layers
# Merge layers from ctb0033 and ctb0034
layerRO <- merge(layer33, layer34,
  by = c("evento_id_febr", "camada_id_febr"),
  suffixes = c("", ".IGNORE"),
  all = TRUE
)
nrow(layerRO)
# 10785 layers after merge
layerRO[, dataset_id := "ctb0033"]
colnames(layerRO)
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
layerRO[, dataset_id34 := NULL]

# Merge events and layers
rondonia <- merge(eventRO, layerRO, all = TRUE)
nrow(rondonia)
# 10789 layers

# Standardize measurement units
rondonia[, areia := areia * 10]
rondonia[, argila := argila * 10]
rondonia[, silte := silte * 10]
rondonia[, terrafina := terrafina * 10]
rondonia[, carbono := carbono * 10]

# Deal with the identification of events containing duplicated layers
# These are extra samples for soil fertility assessment collected nearby the
# soil profile
rondonia[, EXTRA := duplicated(profund_sup), by = observacao_id]
nrow(rondonia[EXTRA == TRUE, ])
# 63 duplicated layers
nrow(unique(rondonia[EXTRA == TRUE, "observacao_id"]))
# 24 events with duplicated layers
# Rename the duplicated layers by pasting the layer id (a letter) to the
# observation id, for example, RO0600C. This will create a new event for each 
# duplicated layer, enabling to identify the source of the sample.
rondonia[EXTRA == TRUE, observacao_id := paste0(observacao_id, camada_nome)]
rondonia[, id := paste0(dataset_id, "-", observacao_id)]
# Next we add a random perturbation to the coordinates of extra samples only to 
# pass checks for duplicated events. We use a small perturbation of 1 m, which 
# is negligible for most practical purposes. The coordinates are transformed to 
# UTM zone 20S (EPSG:32720) before applying the perturbation and then 
# transformed back to WGS84 (EPSG:4326).
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
# In coord_precisao, add 1 to the existing value if it a number larger than 0.
rondonia[
  EXTRA == TRUE & !is.na(coord_x) & !is.na(coord_y) & !is.na(coord_precisao) & coord_precisao > 0,
  coord_precisao := coord_precisao + 1
]
rondonia[, summary(coord_precisao)]
rondonia[, EXTRA := NULL]
rm(extra_coords, amount)
summary_soildata(rondonia)
# Layers: 10789
# Events: 3061
# Georeference: 2962 (yes) / 99 (no)
# Date: 3061 (yes) / 0 (no)
# Datasets: 1

# Read SoilData data processed in the previous script
soildata <- data.table::fread(
  "data/10_soildata.txt",
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

# order rows by dataset_id, observacao_id, profund_sup and profund_inf
soildata <- soildata[order(dataset_id, observacao_id, profund_sup, profund_inf), ]

if (FALSE) {
  x11()
  plot(soildata[, c("coord_x", "coord_y")])
}

# Merge data from Rondônia with the SoilData snapshot
# First create missing columns in the data from Rondônia
rondonia[, dataset_titulo := "Zoneamento Socioeconômico-Ecológico do Estado de Rondônia"]
rondonia[, dataset_licenca := "CC-BY-4.0"]
rondonia[, organizacao_nome := "Governo do Estado de Rondônia"]
# Then remove existing data from Rondônia (morphological descriptions)
length(unique(soildata[, id]))
# 13973 events
soildata <- soildata[dataset_id != "ctb0032", ]
length(unique(soildata[, id]))
# 11059 events
col_ro <- intersect(names(soildata), names(rondonia))
soildata <- data.table::rbindlist(list(soildata, rondonia[, ..col_ro]), fill = TRUE)
# ATENTION: ctb0032 has morphological descriptions and soil horizons are designated by 
# camada_nome like "A", "B1", "B2", "C", etc. In ctb0033 and ctb0034, the layers are not necessarily
# coincident with soil horizons, and camada_nome is letter A, B, C, or D. So, after merging the
# datasets, we remain with the A-B-C-D names for layers. In the future, we need to harmonize this.
# Here what we will do is replace A-B-C-D with the depth intervals.
soildata[dataset_id == "ctb0033", camada_nome := paste0(profund_sup, "-", profund_inf)]

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 50315
# Events: 14120
# Georeferenced events: 10990
# Datasets: 235
data.table::fwrite(soildata, "data/11_soildata.txt", sep = "\t")

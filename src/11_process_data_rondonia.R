# title: SoilData Integration
# subtitle: Process data from Rondônia
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
# licence: MIT
rm(list = ls())

# Load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
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
source("src/00_helper_functions.R")

# Zoneamento Socioeconômico-Ecológico do Estado de Rondônia (ctb0033 and ctb0034)
# Download current version from FEBR: events
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
sapply(list(event33 = event33, event34 = event34), nrow) # 2998 and 107 events
eventRO <- merge(event33, event34, all = TRUE)
nrow(eventRO) # 2999 events after merge
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
data.table::setnames(eventRO, old = names(new_names), new = new_names, skip_absent = TRUE)
eventRO[, estado_id := "RO"]
cols <- intersect(names(eventRO), tolower(names(eventRO)))
eventRO <- eventRO[, ..cols]
eventRO[, data_coleta_ano := as.integer(format(data_coleta, "%Y"))]
nrow(eventRO[is.na(data_coleta_ano), ]) # 87 events missing the sampling date
# Set the sampling date to 1996 for events with missing data
eventRO[is.na(data_coleta_ano), data_coleta_ano := 1996]
if (FALSE) {
  x11()
  plot(eventRO[, c("coord_x", "coord_y")])
}
str(eventRO)

# Attribute new coordinates to events falling in water bodies or outside the state of Rondônia
# Create a column named observacao_cura to store information about the correction (in Portuguese),
# as well as a copy of the original coordinates, the data of the collection, and the accronym of the
# author (ASR).

# RO2656
# Ao consultar as coordenadas originais do ponto RO2656, registradas no SoilData, e visualizar as
# mesmas no GoogleMaps, verificamos que realmente o ponto cai dentro de um curso de água na divisa
# entre Brasil e Bolívia. O estudo da documentação do trabalho revelou que pode haver um erro
# posicional de aproximadamente 100 m. Segundo a descrição textual da localização, o perfil de solo
# foi coletado na "Beira Rio Guapore". Novas coordenadas, coletadas no Google Maps, serão atribuídas
# manualmente ao ponto.
# More information about the location can be found at:
# https://github.com/Laboratorio-de-Pedometria/mapbiomas-soil-train-prep/issues/5
# RO2656: -61.306907, -13.485739
id <- "RO2656"
eventRO[observacao_id == id, coord_x := -61.306907]
eventRO[observacao_id == id, coord_y := -13.485739]
cura_info <- paste0(
  "2024-06-05 (ASR): As coordenadas originais do ponto RO2656 (",
  eventRO[observacao_id == id, coord_x], ", ",
  eventRO[observacao_id == id, coord_y], ") caem dentro de um curso de água na divisa entre Brasil e Bolívia. ",
  " O estudo da documentação do trabalho revelou que pode haver um erro posicional de aproximadamente 100 m. ",
  " Segundo a descrição textual da localização, o perfil de solo foi coletado na Beira Rio Guapore. ",
  " Novas coordenadas, coletadas no Google Maps, foram atribuídas manualmente ao ponto."
)
eventRO[observacao_id == id, observacao_cura := cura_info]

# RO2953: -9.765833 -65.73528 (original)
# The sample location is in Bolivia, close to the Brazilian border
# It is possible that the authors of the data collected the soil samples in Bolivian territory,
# for example, due to ease of access.
# There is no additional information in the dataset to confirm this hypothesis.
# The coordinates were changed to a location in Rondônia, Brazil.
# -9.764905, -65.735686
# google_maps(eventRO[observacao_id == "RO2953", ])
id <- "RO2953"
eventRO[observacao_id == id, coord_x := -65.735686]
eventRO[observacao_id == id, coord_y := -9.764905]
cura_info <- paste0(
  "2024-06-05 (ASR): As coordenadas originais do ponto RO2953 (",
  eventRO[observacao_id == id, coord_x], ", ",
  eventRO[observacao_id == id, coord_y], ") estão em território boliviano, próximo à divisa com o Brasil. ",
  " O estudo da documentação do trabalho revelou que pode haver um erro posicional de aproximadamente 100 m. ",
  " Novas coordenadas, coletadas no Google Maps, foram atribuídas manualmente ao ponto."
)
eventRO[observacao_id == id, observacao_cura := cura_info]
rm(id, cura_info)

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
sapply(list(layer33, layer34), nrow) # 10 779 and 419 layers
# Merge layers from ctb0033 and ctb0034
layerRO <- merge(layer33, layer34,
  by = c("evento_id_febr", "camada_id_febr"),
  suffixes = c("", ".IGNORE"),
  all = TRUE
)
nrow(layerRO) # 10 785 layers after merge
layerRO[, dataset_id := "ctb0033"]
colnames(layerRO)
new_names <- c(
  evento_id_febr = "observacao_id",
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
nrow(rondonia) # 10 789 layers

# Standardize measurement units
rondonia[, areia := areia * 10]
rondonia[, argila := argila * 10]
rondonia[, silte := silte * 10]
rondonia[, terrafina := terrafina * 10]
rondonia[, carbono := carbono * 10]

# Deal with the identification of events containing duplicated layers
# These are extra samples for soil fertility assessment collected nearby the soil profile
rondonia[, EXTRA := duplicated(profund_sup), by = observacao_id]
nrow(rondonia[EXTRA == TRUE, ]) # 63 duplicated layers
nrow(unique(rondonia[EXTRA == TRUE, "observacao_id"])) # 24 events with duplicated layers
# Rename the duplicated layers by pasting the layer id (a letter) to the observation id, for
# example, RO0600C.
rondonia[EXTRA == TRUE, observacao_id := paste0(observacao_id, camada_id_febr)]
rondonia[, id := paste0(dataset_id, "-", observacao_id)]

# Add random perturbation to the coordinates of extra samples
# Use sf::st_jitter() with amount = 100 m, where runif(1, -amount, amount)
amount <- 100
extra_coords <- rondonia[EXTRA == TRUE & !is.na(coord_x), c("id", "coord_x", "coord_y")]
extra_coords <- sf::st_as_sf(extra_coords, coords = c("coord_x", "coord_y"), crs = 4326)
extra_coords <- sf::st_transform(extra_coords, crs = 32720)
set.seed(1984)
extra_coords <- sf::st_jitter(extra_coords, amount = amount)
extra_coords <- sf::st_transform(extra_coords, crs = 4326)
extra_coords <- sf::st_coordinates(extra_coords)
rondonia[EXTRA == TRUE & !is.na(coord_x), coord_x := extra_coords[, "X"]]
rondonia[EXTRA == TRUE & !is.na(coord_x), coord_y := extra_coords[, "Y"]]
rondonia[, EXTRA := NULL]
summary_soildata(rondonia)
# Layers: 10789
# Events: 3061
# Georeferenced events: 2962

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/10_soildata.txt", sep = "\t")
soildata[, coord_datum_epsg := 4326]
summary_soildata(soildata)
# Layers: 50438
# Events: 14011
# Georeferenced events: 10980
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
length(unique(soildata[, id])) # 14 011 events
soildata <- soildata[dataset_id != "ctb0032", ]
length(unique(soildata[, id])) # 11 097 events
col_ro <- intersect(names(soildata), names(rondonia))
soildata <- data.table::rbindlist(list(soildata, rondonia[, ..col_ro]), fill = TRUE)
summary_soildata(soildata)
# Layers: 50353
# Events: 14158
# Georeferenced events: 11028

# Write data to disk
data.table::fwrite(soildata, "data/11_soildata.txt", sep = "\t")

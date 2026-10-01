# title: SoilData Integration
# subtitle: Clean layers
# author: Alessandro Rosa
# date: 2026
# licence: MIT
# description: This script cleans individual soil layers in the dataset-cleaned
# integrated Brazilian Soil Dataset produced by script 14. It standardizes layer
# names and depth intervals, removes repeated or invalid layers, normalizes soil
# property values, applies known layer corrections, plots the spatial
# distribution, and exports the layer-cleaned dataset to data/15_soildata.txt.
rm(list = ls())

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Load required packages
library(data.table)
library(sf)
library(geobr)

# Read Brazilian state boundaries
brazil <- read_brazil_states()

# Read the dataset-cleaned SoilData from the previous script
soildata <- data.table::fread("data/14_soildata.txt", sep = "\t")
summary_soildata(soildata)

# Clean events
# Correct date (there is a typo in the spreadsheet). We checked the original document.
soildata[id == "ctb0585-Perfil-9", data_ano := ifelse(all(data_ano == 1993), 1983, data_ano)]

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
nrow(soildata_events)
# 13255 events
# Identify duplicated events
# Duplicated events have equal spatial and temporal coordinates.
# Make sure to analise events with complete spatial and temporal coordinates.
# For every duplicate, identify and show the copy.
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0010
# Identify duplicated events in ctb0010 dataset
# dataset_titulo = Conjunto de dados da dissertação 'Substâncias húmicas em solos de diferentes
# feições geomorfológicas no rebordo do planalto do Rio Grande do Sul'.
# This dataset (Fábio Menezes, UFSM) contains many events with the same coordinates. However, this
# is not entirely a mistake. The events were sampled in a transect with points very close to each
# other and the author simply recorded the same coordinates for all points in the transect.
idx_duplicated <- soildata_events[dataset_id == "ctb0010" & duplicated == TRUE, V1]
# Create a spatial object for ctb0010 dataset
ctb0010_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# mapview::mapview(ctb0010_sf)  # Check points in an interactive map
# Transform to UTM
ctb0010_sf <- sf::st_transform(ctb0010_sf, crs = 32720)
# Jitter coordinates by 1 meter to separate overlapping points and pass the duplication test. But
# we should check the source document and update the coordinates if possible in the source data.
amount <- 1
set.seed(1984) # For reproducibility
ctb0010_sf <- sf::st_jitter(ctb0010_sf, amount = amount)
# Transform back to WGS84
ctb0010_sf <- sf::st_transform(ctb0010_sf, crs = 4326)
# Extract coordinates from the spatial object
ctb0010_sf <- data.table(
  observacao_id = ctb0010_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0010_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0010_sf)[, "Y"]
)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = ctb0010_sf[, coord_x],
  coord_y = ctb0010_sf[, coord_y],
  # In coord_fonte, append " + amount m jitter" to the existing text.
  coord_fonte = paste0(coord_fonte, " + ", amount, " m jitter"),
  # In coord_precisao, add 'amount' to the existing value if it a number larger than 0.
  coord_precisao = ifelse(coord_precisao > 0, coord_precisao + amount, coord_precisao)
)]
rm(ctb0010_sf, idx_duplicated, amount)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0029 (need to check this events in the source data)
# Identify duplicated events in ctb0029 dataset
# dataset_titulo = "Carbono e matéria orgânica em amostras do solo do Estado do Rio Grande do Sul por
# diferentes métodos de determinação"
# This dataset (Alessandro Rosa, UFSM) reuses data from other datasets. Also, some events from some
# datasets were recorded with the same coordinates just for being very close to each other. We need
# to check these events in the source document and update the coordinates if possible in the source
# data.
idx_duplicated <- soildata_events[dataset_id == "ctb0029" & duplicated == TRUE, V1]
# Create a spatial object for ctb0029 dataset
ctb0029_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# mapview::mapview(ctb0029_sf)  # Check points in an interactive map
# Transform to UTM
ctb0029_sf <- sf::st_transform(ctb0029_sf, crs = 32720)
# Jitter coordinates
# We jitter the coordinates by 1 meter to separate overlapping points and pass the duplication
# test. But we should check the source document and update the coordinates if possible in the
# source data.
amount <- 1
set.seed(1984) # For reproducibility
ctb0029_sf <- sf::st_jitter(ctb0029_sf, amount = amount)
# Transform back to WGS84
ctb0029_sf <- sf::st_transform(ctb0029_sf, crs = 4326)
# Extract coordinates from the spatial object
ctb0029_sf <- data.table(
  observacao_id = ctb0029_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0029_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0029_sf)[, "Y"]
)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = ctb0029_sf[, coord_x],
  coord_y = ctb0029_sf[, coord_y],
  # In coord_fonte, append " + amount m jitter" to the existing text.
  coord_fonte = paste0(coord_fonte, " + ", amount, " m jitter"),
  # In coord_precisao, add 'amount' to the existing value if it a number larger than 0.
  coord_precisao = ifelse(coord_precisao > 0, coord_precisao + amount, coord_precisao)
)]
rm(ctb0029_sf, idx_duplicated, amount)

# Events located in urban areas. Delete coordinates. We need to check these events in the
# source data in the future.
ctb0029_ids <- c(99, 26, 66, 67, 24, 44, 105, 51, 62)
soildata[dataset_id == "ctb0029" & observacao_id %in% ctb0029_ids, `:=`(
  coord_x = NA_real_,
  coord_y = NA_real_,
  coord_precisao = NA_real_,
  coord_fonte = NA_character_,
  coord_datum = NA_character_
)]
rm(ctb0029_ids)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0033 (need to check this events in the source data)
# Identify duplicated events in ctb0033 dataset. We did not expect duplicated events in this
# dataset anymore.
idx_duplicated <- soildata_events[dataset_id == "ctb0033" & duplicated == TRUE, V1]
# Create a spatial object for ctb0033 dataset
ctb0033_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0033_sf <- sf::st_transform(ctb0033_sf, crs = 32720)
# Jitter coordinates by 1 meter to separate overlapping points and pass the duplication test. But
# we should check the source document and update the coordinates if possible in the source data.
amount <- 1
set.seed(1984) # For reproducibility
ctb0033_sf <- sf::st_jitter(ctb0033_sf, amount = amount)
# Transform back to WGS84
ctb0033_sf <- sf::st_transform(ctb0033_sf, crs = 4326)
# Extract coordinates from the spatial object.
ctb0033_sf <- data.table(
  observacao_id = ctb0033_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0033_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0033_sf)[, "Y"]
)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = ctb0033_sf[, coord_x],
  coord_y = ctb0033_sf[, coord_y],
  # In coord_fonte, append " + amount m jitter" to the existing text.
  coord_fonte = paste0(coord_fonte, " + ", amount, " m jitter"),
  # In coord_precisao, add 'amount' to the existing value if it a number larger than 0.
  coord_precisao = ifelse(coord_precisao > 0, coord_precisao + amount, coord_precisao)
)]
rm(ctb0033_sf, idx_duplicated)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0702 (need to check this events in the source data)
# dataset_titulo = "Dados do 'PROJETO DE GESTÃO AMBIENTAL INTEGRADO DO ESTADO DO AMAZONAS.'"
# It appears that all of the duplicated events in ctb0702 are copies from events in ctb0829.
# dataset_titulo = "Levantamento de reconhecimento dos solos da região sul-sudeste do Estado do
# Amazonas-IPAAM"
# We need to check these events in the source documents. Here, we simply remove the duplicated
# events in ctb0702 and keep the events in ctb0829.
# Identify duplicated events in ctb0702 dataset
idx_duplicated <- soildata_events[dataset_id == "ctb0702" & duplicated == TRUE, V1]
# Remove duplicated events in ctb0702
soildata <- soildata[!(id %in% idx_duplicated & dataset_id == "ctb0702")]
rm(idx_duplicated)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0607 (need to check this events in the source data)
# dataset_titulo = "Levantamento de reconhecimento de média intensidade do solos da região dos
# Tabuleiros Costeiros e da Baixada Litorânea do Estado de Sergipe"
# Apparently, all of the duplicated events in ctb0607 are unique events in the source data. We
# need to check these events in the source documents more carefully in the future. Here, we simply
# jitter the coordinates by a small amount to pass the duplication test.
# Identify duplicated events in ctb0607 dataset
idx_duplicated <- soildata_events[dataset_id == "ctb0607" & duplicated == TRUE, V1]
# Create a spatial object for ctb0607 dataset
ctb0607_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# mapview::mapview(ctb0607_sf)  # Check points in an interactive map
# Transform to UTM
ctb0607_sf <- sf::st_transform(ctb0607_sf, crs = 32720)
# Jitter coordinates
# We jitter the coordinates by a random amount between -1 and 1 meter to separate overlapping
# points and pass the duplication test. But we should check the source document and update the
# coordinates if possible in the source data.
amount <- 1
set.seed(1984) # For reproducibility
ctb0607_sf <- sf::st_jitter(ctb0607_sf, amount = amount)
# Transform back to WGS84
ctb0607_sf <- sf::st_transform(ctb0607_sf, crs = 4326)
# Extract coordinates from the spatial object
ctb0607_sf <- data.table(
  observacao_id = ctb0607_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0607_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0607_sf)[, "Y"]
)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = ctb0607_sf[, coord_x],
  coord_y = ctb0607_sf[, coord_y],
  # In coord_fonte, append " + amount m jitter" to the existing text.
  coord_fonte = paste0(coord_fonte, " + ", amount, " m jitter"),
  # In coord_precisao, add 'amount' to the existing value if it a number larger than 0.
  coord_precisao = ifelse(coord_precisao > 0, coord_precisao + amount, coord_precisao)
)]
rm(ctb0607_sf, idx_duplicated)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0585
# Identify duplicated events in ctb0585 dataset
# dataset_titulo = "Conjunto de dados do 'Solos do Campo Experimental da Embrapa Milho e Sorgo:
# suas características e classificação no novo sistema brasileiro'"
# This dataset (Embrapa Milho e Sorgo, Sete Lagoas, MG) contains the very same central coordinates
# for all events. However, the source document provides relative positions for each event based on
# the central coordinates. We use these relative positions to update the coordinates of each
# event.
# We assume that the original coordinates are in SAD69 (EPSG:4618) and then we transform to UTM zone
# 20S (EPSG:32720) to apply the relative position offsets.
idx_duplicated <- soildata_events[dataset_id == "ctb0585" & duplicated == TRUE, V1]
# Create a spatial object for ctb0585 dataset
# The CRS probably is SAD69
ctb0585_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4618
)
# Transform to UTM
ctb0585_sf <- sf::st_transform(ctb0585_sf, crs = 32720)
ctb0585_sf <- data.table(
  observacao_id = ctb0585_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0585_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0585_sf)[, "Y"]
)
# Perfil-1: A 2.200 metros a W. do meridiano de 44º10' e a 5.300 metros ao S. do paralelo de 19º27'.
ctb0585_sf[observacao_id == "Perfil-1", `:=`(
  coord_x = coord_x - 2200,
  coord_y = coord_y - 5300
)]
# Perfil-10: A 100 metros a W. do meridiano de 44º10? e a 3.300 metros ao S. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-10", `:=`(
  coord_x = coord_x - 100,
  coord_y = coord_y - 3300
)]
# Perfil-11: A 1.800 metros a L. do meridiano de 44º10? e a 2.900 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-11", `:=`(
  coord_x = coord_x + 1800,
  coord_y = coord_y + 2900
)]
# Perfil-12: A 700 metros a L. do meridiano de 44º10? e a 3.100 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-12", `:=`(
  coord_x = coord_x + 700,
  coord_y = coord_y + 3100
)]
# Perfil-13: A 600 metros a W do meridiano de 44º10? e a 1.500 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-13", `:=`(
  coord_x = coord_x - 600,
  coord_y = coord_y + 1500
)]
# Perfil-2: A 2.300 metros a W. do meridiano de 44º10? e a 2.000 metros ao S. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-2", `:=`(
  coord_x = coord_x - 2300,
  coord_y = coord_y - 2000
)]
# Perfil-21: A 800 metros a W do meridiano de 44º10? e a 1.300 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-21", `:=`(
  coord_x = coord_x - 800,
  coord_y = coord_y + 1300
)]
# Perfil-27: A 2000 metros a L do meridiano de 44º10? e a 3.000 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-27", `:=`(
  coord_x = coord_x + 2000,
  coord_y = coord_y + 3000
)]
# Perfil-30: A 100 metros a L do meridiano de 44º10? e a 1.300 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-30", `:=`(
  coord_x = coord_x + 100,
  coord_y = coord_y + 1300
)]
# Perfil-43: A 700 metros a W do meridiano de 44º10? e a 900 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-43", `:=`(
  coord_x = coord_x - 700,
  coord_y = coord_y + 900
)]
# Perfil-7: A 1.200 metros a W. do meridiano de 44º10? e a 200 metros ao S. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-7", `:=`(
  coord_x = coord_x - 1200,
  coord_y = coord_y - 200
)]
# Perfil-8: A 1.000 metros a L. do meridiano de 44º10? e a 1.600 metros ao N. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-8", `:=`(
  coord_x = coord_x + 1000,
  coord_y = coord_y + 1600
)]
# Perfil-9: A 800 metros a W. do meridiano de 44º10? e a 2.700 metros ao S. do paralelo de 19º27?.
ctb0585_sf[observacao_id == "Perfil-9", `:=`(
  coord_x = coord_x - 800,
  coord_y = coord_y - 2700
)]
# Create spatial object again
ctb0585_sf <- sf::st_as_sf(ctb0585_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0585_sf <- sf::st_transform(ctb0585_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(ctb0585_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0585_sf)[, "Y"]
)]
rm(ctb0585_sf, idx_duplicated)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# ctb0631
# Identify duplicated events in ctb0631 dataset
# dataset_titulo = "Levantamento de reconhecimento dos solos do Estado do Paraná - Tomos I e II"
# It is not clear why there are duplicated events in this dataset. We need to check these events
# in the source documents more carefully in the future. Here, we simply jitter the coordinates by
# a small amount to pass the duplication test.
idx_duplicated <- soildata_events[dataset_id == "ctb0631" & duplicated == TRUE, V1]
# Create a spatial object for ctb0631 dataset
ctb0631_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0631_sf <- sf::st_transform(ctb0631_sf, crs = 32720)
# Jitter coordinates
amount <- 1
set.seed(1984) # For reproducibility
ctb0631_sf <- sf::st_jitter(ctb0631_sf, amount = amount)
# Transform back to WGS84
ctb0631_sf <- sf::st_transform(ctb0631_sf, crs = 4326)
# Extract coordinates from the spatial object
ctb0631_sf <- data.table(
  observacao_id = ctb0631_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0631_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0631_sf)[, "Y"]
)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = ctb0631_sf[, coord_x],
  coord_y = ctb0631_sf[, coord_y],
  # In coord_fonte, append " + amount m jitter" to the existing text.
  coord_fonte = paste0(coord_fonte, " + ", amount, " m jitter"),
  # In coord_precisao, add 'amount' to the existing value if it autor_nome a number larger than 0.
  coord_precisao = ifelse(coord_precisao > 0, coord_precisao + amount, coord_precisao)
)]
rm(ctb0631_sf, idx_duplicated, amount)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

# all other datasets
# Identify duplicated events in all other datasets
# We need to check these events in the source documents more carefully in the future. Here, we
# simply jitter the coordinates by a small amount to pass the duplication test.
idx_duplicated <- soildata_events[duplicated == TRUE, V1]
# Create a spatial object for all duplicated datasets
soildata_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
soildata_sf <- sf::st_transform(soildata_sf, crs = 32720)
# Jitter coordinates
amount <- 1
set.seed(1984) # For reproducibility
soildata_sf <- sf::st_jitter(soildata_sf, amount = amount)
# Transform back to WGS84
soildata_sf <- sf::st_transform(soildata_sf, crs = 4326)
# Extract coordinates from the spatial object
soildata_sf <- data.table(
  observacao_id = soildata_sf$observacao_id,
  coord_x = sf::st_coordinates(soildata_sf)[, "X"],
  coord_y = sf::st_coordinates(soildata_sf)[, "Y"]
)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = soildata_sf[, coord_x],
  coord_y = soildata_sf[, coord_y],
  # In coord_fonte, append " + amount m jitter" to the existing text.
  coord_fonte = paste0(coord_fonte, " + ", amount, " m jitter"),
  # In coord_precisao, add 'amount' to the existing value if it author_nome a number larger than 0.
  coord_precisao = ifelse(coord_precisao > 0, coord_precisao + amount, coord_precisao)
)]
rm(soildata_sf, idx_duplicated, amount)

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
soildata_events[
  ,
  duplicated := duplicated(paste0(observacao_id, coord_x, coord_y, data_ano)) |
    duplicated(paste0(coord_x, coord_y, data_ano), fromLast = TRUE)
]
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
# View(soildata_events[duplicated == TRUE, ])

summary_soildata(soildata)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255

# Update coordinates (this has already been implemented in the source spreadsheets)
# MATA ATLÂNTICA
# ctb0691-15
# -20.270007, -40.277173
# mapview::mapview(
#   sf::st_as_sf(soildata[id == "ctb0691-15", ], coords = c("coord_x", "coord_y"), crs = 4326)
# )
# This point is in the airport of Vitoria, ES. We checked Google Maps/Earth and decided to move it
# to a nearby area.
soildata[id == "ctb0691-15", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0691-15", coord_y := -20.270007]
soildata[id == "ctb0691-15", coord_x := -40.277173]
soildata[id == "ctb0691-15", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0691-15", .(id, coord_y, coord_x, coord_fonte)]

# CERRADO
# ctb0617-Perfil-45
# -19.5055229, -47.7914277
# mapview::mapview(
#   sf::st_as_sf(soildata[id == "ctb0617-Perfil-45", ], coords = c("coord_x", "coord_y"), crs = 4326)
# )
soildata[id == "ctb0617-Perfil-45", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0617-Perfil-45", coord_y := -19.5055229]
soildata[id == "ctb0617-Perfil-45", coord_x := -47.7914277]
soildata[id == "ctb0617-Perfil-45", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0617-Perfil-45", .(id, coord_y, coord_x, coord_fonte)]

# ctb0617-Perfil-49
# -19.5042035, -47.7903787
# mapview::mapview(
#   sf::st_as_sf(soildata[id == "ctb0617-Perfil-49", ], coords = c("coord_x", "coord_y"), crs = 4326)
# )
soildata[id == "ctb0617-Perfil-49", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0617-Perfil-49", coord_y := -19.5042035]
soildata[id == "ctb0617-Perfil-49", coord_x := -47.7903787]
soildata[id == "ctb0617-Perfil-49", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0617-Perfil-49", .(id, coord_y, coord_x, coord_fonte)]

# ctb0777-41
# -13.070637, -46.0070019
soildata[id == "ctb0777-41", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0777-41", coord_y := -13.070637]
soildata[id == "ctb0777-41", coord_x := -46.0070019]
soildata[id == "ctb0777-41", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0777-41", .(id, coord_y, coord_x, coord_fonte)]

# ctb0600-TS-8
# -16.6849201, -48.7208003
soildata[id == "ctb0600-TS-8", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0600-TS-8", coord_y := -16.6849201]
soildata[id == "ctb0600-TS-8", coord_x := -48.7208003]
soildata[id == "ctb0600-TS-8", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0600-TS-8", .(id, coord_y, coord_x, coord_fonte)]

# CAATINGA
# ctb0694-49
# -5.3244224, -35.4646402
soildata[id == "ctb0694-49", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0694-49", coord_y := -5.3244224]
soildata[id == "ctb0694-49", coord_x := -35.4646402]
soildata[id == "ctb0694-49", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0694-49", .(id, coord_y, coord_x, coord_fonte)]

# PANTANAL
# ctb0763-169
# -19.052547, -57.6605278
soildata[id == "ctb0763-169", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0763-169", coord_y := -19.052547]
soildata[id == "ctb0763-169", coord_x := -57.6605278]
soildata[id == "ctb0763-169", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0763-169", .(id, coord_y, coord_x, coord_fonte)]

# PAMPA
# ctb0797-RS-113
# # -30.8161997, -53.8114681
soildata[id == "ctb0797-RS-113", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0797-RS-113", coord_y := -30.8161997]
soildata[id == "ctb0797-RS-113", coord_x := -53.8114681]
soildata[id == "ctb0797-RS-113", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0797-RS-113", .(id, coord_y, coord_x, coord_fonte)]

# OTHER
# ctb0607-PERFIL-92
# -10.7552957, -37.0623882
soildata[id == "ctb0607-PERFIL-92", .(id, coord_y, coord_x, coord_fonte)]
soildata[id == "ctb0607-PERFIL-92", coord_y := -10.7552957]
soildata[id == "ctb0607-PERFIL-92", coord_x := -37.0623882]
soildata[id == "ctb0607-PERFIL-92", coord_fonte := "Google Maps (curadoria)"]
soildata[id == "ctb0607-PERFIL-92", .(id, coord_y, coord_x, coord_fonte)]

# Check intersection with the Brazilian territory 'brazil sf' object
soildata_sf <- sf::st_as_sf(soildata[!is.na(coord_x) & !is.na(coord_y)],
  coords = c("coord_x", "coord_y"), crs = 4326
)
brazil <- sf::st_transform(brazil, crs = 4326)
soildata_sf <- sf::st_join(soildata_sf, brazil["code_state"], left = TRUE)
nrow(soildata_sf[is.na(soildata_sf$code_state), ])
# 136
# View(soildata_sf[is.na(soildata_sf$code_state), ])
# ctb0020 is outside Brazil. This happened because the latitude is positive when it should be
# negative. We need to correct this event in the source data.
# mapview::mapview(soildata_sf[is.na(soildata_sf$code_state), ])
# Correct the coordinates if necessary
soildata[dataset_id == "ctb0020", coord_y := ifelse(coord_y > 0, coord_y * -1, coord_y)]
soildata[dataset_id == "ctb0020", .(id, coord_y, coord_x, coord_fonte)]
soildata_sf <- sf::st_as_sf(soildata[!is.na(coord_x) & !is.na(coord_y)],
  coords = c("coord_x", "coord_y"), crs = 4326
)
soildata_sf <- sf::st_join(soildata_sf, brazil["code_state"], left = TRUE)
nrow(soildata_sf[is.na(soildata_sf$code_state), ])
# 0

# THIS HAS ALREADY BEEN CORRECTED IN THE ORIGINAL DATASET. WE KEEP IT HERE FOR REFERENCE.
soildata[
  dataset_id == "ctb0607" & observacao_id == "PERFIL-92",
  carbono := ifelse(carbono == 413, 41.3, carbono)
]

# THIS HAS ALREADY BEEN CORRECTED IN THE ORIGINAL DATASET. WE KEEP IT HERE FOR REFERENCE.
# ctb0718-51. carbon is recorded as 145 g/kg. It is corrected to 14.5 g/kg.
soildata[
  dataset_id == "ctb0718" & observacao_id == "51",
  carbono := ifelse(carbono == 145, 14.5, carbono)
]

# FIGURE 15.1
# Check spatial distribution after cleaning layers
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot spatial distribution
file_path <- fig_path("151_spatial_distribution_after_cleaning_layers.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after cleaning layers"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255
data.table::fwrite(soildata, "data/15_soildata.txt", sep = "\t")

# title: SoilData Integration
# subtitle: Clean data
# author: Alessandro Rosa
# date: 2026
# licence: MIT
# description: This script reconciles known overlap between datasets in the
# integrated Brazilian Soil Dataset produced by script 13. It removes records
# duplicated across datasets and known duplicate datasets, plots the resulting
# spatial distribution, and exports the dataset-cleaned result to
# data/14_soildata.txt. Layer-level cleaning is performed by script 15.
rm(list = ls())

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Load required packages
library(data.table)
library(sf)

# Read Brazilian state boundaries
brazil <- read_brazil_states()

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/13_soildata.txt", sep = "\t")
summary_soildata(soildata)
# 2026 ---
# Layers: 64033
# Events: 20392
# Georeference: 16852 (yes) / 3540 (no)
# Date: 20237 (yes) / 155 (no)
# Datasets: 274
# 2025 ---
# Layers: 61145
# Events: 18537
# Georeferenced events: 14995
# Datasets: 263

# ctb0002 and ctb0838
# Some records in the ctb0002 dataset are duplicated in the ctb0838 dataset.
# They have about the same coordinates (coord_x, coord_y) and supposedly the
# same soil classification (taxon_sibcs). These data come from the same
# source/author (Elias Mendes da Costa) and thus are known duplicates.
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "taxon_sibcs")
ctb0002 <- unique(soildata[dataset_id == "ctb0002", ..cols])
ctb0002 <- ctb0002[complete.cases(coord_x, coord_y, taxon_sibcs)]
ctb0002[, coord_x := round(coord_x, 4)]
ctb0002[, coord_y := round(coord_y, 4)]
ctb0838 <- unique(soildata[dataset_id == "ctb0838", ..cols])
ctb0838 <- ctb0838[complete.cases(coord_x, coord_y, taxon_sibcs)]
ctb0838[, coord_x := round(coord_x, 4)]
ctb0838[, coord_y := round(coord_y, 4)]
# Check for duplicates
duplicates <- ctb0002[ctb0838,
  on = .(coord_x, coord_y, taxon_sibcs), nomatch = 0
]
duplicates_idx <- unique(duplicates[, observacao_id])
if (length(duplicates_idx) > 0L) {
  message(
    "Found ", length(duplicates_idx),
    " duplicated records in ctb0002 and ctb0838."
  )
} else {
  message("No duplicated records found in ctb0002 and ctb0838.")
}
# Drop dataset_id = ctb0002 duplicates from soildata
soildata <- soildata[!(dataset_id == "ctb0002" & observacao_id %in% duplicates_idx)]
summary_soildata(soildata)
# Layers: 64018
# Events: 20377
# Georeference: 16837 (yes) / 3540 (no)
# Date: 20222 (yes) / 155 (no)
# Datasets: 274

# ctb0029
# Carbono e matéria orgânica em amostras do solo do Estado do Rio Grande do Sul
# por diferentes métodos de determinação
# Some of the samples come from ctb0012. Those samples meet the following
# criteria: municipio_id == "Silveira Martins" & amostra_tipo == "SIMPLES"
# Filter out samples in ctb0029 that are also in ctb0012
dup <- soildata[(
  dataset_id == "ctb0029" & municipio_id == "Silveira Martins" &
    amostra_tipo == "SIMPLES"), ]
if (nrow(dup) > 0L) {
  message(
    "Found ", nrow(dup),
    " duplicated records in ctb0029 and ctb0012."
  )
} else {
  message("No duplicated records found in ctb0029 and ctb0012.")
}
soildata <- soildata[!(
  dataset_id == "ctb0029" & municipio_id == "Silveira Martins" &
    amostra_tipo == "SIMPLES"), ]
summary_soildata(soildata)
# Layers: 64014
# Events: 20373
# Georeference: 16833 (yes) / 3540 (no)
# Date: 20218 (yes) / 155 (no)
# Datasets: 274

# ctb0654 (exact duplicate of ctb0608)
# Conjunto de dados do 'V Reunião de Classificação, Correlação e Aplicação de
# Levantamentos de Solo - guia de excursão de estudos de solos nos Estados de
# Pernambuco, Paraíba, Rio Grande do Norte, Ceará e Bahia'
# These datasets are exact duplicates. We remove ctb0654.
soildata <- soildata[dataset_id != "ctb0654", ]
summary_soildata(soildata)
# Layers: 63906
# Events: 20353
# Georeference: 16814 (yes) / 3539 (no)
# Date: 20198 (yes) / 155 (no)
# Datasets: 273

# ctb0800 (many duplicates of ctb0702)
# Estudos pedológicos e suas relações ambientais
# Ideally we should check each duplicated event to decide which one to keep. But
# this is a time-consuming task. So we just remove all records from ctb0800. 
# These data need to be checked in the future.
soildata <- soildata[dataset_id != "ctb0800", ]
summary_soildata(soildata)
# Layers: 63661
# Events: 20309
# Georeference: 16770 (yes) / 3539 (no)
# Date: 20154 (yes) / 155 (no)
# Datasets: 272

# ctb0808 (exact duplicate of ctb0574)
# Conjunto de dados do levantamento semidetalhado 'Levantamento Semidetalhado e
# Aptidão Agrícola dos Solos do Município do Rio de Janeiro, RJ.'
# These datasets are exact duplicates. We remove ctb0808.
soildata <- soildata[dataset_id != "ctb0808", ]
summary_soildata(soildata)
# Layers: 63320
# Events: 20249
# Georeference: 16710 (yes) / 3539 (no)
# Date: 20094 (yes) / 155 (no)
# Datasets: 271

# FIGURE 14.1
# Check spatial distribution after cleaning datasets
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot spatial distribution
file_path <- fig_path("141_spatial_distribution_after_cleaning_datasets.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after cleaning datasets"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Write data to disk ###########################################################
summary_soildata(soildata)
# 2026 ---
# Layers: 63320
# Events: 20251
# Georeference: 16710 (yes) / 3541 (no)
# Date: 20094 (yes) / 157 (no)
# Datasets: 271
data.table::fwrite(soildata, "data/14_soildata.txt", sep = "\t")

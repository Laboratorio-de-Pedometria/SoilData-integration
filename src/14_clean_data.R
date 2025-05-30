# title: SoilData Integration
# subtitle: Clean data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
# licence: MIT
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}

# Source helper functions
source("src/00_helper_functions.R")

# Download Brazilian state boundaries
# Check if the file already exists to avoid re-downloading
if (!file.exists("data/brazil_states.geojson")) {
  brazil <- geobr::read_state()
  # Save the data to a file for future use
  sf::st_write(brazil, "data/brazil_states.geojson", delete_dsn = TRUE)
} else {
  brazil <- sf::st_read("data/brazil_states.geojson")
}

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/13_soildata.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 61145
# Events: 18537
# Georeferenced events: 14995

# Clean datasets

# ctb0002 and ctb0838
# Some records in the ctb0002 dataset are duplicated in the ctb0838 dataset. They have about the
# same coordinates (coord_x, coord_y) and supposedly the same soil classification (taxon_sibcs).
cols <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "taxon_sibcs")
ctb0002 <- soildata[dataset_id == "ctb0002", ..cols]
ctb0002[, coord_x := round(coord_x, 4)]
ctb0002[, coord_y := round(coord_y, 4)]
ctb0838 <- unique(soildata[dataset_id == "ctb0838", ..cols])
ctb0838[, coord_x := round(coord_x, 4)]
ctb0838[, coord_y := round(coord_y, 4)]
# Check for duplicates
duplicates <- ctb0002[ctb0838, on = .(coord_x, coord_y, taxon_sibcs), nomatch = 0]
duplicates_idx <- duplicates[, observacao_id]
# Drop dataset_id = ctb0002 duplicates from soildata
soildata <- soildata[!(dataset_id == "ctb0002" & observacao_id %in% duplicates_idx)]
summary_soildata(soildata)
# Layers: 61130
# Events: 18522
# Georeferenced events: 14980

# ctb0029
# Carbono e matéria orgânica em amostras do solo do Estado do Rio Grande do Sul por diferentes
# métodos de determinação
# Some of the samples come from ctb0012. Those samples meet the following criteria:
# municipio_id == "Silveira Martins" & amostra_tipo == "SIMPLES" 
# Filter out samples in ctb0029 that are also in ctb0012
soildata <- soildata[!(dataset_id == "ctb0029" & municipio_id == "Silveira Martins" &
  amostra_tipo == "SIMPLES"), ]
summary_soildata(soildata)
# Layers: 61126
# Events: 18518
# Georeferenced events: 14976

# ctb0654 (exact duplicate of ctb0608)
# Conjunto de dados do 'V Reunião de Classificação, Correlação e Aplicação de Levantamentos de Solo
#  - guia de excursão de estudos de solos nos Estados de Pernambuco, Paraíba, Rio Grande do Norte,
# Ceará e Bahia'
soildata <- soildata[dataset_id != "ctb0654", ]
summary_soildata(soildata)
# Layers: 61018
# Events: 18498
# Georeferenced events: 14957

# ctb0800 (many duplicates of ctb0702)
# Estudos pedológicos e suas relações ambientais
soildata <- soildata[dataset_id != "ctb0800", ]
summary_soildata(soildata)
# Layers: 60773
# Events: 18454
# Georeferenced events: 14913

# ctb0808 (exact duplicate of ctb0574)
# Conjunto de dados do levantamento semidetalhado 'Levantamento Semidetalhado e Aptidão Agrícola dos
# Solos do Município do Rio de Janeiro, RJ.'
soildata <- soildata[dataset_id != "ctb0808", ]
summary_soildata(soildata)
# Layers: 60432
# Events: 18394
# Georeferenced events: 14853

# LAYER ORDER
soildata <- soildata[order(id, profund_sup, profund_inf)]

# Correct layer names (if necessary)
soildata[camada_nome == "", camada_nome := NA_character_]
soildata[
  id == "ctb0770-100" & camada_nome == "B21H",
  camada_nome := ifelse(camada_nome == "B21H", "B21h", camada_nome)
]
soildata[
  id == "ctb0636-Perfil-03" & profund_sup == 0,
  camada_nome := ifelse(camada_nome == "Ao", "A1", camada_nome)
]

# Incorrect depth limits: profund_sup > profund_inf
cols <- c("id", "camada_nome", "profund_sup", "profund_inf")
soildata[profund_sup > profund_inf, ..cols]
soildata[id == "ctb0033-RO1154" & profund_sup == 80, `:=` (
  profund_sup = 70,
  profund_inf = 80
)]
soildata[id == "ctb0033-RO2463" & profund_sup == 80, `:=` (
  profund_sup = 70,
  profund_inf = 80
)]
soildata[id == "ctb0033-RO2826" & profund_sup == 140, `:=` (
  profund_sup = 140,
  profund_inf = 160
)]
soildata[id == "ctb0033-RO3542" & profund_sup == 110, `:=`(
  profund_sup = 110,
  profund_inf = 120
)]

# Correct negative (profund_sup < 0) depth limit of topsoil layers
# Check each soil profile (id) for negative depth limits. Store the result in a new column
# "negative_depth" (TRUE/FALSE). If a profile has negative depth limits, add the absolute value of
# the negative depth limit to the depth limits (profund_sup and profund_inf) of all layers of that
# profile.
negative_depths <- soildata[, .(min_depth = min(profund_sup)), by = id][min_depth < 0]
print(negative_depths)
if (nrow(negative_depths) > 0) {
  soildata[negative_depths, on = "id", `:=` (
    profund_sup = profund_sup + abs(i.min_depth),
    profund_inf = profund_inf + abs(i.min_depth)
    )
  ]
}
rm(negative_depths)

# Some layers have equal values for profund_sup and profund_inf.
# This may occur when the soil profile sampling and description ended at the top of the layer,
# producing a censoring effect. If the layer has a name containing R, D, or C, we add a fixed depth
# (plus_depth).
nrow(soildata[profund_sup == profund_inf])
# 222 layers
soildata[, equal_depth := any(profund_sup == profund_inf), by = id]
print(soildata[equal_depth == TRUE, ..cols])
# Add a fixed depth to R, D, and C layers with equal depth limits
plus_depth <- 20
soildata[
  profund_sup == profund_inf & grepl("R|D|C", camada_nome),
  profund_inf := profund_inf + plus_depth
]
nrow(soildata[profund_sup == profund_inf])
# 67 layers
soildata[, equal_depth := any(profund_sup == profund_inf), by = id]
# View(soildata[equal_depth == TRUE, ..cols])
# Some events from dataset_id = ctb0033 have a single layer and the depth limit is equal to zero.
# We remove these layers.
soildata[, n_layers := .N, by = id]
soildata[
  dataset_id == "ctb0033" & profund_sup == profund_inf & profund_sup == 0 & n_layers == 1,
  .(id, camada_nome, profund_sup, profund_inf)
]
soildata <- soildata[
  !(dataset_id == "ctb0033" & profund_sup == profund_inf & profund_sup == 0 & n_layers == 1)
]
nrow(soildata[profund_sup == profund_inf])
# 59 layers
soildata[, n_layers := NULL]
print(soildata[equal_depth == TRUE, ..cols])
# Some events with profund_sup == profund_inf and profund_sup == 0 are from ctb0631.
# Actually, these layers have not a depth limit recorded. So we set them to NA.
soildata[
  dataset_id == "ctb0631" & profund_sup == profund_inf & profund_sup == 0,
  .(id, camada_nome, profund_sup, profund_inf, carbono)
]
soildata <- soildata[!(dataset_id == "ctb0631" & profund_sup == profund_inf & profund_sup == 0)]
nrow(soildata[profund_sup == profund_inf])
# 36 layers
print(soildata[equal_depth == TRUE, ..cols])
# For some datasets, we add a fixed depth to the lowermost layer
# ctb0691, ctb0787, ctb0675, ctb0603, ctb0645, ctb0033, ctb0678, ctb0691
soildata[
  dataset_id == "ctb0691" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0787" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0675" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0603" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0645" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0033" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0678" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  dataset_id == "ctb0691" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  id == "ctb0662-P55" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
soildata[
  id == "ctb0717-38" & profund_sup == profund_inf, profund_inf := profund_inf + plus_depth
]
# Some layers have equal values for profund_sup and profund_inf, but they are not R, D, or C layers.
soildata <- soildata[!(id == "ctb0809-Exame-8" & profund_sup == profund_inf)]
# Check
nrow(soildata[profund_sup == profund_inf])
# 15 layers
soildata[, equal_depth := any(profund_sup == profund_inf), by = id]
print(soildata[equal_depth == TRUE, ..cols])
# For some datasets, we add a fixed depth to the uppermost layer
soildata[id == "ctb0775-9" & camada_nome == "B21" & profund_sup == 150 & profund_inf == 150, `:=`(
  profund_sup = 100,
  profund_inf = 150
)]
soildata[, equal_depth := any(profund_sup == profund_inf), by = id]
# View(soildata[equal_depth == TRUE, ..cols])
# Drop all of the remaining layers with equal depth limits
soildata <- soildata[equal_depth == FALSE]
nrow(soildata[profund_sup == profund_inf])
# 0 layers
soildata[, equal_depth := NULL]
summary_soildata(soildata)
# Layers: 57891
# Events: 16868
# Georeferenced events: 14388

# Layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
# Update the columns camada_id
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Remove repeated layers
# Some layers are repeated in the same event (id). These layers have equal values for camada_nome,
# profund_sup, and profund_inf. We create a new variable called repeated to identify these layers.
# Then, we filter out these layers.
soildata[,
  repeated := duplicated(camada_nome) & duplicated(profund_sup) & duplicated(profund_inf),
  by = id
]
print(soildata[repeated == TRUE, .(id, camada_nome, profund_sup, profund_inf, carbono)])
# ATTENTION: REPEATED LAYERS IN DATA FROM THE NATIONAL FOREST INVENTORY HAVE DIFFERENT SOIL
# PROPERTY VALUES. THIS IS A PROBLEM THAT NEEDS TO BE SOLVED IN THE FUTURE!
print(soildata[id == "ctb0055-PR_4", .(id, camada_nome, profund_sup, profund_inf, carbono)])
soildata <- soildata[repeated == FALSE, ]
soildata[, repeated := NULL]
summary_soildata(soildata)
# Layers: 57327
# Events: 16868
# Georeferenced events: 14387

# Update layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Fine earth
# R layers are consolidated rock layers. These layers should have terrafina == NA_real.
# Correct samples with terrafina == 0 g/kg
# When terrafina == 0, we set the fine earth content to 1000 g/kg.
soildata[terrafina == 0, .N]
# 23 samples with terrafina == 0
cols <- c("id", "camada_nome", "profund_sup", "profund_inf", "terrafina", "argila", "taxon_sibcs")
print(soildata[terrafina == 0, ..cols])
# If camada_nome != "R", set terrafina to 1000 g/kg
soildata[camada_nome == "R", terrafina := NA_real_]
soildata[camada_nome == "2R", terrafina := NA_real_]
soildata[camada_nome == "IIR", terrafina := NA_real_]
soildata[terrafina == 0, terrafina := 1000]

# Fine earth content
soildata[, esqueleto := 1000 - terrafina]
# Check samples with esqueleto > 800
print(soildata[esqueleto > 800, .N])
# 133 sample with esqueleto > 800
# Correct soil skeleton and fine earth content
soildata[id == "ctb0565-Perfil-08" & camada_nome == "BC1", `:=`(
  esqueleto = 0,
  terrafina = 1000
)]
# There datasets have been checks
ctb_ok <- c(
  "ctb0006", "ctb0011", "ctb0017", "ctb0025", "ctb0033", "ctb0038", "ctb0044", "ctb0562",
  "ctb0600", "ctb0605", "ctb0606"
)
cols <- c("id", "camada_nome", "profund_sup", "profund_inf", "esqueleto", "terrafina")
# View(soildata[!(dataset_id %in% ctb_ok) & esqueleto > 800, ..cols])
# Filter out samples with skeleton > 1000.
# Some layers have esqueleto > 1000. This is not possible.
soildata <- soildata[is.na(esqueleto) | esqueleto < 1000]
summary_soildata(soildata)
# Layers: 57326
# Events: 16868
# Georeferenced events: 14387

# Clean camada_nome
soildata[, camada_nome := as.character(camada_nome)]
soildata[is.na(camada_nome) | camada_nome == "" & profund_sup == 0, camada_nome := "A"]
soildata[is.na(camada_nome) | camada_nome == "" & profund_sup != 0, camada_nome := NA_character_]
soildata[, camada_nome := gsub("p1", "pl", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("Çg", "Cg", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("0", "O", camada_nome, ignore.case = FALSE)]
soildata[grepl(",OOE+O", camada_nome, fixed = TRUE), camada_nome := NA_character_]
sort(unique(soildata[, camada_nome]))

# Particle size distribution
# Transform the particle size fractions from g/kg to %. Then check if their sum is 100%
soildata[, argila := round(argila / 10)]
soildata[, silte := round(silte / 10)]
soildata[, areia := round(areia / 10)]
soildata[, psd := round(argila + silte + areia)]
# Correct the particle size fractions
soildata[
  id == "ctb0591-P-13-Sao-Mateus-do-Sul" & camada_nome == "BW1" & argila == 37, `:=` (
    argila = 100 - 16 - 9,
    silte = 16,
    areia = 9
  )
]
soildata[
  id == "ctb0591-P-13-Sao-Mateus-do-Sul" & camada_nome == "BW2" & argila == 0, `:=` (
    argila = 100 - 14 - 10,
    silte = 14,
    areia = 10
  )
]
soildata[
  id == "ctb0591-P-13-Sao-Mateus-do-Sul" & camada_nome == "BW3" & argila == 4, `:=` (
    argila = 100 - 13 - 10,
    silte = 13,
    areia = 10
  )
]
soildata[
  id == "ctb0620-Á-de-Chapecó-3" & camada_nome == "Ap" & argila == 0, `:=`(
    argila = 100 - 36 - 2,
    silte = 36,
    areia = 2
  )
]
soildata[
  id == "ctb0646-PERFIL-20" & camada_nome == "C2" & argila == 0, `:=` (
    argila = NA_real_,
    silte = NA_real_,
    areia = NA_real_
  )
]
# Check if there is any size fraction equal to 0
# clay
ctb_zero_clay <- c(
  "ctb0607", "ctb0656", "ctb0666", "ctb0679", "ctb0020", "ctb0691", "ctb0695", "ctb0698",
  "ctb0705"
)
soildata[argila == 0 & !dataset_id %in% ctb_zero_clay, .N]
# 23 layers (they need to be checked)
# Print the layers with clay == 0
cols <- c("id", "camada_nome", "argila", "silte", "areia")
soildata[argila == 0 & !dataset_id %in% ctb_zero_clay, ..cols]
# silt
soildata[silte == 0, .N]
# 85 layers (they need to be checked)
# Print the layers with silt == 0
soildata[silte == 0, ..cols]
# sand
soildata[areia == 0, .N]
# 138 layers (they need to be checked)
# Print the layers with sand == 0
soildata[areia == 0, ..cols]

# Check if the sum of the three fractions is 100%
soildata[psd != 100, .N]
# 2242 layers
psd_lims <- 90:110
soildata[!is.na(psd) & !(psd %in% psd_lims), .N]
# 2 layers
cols <- c("id", "camada_nome", "argila", "silte", "areia", "psd")
soildata[!is.na(psd) & !(psd %in% psd_lims), ..cols]
# If the sum of the three fractions is different from 100%, adjust their values, adding the
# difference to the silt fraction.
soildata[psd != 100, argila := round(argila / psd * 100)]
soildata[psd != 100, areia := round(areia / psd * 100)]
soildata[psd != 100, silte := 100 - argila - areia]
soildata[, psd := round(argila + silte + areia)]
soildata[psd != 100, psd]
soildata[, psd := NULL]

# Correct bulk density values
soildata[id == "ctb0562-Perfil-13" & camada_id == 2, dsi := ifelse(dsi == 2.6, 0.86, dsi)]
soildata[id == "ctb0562-Perfil-14" & camada_id == 1, dsi := ifelse(dsi == 2.53, 1.09, dsi)]
soildata[id == "ctb0562-Perfil-14" & camada_id == 2, dsi := ifelse(dsi == 2.6, 0.9, dsi)]
soildata[id == "ctb0608-15-V-RCC" & camada_id == 3, dsi := ifelse(dsi == 0.42, 1.94, dsi)]
soildata[id == "ctb0631-Perfil-17" & camada_id == 3, dsi := ifelse(dsi == 0.14, 1.1, dsi)]
soildata[id == "ctb0700-15" & camada_id == 1, dsi := ifelse(dsi == 2.53, 1.6, dsi)]
soildata[id == "ctb0700-15" & camada_id == 2, dsi := ifelse(dsi == 2.56, 1.49, dsi)]
soildata[id == "ctb0771-26" & camada_id == 1, dsi := ifelse(dsi == 2.59, 1.32, dsi)]
soildata[id == "ctb0771-26" & camada_id == 2, dsi := ifelse(dsi == 2.56, 1.37, dsi)]
soildata[id == "ctb0777-1" & camada_id == 1, dsi := ifelse(dsi == 2.65, 1.35, dsi)]
soildata[id == "ctb0787-1" & camada_id == 2, dsi := ifelse(dsi == 2.58, 1.35, dsi)]
soildata[id == "ctb0787-4" & camada_id == 1, dsi := ifelse(dsi == 2.35, 1.35, dsi)]
soildata[id == "ctb0787-4" & camada_id == 2, dsi := ifelse(dsi == 1.3, 1.27, dsi)]
soildata[id == "ctb0811-2" & camada_id == 3, dsi := ifelse(dsi == 0.34, 1.64, dsi)]
soildata[id == "ctb0702-P-46" & camada_id == 1, dsi := ifelse(dsi == 2.08, 1.08, dsi)] # check document
soildata[id == "ctb0572-Perfil-063" & camada_id == 2, dsi := ifelse(dsi == 0.34, 1.84, dsi)]
soildata[id == "ctb0605-P-06" & camada_id == 2, dsi := ifelse(dsi == 0.31, 1.32, dsi)]
summary_soildata(soildata)
# Layers: 57326
# Events: 16868
# Georeferenced events: 14387

# Clean events
# Correct date (there is a typo in the spreadsheet)
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
idx_duplicated <- soildata_events[dataset_id == "ctb0010" & duplicated == TRUE, V1]
# Create a spatial object for ctb0010 dataset
ctb0010_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0010_sf <- sf::st_transform(ctb0010_sf, crs = 32720)
ctb0010_sf <- data.table(
  observacao_id = ctb0010_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0010_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0010_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0010_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
ctb0010_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
ctb0010_sf <- sf::st_as_sf(ctb0010_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0010_sf <- sf::st_transform(ctb0010_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(ctb0010_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0010_sf)[, "Y"]
)]
rm(ctb0010_sf, idx_duplicated)

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
idx_duplicated <- soildata_events[dataset_id == "ctb0029" & duplicated == TRUE, V1]

# Create a spatial object for ctb0029 dataset
ctb0029_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0029_sf <- sf::st_transform(ctb0029_sf, crs = 32720)
ctb0029_sf <- data.table(
  observacao_id = ctb0029_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0029_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0029_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0029_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
ctb0029_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
ctb0029_sf <- sf::st_as_sf(ctb0029_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0029_sf <- sf::st_transform(ctb0029_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(ctb0029_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0029_sf)[, "Y"]
)]
rm(ctb0029_sf, idx_duplicated)
# Events located in urban areas. Delete coordinates.
ctb0029_ids <- c(99, 26, 66, 67, 24, 44, 105, 51, 62)
soildata[dataset_id == "ctb0029" & observacao_id %in% ctb0029_ids, `:=`(
  coord_x = NA_real_,
  coord_y = NA_real_,
  coord_precision = NA_real_,
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
# Identify duplicated events in ctb0033 dataset
idx_duplicated <- soildata_events[dataset_id == "ctb0033" & duplicated == TRUE, V1]
# Create a spatial object for ctb0033 dataset
ctb0033_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0033_sf <- sf::st_transform(ctb0033_sf, crs = 32720)
ctb0033_sf <- data.table(
  observacao_id = ctb0033_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0033_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0033_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0033_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
ctb0033_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
ctb0033_sf <- sf::st_as_sf(ctb0033_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0033_sf <- sf::st_transform(ctb0033_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(ctb0033_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0033_sf)[, "Y"]
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
# It appears that all of the duplicated events in ctb0702 are copies from events in ctb0829.
# We will remove the duplicated events in ctb0702 and keep the events in ctb0829.
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
# Apparently, all of the duplicated events in ctb0607 are unique events in the source data.
# Identify duplicated events in ctb0607 dataset
idx_duplicated <- soildata_events[dataset_id == "ctb0607" & duplicated == TRUE, V1]
# Create a spatial object for ctb0607 dataset
ctb0607_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0607_sf <- sf::st_transform(ctb0607_sf, crs = 32720)
ctb0607_sf <- data.table(
  observacao_id = ctb0607_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0607_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0607_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0607_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
ctb0607_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
ctb0607_sf <- sf::st_as_sf(ctb0607_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0607_sf <- sf::st_transform(ctb0607_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(ctb0607_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0607_sf)[, "Y"]
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
idx_duplicated <- soildata_events[dataset_id == "ctb0631" & duplicated == TRUE, V1]
# Create a spatial object for ctb0631 dataset
ctb0631_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0631_sf <- sf::st_transform(ctb0631_sf, crs = 32720)
ctb0631_sf <- data.table(
  observacao_id = ctb0631_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0631_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0631_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0631_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
ctb0631_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
ctb0631_sf <- sf::st_as_sf(ctb0631_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0631_sf <- sf::st_transform(ctb0631_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(ctb0631_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0631_sf)[, "Y"]
)]
rm(ctb0631_sf, idx_duplicated)

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
# Identify duplicated events in ctb0832 dataset
idx_duplicated <- soildata_events[duplicated == TRUE, V1]
# Create a spatial object for all duplicated datasets
soildata_sf <- sf::st_as_sf(soildata[id %in% idx_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
soildata_sf <- sf::st_transform(soildata_sf, crs = 32720)
soildata_sf <- data.table(
  dataset_id = soildata_sf$dataset_id,
  observacao_id = soildata_sf$observacao_id,
  coord_x = sf::st_coordinates(soildata_sf)[, "X"],
  coord_y = sf::st_coordinates(soildata_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
soildata_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
soildata_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
soildata_sf <- sf::st_transform(soildata_sf, crs = 4326)
# Update coordinates in soildata
soildata[id %in% idx_duplicated, `:=`(
  coord_x = sf::st_coordinates(soildata_sf)[, "X"],
  coord_y = sf::st_coordinates(soildata_sf)[, "Y"]
)]
rm(soildata_sf, idx_duplicated)

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
# Layers: 57079
# Events: 16824
# Georeferenced events: 14334

# Update coordinates (this has already been implemented in the source spreadsheets)
# MATA ATLÂNTICA
# ctb0691-15
# -20.270007, -40.277173
soildata[id == "ctb0691-15", .(id, coord_y, coord_x)]
soildata[id == "ctb0691-15", coord_y := -20.270007]
soildata[id == "ctb0691-15", coord_x := -40.277173]
soildata[id == "ctb0691-15", .(id, coord_y, coord_x)]

# CERRADO
# ctb0617-Perfil-45
# -19.5055229, -47.7914277
soildata[id == "ctb0617-Perfil-45", .(id, coord_y, coord_x)]
soildata[id == "ctb0617-Perfil-45", coord_y := -19.5055229]
soildata[id == "ctb0617-Perfil-45", coord_x := -47.7914277]
soildata[id == "ctb0617-Perfil-45", .(id, coord_y, coord_x)]

# ctb0617-Perfil-49
# -19.5042035, -47.7903787
soildata[id == "ctb0617-Perfil-49", .(id, coord_y, coord_x)]
soildata[id == "ctb0617-Perfil-49", coord_y := -19.5042035]
soildata[id == "ctb0617-Perfil-49", coord_x := -47.7903787]
soildata[id == "ctb0617-Perfil-49", .(id, coord_y, coord_x)]

# ctb0777-41
# -13.070637, -46.0070019
soildata[id == "ctb0777-41", .(id, coord_y, coord_x)]
soildata[id == "ctb0777-41", coord_y := -13.070637]
soildata[id == "ctb0777-41", coord_x := -46.0070019]
soildata[id == "ctb0777-41", .(id, coord_y, coord_x)]

# ctb0600-TS-8
# -16.6849201, -48.7208003
soildata[id == "ctb0600-TS-8", .(id, coord_y, coord_x)]
soildata[id == "ctb0600-TS-8", coord_y := -16.6849201]
soildata[id == "ctb0600-TS-8", coord_x := -48.7208003]
soildata[id == "ctb0600-TS-8", .(id, coord_y, coord_x)]

# CAATINGA
# ctb0694-49
# -5.3244224, -35.4646402
soildata[id == "ctb0694-49", .(id, coord_y, coord_x)]
soildata[id == "ctb0694-49", coord_y := -5.3244224]
soildata[id == "ctb0694-49", coord_x := -35.4646402]
soildata[id == "ctb0694-49", .(id, coord_y, coord_x)]

# PANTANAL
# ctb0763-169
# -19.052547, -57.6605278
soildata[id == "ctb0763-169", .(id, coord_y, coord_x)]
soildata[id == "ctb0763-169", coord_y := -19.052547]
soildata[id == "ctb0763-169", coord_x := -57.6605278]
soildata[id == "ctb0763-169", .(id, coord_y, coord_x)]

# PAMPA
# ctb0797-RS-113
# # -30.8161997, -53.8114681
soildata[id == "ctb0797-RS-113", .(id, coord_y, coord_x)]
soildata[id == "ctb0797-RS-113", coord_y := -30.8161997]
soildata[id == "ctb0797-RS-113", coord_x := -53.8114681]
soildata[id == "ctb0797-RS-113", .(id, coord_y, coord_x)]

# OTHER
# ctb0607-PERFIL-92
# -10.7552957, -37.0623882
soildata[id == "ctb0607-PERFIL-92", .(id, coord_y, coord_x)]
soildata[id == "ctb0607-PERFIL-92", coord_y := -10.7552957]
soildata[id == "ctb0607-PERFIL-92", coord_x := -37.0623882]
soildata[id == "ctb0607-PERFIL-92", .(id, coord_y, coord_x)]

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

# FIGURE 14.1
# Check spatial distribution after cleaning data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
file_path <- "res/fig/141_spatial_distribution_after_cleaning_data.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after cleaning data"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Clean soil bulk density data
# Delete possible inconsistent values
soildata[dsi > 2.5, dsi := NA_real_]

# Correct inconsistent soil bulk density values
soildata[id == "ctb0058-RN_20", dsi := ifelse(dsi == 2.11, 1.11, dsi)]

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 57079
# Events: 16824
# Georeferenced events: 14334
# Export cleaned data
data.table::fwrite(soildata, "data/14_soildata.txt", sep = "\t")

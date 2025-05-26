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

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/13_soildata.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 61009
# Events: 18537
# Georeferenced events: 14994

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
# Layers: 60994
# Events: 18522
# Georeferenced events: 14979

# ctb0029
# Carbono e matéria orgânica em amostras do solo do Estado do Rio Grande do Sul por diferentes
# métodos de determinação
# Some of the samples come from ctb0012. Those samples meet the following criteria:
# municipio_id == "Silveira Martins" & amostra_tipo == "SIMPLES" 
# Filter out samples in ctb0029 that are also in ctb0012
soildata <- soildata[!(dataset_id == "ctb0029" & municipio_id == "Silveira Martins" &
  amostra_tipo == "SIMPLES"), ]
summary_soildata(soildata)
# Layers: 60990
# Events: 18518
# Georeferenced events: 14975

# ctb0654 (exact duplicate of ctb0608)
# Conjunto de dados do 'V Reunião de Classificação, Correlação e Aplicação de Levantamentos de Solo
#  - guia de excursão de estudos de solos nos Estados de Pernambuco, Paraíba, Rio Grande do Norte,
# Ceará e Bahia'
soildata <- soildata[dataset_id != "ctb0654", ]
summary_soildata(soildata)
# Layers: 60882
# Events: 18498
# Georeferenced events: 14956

# ctb0800 (many duplicates of ctb0702)
# Estudos pedológicos e suas relações ambientais
soildata <- soildata[dataset_id != "ctb0800", ]
summary_soildata(soildata)
# Layers: 60637
# Events: 18454
# Georeferenced events: 14912

# ctb0808 (exact duplicate of ctb0574)
# Conjunto de dados do levantamento semidetalhado 'Levantamento Semidetalhado e Aptidão Agrícola dos
# Solos do Município do Rio de Janeiro, RJ.'
soildata <- soildata[dataset_id != "ctb0808", ]
summary_soildata(soildata)
# Layers: 60296
# Events: 18394
# Georeferenced events: 14852

# LAYER ORDER
soildata <- soildata[order(id, profund_sup, profund_inf)]

# Correct layer names (if necessary)
soildata[camada_nome == "", camada_nome := NA_character_]
soildata[
  id == "ctb0770-100" & camada_nome == "B21H",
  camada_nome := ifelse(camada_nome == "B21H", "B21h", camada_nome)
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
nrow(soildata[profund_sup == profund_inf]) # 222 layers
soildata[, equal_depth := any(profund_sup == profund_inf), by = id]
print(soildata[equal_depth == TRUE, ..cols])
# Add a fixed depth to R, D, and C layers with equal depth limits
plus_depth <- 20
soildata[
  profund_sup == profund_inf & grepl("R|D|C", camada_nome),
  profund_inf := profund_inf + plus_depth
]
nrow(soildata[profund_sup == profund_inf]) # 67 layers
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
nrow(soildata[profund_sup == profund_inf]) # 59 layers
soildata[, n_layers := NULL]
print(soildata[equal_depth == TRUE, ..cols])
# Some events with profund_sup == profund_inf and profund_sup == 0 are from ctb0631.
# Actually, these layers have not a depth limit recorded. So we set them to NA.
soildata[
  dataset_id == "ctb0631" & profund_sup == profund_inf & profund_sup == 0,
  .(id, camada_nome, profund_sup, profund_inf, carbono)
]
soildata <- soildata[!(dataset_id == "ctb0631" & profund_sup == profund_inf & profund_sup == 0)]
nrow(soildata[profund_sup == profund_inf]) # 36 layers
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
nrow(soildata[profund_sup == profund_inf]) # 15 layers
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
nrow(soildata[profund_sup == profund_inf]) # 0 layers
soildata[, equal_depth := NULL]
summary_soildata(soildata)
# Layers: 58171
# Events: 16988
# Georeferenced events: 14507

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
# Layers: 57607
# Events: 16988
# Georeferenced events: 14506

# Update layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Fine earth
# R layers are consolidated rock layers. These layers should have terrafina == NA_real.
# Correct samples with terrafina == 0 g/kg
# When terrafina == 0, we set the fine earth content to 1000 g/kg.
soildata[terrafina == 0, .N] # 23 samples with terrafina == 0
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
print(soildata[esqueleto > 800, .N]) # 133 sample with esqueleto > 800
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
# Layers: 57609
# Events: 16988
# Georeferenced events: 14506

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
soildata[psd != 100, .N] # 2363 layers
psd_lims <- 90:110
soildata[!is.na(psd) & !(psd %in% psd_lims), .N] # 2 layers
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
# Layers: 57606
# Events: 16988
# Georeferenced events: 14506

# Clean events

# Get unique events
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_ano")
]
nrow(soildata_events) # 13374 events

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
View(soildata_events[duplicated == TRUE, ])

# ctb0010
# Create a spatial object for ctb0010 dataset
ctb0010_sf <- sf::st_as_sf(soildata[dataset_id == "ctb0010" & !is.na(coord_x) & !is.na(coord_y)],
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
ctb0010_sf[, coord_x := coord_x + runif(.N, -1, 1), by = observacao_id]
set.seed(2001)
ctb0010_sf[, coord_y := coord_y + runif(.N, -1, 1), by = observacao_id]
ctb0010_sf <- sf::st_as_sf(ctb0010_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0010_sf <- sf::st_transform(ctb0010_sf, crs = 4326)
# Update coordinates in soildata
soildata[dataset_id == "ctb0010" & !is.na(coord_x) & !is.na(coord_y), `:=`(
  coord_x = sf::st_coordinates(ctb0010_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0010_sf)[, "Y"]
)]
rm(ctb0010_sf)

# ctb0029

# Jitter duplicates...


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
idx_duplicated <- duplicated(soildata[
  dataset_id == "ctb0029" & !is.na(coord_x) & !is.na(coord_y),
  .(coord_x, coord_y)
])
# Create a spatial object for ctb0029 dataset considering only idx_duplicated
ctb0029_sf <- soildata[dataset_id == "ctb0029" & !is.na(coord_x) & !is.na(coord_y)]
ctb0029_sf <- ctb0029_sf[idx_duplicated, ]
ctb0029_sf <- sf::st_as_sf(ctb0029_sf, coords = c("coord_x", "coord_y"), crs = 4326)
# Transform to UTM
ctb0029_sf <- sf::st_transform(ctb0029_sf, crs = 32720)
ctb0029_sf < data.table(
  observacao_id = ctb0029_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0029_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0029_sf)[, "Y"]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0029_sf[, coord_x := coord_x + runif(.N, -1, 1), by = observacao_id]
set.seed(2001)
ctb0029_sf[, coord_y := coord_y + runif(.N, -1, 1), by = observacao_id]
ctb0029_sf <- sf::st_as_sf(ctb0029_sf, coords = c("coord_x", "coord_y"), crs = 32720)
# Transform back to WGS84
ctb0029_sf <- sf::st_transform(ctb0029_sf, crs = 4326)
# Update coordinates in soildata
soildata[dataset_id == "ctb0029" & !is.na(coord_x) & !is.na(coord_y),] `:=`(
  coord_x = sf::st_coordinates(ctb0029_sf)[, "X"],
  coord_y = sf::st_coordinates(ctb0029_sf)[, "Y"]
)]


# title: SoilData Integration
# subtitle: Merge National Forest Inventory data
# author: Alessandro Samuel-Rosa
# date: 2026
# licence: MIT
# description: This script integrates seven soil datasets from the Brazilian
# National Forest Inventory (Inventário Florestal Nacional) in the FEBR
# repository into the Brazilian Soil Dataset. It reads event and layer files,
# standardizes variable names, converts geographic coordinates to WGS84
# (EPSG:4326), and removes invalid sampling years. It then combines the
# National Forest Inventory data with the dataset produced in the previous step
# and creates maps showing the spatial distribution before and after the
# integration. Finally, it fills missing title, license, and organization
# metadata for the integrated datasets and writes the resulting dataset to
# data/12_soildata.txt.
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("geobr")) {
  install.packages("geobr")
  library(geobr)
}

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Read Brazilian state boundaries
brazil <- read_brazil_states()
if (FALSE) {
  # Plot Brazilian states
  plot(brazil["code_state"], col = "gray95", lwd = 0.5, reset = FALSE)
}

# Rename columns following previous standards
rename <- list(
  dados_id_febr = "dataset_id",
  data_coleta_ano = "data_ano",
  evento_id_febr = "id",
  camada_id_febr = "camada_id",
  coord_longitude = "coord_x",
  coord_latitude = "coord_y",
  coord_datum_epsg = "coord_datum",
  coord_municipio_nome = "municipio_id",
  coord_estado_sigla = "estado_id",
  coord_pais_id = "pais_id",
  subamostra_quanti = "amostra_quanti",
  ph_h2o_25_eletrodo = "ph",
  ph_h2o = "ph",
  ctc_soma_calc = "ctc",
  carbono_forno_1min950_cgdct = "carbono",
  argila_sodio_pipeta = "argila",
  densidade_solo_cilindro = "dsi",
  sibcs_20xx = "taxon_sibcs"
)

# Events #######################################################################
files_event <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-evento.txt$",
  full.names = TRUE, recursive = TRUE
)
length(files_event)
# 7 data sets
print(files_event)
data_event <- list()
for (i in seq_along(files_event)) {
  data_event[[i]] <- data.table::fread(files_event[i], dec = ",")
  id <- rev(strsplit(files_event[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_event[[i]][, dados_id_febr := id]
  data.table::setnames(data_event[[i]],
    old = names(rename), new = unlist(rename), skip_absent = TRUE
  )
}
data_event <- data.table::rbindlist(data_event, fill = TRUE)
nrow(data_event)
# 1662 events

# Standardize coordinate reference system
target_crs <- 4326
data_event[, coord_datum := as.integer(gsub("EPSG:", "", coord_datum))]
sf_data_event <- split(data_event, data_event[, coord_datum])
idx_transform <- which(names(sf_data_event) != target_crs)
for (i in seq_along(sf_data_event)) {
  if (i %in% idx_transform) {
    crs <- as.integer(names(sf_data_event[i]))
    sf_data_event[[i]] <- sf::st_as_sf(
      sf_data_event[[i]],
      coords = c("coord_x", "coord_y"), crs = crs
    )
    sf_data_event[[i]] <- sf::st_transform(sf_data_event[[i]], crs = target_crs)
  } else {
    sf_data_event[[i]] <- sf::st_as_sf(sf_data_event[[i]],
      coords = c("coord_x", "coord_y"),
      crs = target_crs
    )
  }
}
data_event <- do.call(rbind, sf_data_event)
data_event <- cbind(sf::st_coordinates(data_event), as.data.frame(data_event))
data_event <- data.table::as.data.table(data_event)
data_event[
  coord_datum != target_crs & !is.na(coord_datum),
  coord_datum := target_crs
]
data.table::setnames(data_event,
  old = c("X", "Y"), new = c("coord_x", "coord_y")
)
data_event[, geometry := NULL]
summary_soildata(data_event)
# Layers: 1662
# Events: 1662
# Georeference: 1662 (yes) / 0 (no)
# Date: 1662 (yes) / 0 (no)
# Datasets: 7

# Clean sampling date (just to make sure)
if (any(data_event$data_ano < 2000)) {
  stop("Some sampling years are before 2000. Setting them to NA.")
}
if (any(data_event$data_ano > as.integer(format(Sys.time(), "%Y")))) {
  stop("Some sampling years are in the future. Setting them to NA.")
}
data_event[!is.na(data_ano), data_ano_fonte := "original"]
summary_soildata(data_event)
# Layers: 1662
# Events: 1662
# Georeference: 1662 (yes) / 0 (no)
# Date: 1662 (yes) / 0 (no)
# Datasets: 7

# Layers #######################################################################
files_layer <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-camada.txt$",
  full.names = TRUE, recursive = TRUE
)
length(files_layer)
# 7 data sets
print(files_layer)
data_layer <- list()
for (i in seq_along(files_layer)) {
  data_layer[[i]] <- data.table::fread(files_layer[i], dec = ",")
  id <- rev(strsplit(files_layer[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_layer[[i]][, dados_id_febr := id]
  data.table::setnames(data_layer[[i]],
    old = names(rename), new = unlist(rename), skip_absent = TRUE
  )
}
data_layer <- data.table::rbindlist(data_layer, fill = TRUE)
data_layer[, camada_nome := camada_id]

# Standardize layer names (camada_nome), removing spaces
data_layer[, camada_nome := gsub(" ", "", camada_nome)]
data_layer[, .N, by = camada_nome]
nrow(data_layer)
# 2134 layers

# Merge data from events and layers ############################################
ifndata <- merge(data_event, data_layer, by = c("dataset_id", "id"))
colnames(ifndata)
if (!"terrafina" %in% colnames(ifndata)) {
  ifndata[, terrafina := NA_real_]
}
# if (!"camada_nome" %in% colnames(ifndata)) {
#   ifndata[, camada_nome := NA_character_]
# }
summary_soildata(ifndata)
# Layers: 1941
# Events: 1051
# Georeference: 1051 (yes) / 0 (no)
# Date: 1051 (yes) / 0 (no)
# Datasets: 7

# Check IFN for outliers #######################################################
# Run the curation checks on the merged IFN data
ifndata <- curate_soil_data_dt(
  dt = ifndata,
  col_id = "id",
  col_layer = "camada_nome",
  col_soc = "carbono",
  col_clay = "argila",
  col_silt = "silte",
  col_sand = "areia",
  col_bd = "dsi",
  texture_tol_pct = 0.05
)
# Summary of quality classification across depth layers
ifndata[, .N, by = .(camada_nome, quality_flag)]

# Subset clean records for digital soil mapping / stock modeling
modeling_ready_dt <- ifndata[quality_flag == "Consistent"]

# Inspect high-inconsistency records
audit_dt <- ifndata[
  quality_flag == "Inconsistent (Discard/Audit)", 
  .(id, camada_nome, carbono, argila, dsi, std_ptf_res, inconsistency_score)
]
if (FALSE) {
  # View high-inconsistency records
  View(audit_dt[order(-inconsistency_score)])
}

# Read SoilData data processed in the previous scripts #########################
soildata_02 <- data.table::fread("data/11_soildata.txt",
  sep = "\t", na.strings = c("", "NA")
)
if (!"coord_datum" %in% colnames(soildata_02)) {
  soildata_02[, coord_datum := 4326]
}
# Check spatial distribution before merging National Forest Inventory data
soildata_02_sf <- soildata_02[!is.na(coord_x) & !is.na(coord_y)]
soildata_02_sf <- sf::st_as_sf(soildata_02_sf,
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Plot spatial distribution
file_path <- fig_path("121_spatial_distribution_before_ifn_data.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData before merging IFN data"
)
plot(soildata_02_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()
summary_soildata(soildata_02)
# Layers: 50277
# Events: 14003
# Georeference: 10903 (yes) / 3100 (no)
# Date: 13850 (yes) / 153 (no)
# Datasets: 235

# Merge SoilData data with National Forest Inventory data ######################
ifndata[, observacao_id := id]
ifndata[, id := paste0(dataset_id, "-", id)]
soildata <- rbind(soildata_02, ifndata, fill = TRUE)
summary_soildata(soildata)
# Layers: 52218
# Events: 15054
# Georeference: 11954 (yes) / 3100 (no)
# Date: 14901 (yes) / 153 (no)
# Datasets: 242

# Check spatial distribution after merging National Forest Inventory data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf,
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Plot spatial distribution
file_path <- fig_path("122_spatial_distribution_after_ifn_data.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after merging IFN data"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Set values for missing title
# "ctb0055" "ctb0056" "ctb0057" "ctb0058" "ctb0059" "ctb0060" "ctb0061"
soildata[is.na(dataset_titulo), unique(dataset_id)]
dataset_title <- c(
  "ctb0055" = "ctb0055-Inventário Florestal Nacional - Paraná",
  "ctb0056" = "ctb0056-Inventário Florestal Nacional - Espírito Santo",
  "ctb0057" = "ctb0057-Inventário Florestal Nacional - Sergipe",
  "ctb0058" = "ctb0058-Inventário Florestal Nacional - Rio Grande do Norte",
  "ctb0059" = "ctb0059-Inventário Florestal Nacional - Ceará",
  "ctb0060" = "ctb0060-Inventário Florestal Nacional - Paraíba",
  "ctb0061" = "ctb0061-Inventário Florestal Nacional - Caçador SC"
)
soildata[is.na(dataset_titulo), dataset_titulo := dataset_title[dataset_id]]

# Set values for missing licence
soildata[is.na(dataset_licenca), unique(dataset_id)]
# "ctb0055" "ctb0056" "ctb0057" "ctb0058" "ctb0059" "ctb0060" "ctb0061"
dataset_licence <- c(
  "ctb0055" = "CC-BY-4.0",
  "ctb0056" = "CC-BY-4.0",
  "ctb0057" = "CC-BY-4.0",
  "ctb0058" = "CC-BY-4.0",
  "ctb0059" = "CC-BY-4.0",
  "ctb0060" = "CC-BY-4.0",
  "ctb0061" = "CC-BY-4.0"
)
soildata[is.na(dataset_licenca), dataset_licenca := dataset_licence[dataset_id]]

# Set values for missing organizacao_nome
soildata[is.na(organizacao_nome), unique(dataset_id)]
# "ctb0055" "ctb0056" "ctb0057" "ctb0058" "ctb0059" "ctb0060" "ctb0061"
# organizacao_nome = Serviço Florestal Brasileiro (SFB/MAPA)
dataset_organization <- c(
  "ctb0055" = "Serviço Florestal Brasileiro (SFB/MAPA)",
  "ctb0056" = "Serviço Florestal Brasileiro (SFB/MAPA)",
  "ctb0057" = "Serviço Florestal Brasileiro (SFB/MAPA)",
  "ctb0058" = "Serviço Florestal Brasileiro (SFB/MAPA)",
  "ctb0059" = "Serviço Florestal Brasileiro (SFB/MAPA)",
  "ctb0060" = "Serviço Florestal Brasileiro (SFB/MAPA)",
  "ctb0061" = "Serviço Florestal Brasileiro (SFB/MAPA)"
)
soildata[is.na(organizacao_nome), organizacao_nome := dataset_organization[dataset_id]]

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 52256
# Events: 15171
# Georeferenced events: 12041
# Datasets: 242
data.table::fwrite(soildata, "data/12_soildata.txt", sep = "\t")

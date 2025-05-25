# title: SoilData Integration
# subtitle: Merge external data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
# licence: MIT
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}

# Source helper functions
source("src/00_helper_functions.R")

# Download Brazilian state boundaries
brazil <- geobr::read_state()

# Rename columns following previous standards
rename <- c(
  "dados_id_febr",                "dataset_id",
  "evento_id_febr",               "id",
  "camada_id_febr",               "camada_id",
  "coord_longitude",              "coord_x",
  "coord_latitude",               "coord_y",
  "coord_estado_sigla",           "estado_id",
  "ph_h2o_25_eletrodo",           "ph",
  "ph_h2o",                       "ph",
  "ctc_soma_calc",                "ctc",
  "carbono_forno_1min950_cgdct",  "carbono",
  "argila_sodio_pipeta",          "argila",
  "densidade_solo_cilindro",      "dsi",
  "sibcs_20xx",                   "taxon_sibcs"
)
rename <- matrix(rename, ncol = 2, byrow = TRUE)

# Load external data sets
# Events
files_event <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-evento.txt$",
  full.names = TRUE, recursive = TRUE
)
length(files_event) # 7 data sets
print(files_event)
data_event <- list()
for (i in seq_along(files_event)) {
  data_event[[i]] <- data.table::fread(files_event[i], dec = ",")
  id <- rev(strsplit(files_event[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_event[[i]][, dados_id_febr := id]
  data.table::setnames(data_event[[i]], old = rename[, 1], new = rename[, 2], skip_absent = TRUE)
}
data_event <- data.table::rbindlist(data_event, fill = TRUE)
nrow(data_event) # 1662 events

# Standardize coordinate reference system
target_crs <- 4326
data_event[, coord_datum_epsg := as.integer(gsub("EPSG:", "", coord_datum_epsg))]
sf_data_event <- split(data_event, data_event[, coord_datum_epsg])
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
data_event[coord_datum_epsg != target_crs & !is.na(coord_datum_epsg), coord_datum_epsg := target_crs]
data.table::setnames(data_event, old = c("X", "Y"), new = c("coord_x", "coord_y"))
data_event[, geometry := NULL]
summary_soildata(data_event)
# Layers: 1662
# Events: 1662
# Georeferenced events: 1662

# Clean sampling date (just to make sure)
data_event[data_coleta_ano < 1950, data_coleta_ano := NA_integer_]
data_event[data_coleta_ano > as.integer(format(Sys.time(), "%Y")), data_coleta_ano := NA_integer_]
data_event[!is.na(data_coleta_ano), data_coleta_ano_fonte := "original"]
summary_soildata(data_event)
# Layers: 1662
# Events: 1662
# Georeferenced events: 1662

# Layers
files_layer <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-camada.txt$",
  full.names = TRUE, recursive = TRUE
)
length(files_layer) # 7 data sets
print(files_layer)
data_layer <- list()
for (i in seq_along(files_layer)) {
  data_layer[[i]] <- data.table::fread(files_layer[i], dec = ",")
  id <- rev(strsplit(files_layer[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_layer[[i]][, dados_id_febr := id]
  data.table::setnames(data_layer[[i]], old = rename[, 1], new = rename[, 2], skip_absent = TRUE)
}
data_layer <- data.table::rbindlist(data_layer, fill = TRUE)
data_layer[, camada_nome := camada_id]
nrow(data_layer) # 2134 layers

# Merge data from events and layers
soildata_01 <- merge(data_event, data_layer, by = c("dataset_id", "id"))
colnames(soildata_01)
if (!"terrafina" %in% colnames(soildata_01)) {
  soildata_01[, terrafina := NA_real_]
}
# if (!"camada_nome" %in% colnames(soildata_01)) {
#   soildata_01[, camada_nome := NA_character_]
# }
summary_soildata(soildata_01)
# Layers: 1941
# Events: 1051
# Georeferenced events: 1051

# Read SoilData data processed in the previous scripts
soildata_02 <- data.table::fread("data/11_soildata.txt", sep = "\t", na.strings = c("", "NA"))
if (!"coord_datum_epsg" %in% colnames(soildata_02)) {
  soildata_02[, coord_datum_epsg := 4326]
}
# Check spatial distribution before merging external data
soildata_02_sf <- soildata_02[!is.na(coord_x) & !is.na(coord_y)]
soildata_02_sf <- sf::st_as_sf(soildata_02_sf, coords = c("coord_x", "coord_y"), crs = 4326)
file_path <- "res/fig/121_spatial_distribution_before_external_data.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData before merging external data"
)
plot(soildata_02_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Merge SoilData data with external data
soildata_01[, observacao_id := id]
soildata_01[, id := paste0(dataset_id, "-", id)]
soildata <- rbind(soildata_02, soildata_01, fill = TRUE)
summary_soildata(soildata)
# Layers: 52326
# Events: 15241
# Georeferenced events: 12111

# Check spatial distribution after merging external data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
file_path <- "res/fig/122_spatial_distribution_after_external_data.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after merging external data"
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

# Write data to disk
summary_soildata(soildata)
# Layers: 52326
# Events: 15241
# Georeferenced events: 12111
data.table::fwrite(soildata, "data/12_soildata.txt", sep = "\t")

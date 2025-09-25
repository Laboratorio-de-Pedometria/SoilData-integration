# title: SoilData Integration
# subtitle: Merge curated data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
# licence: MIT
# summary: This script integrates manually curated soil datasets into the main Brazilian Soil 
#          Dataset. It reads multiple curated CSV files from a local directory, standardizes their 
#          columns, and combines them into a single data table. It then loads the main dataset 
#          processed in the previous step and plots its spatial distribution. To prevent 
#          duplication, any datasets present in the curated data are first removed from the main 
#          dataset before the curated data is merged. Finally, the script plots the spatial 
#          distribution of the newly combined dataset and saves the final result to a file.
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

# Read curated data
dir_path <- "~/ownCloud/SoilData"
# Check if the directory exists
if (!dir.exists(dir_path)) {
  stop("Directory does not exist: ", dir_path)
} else {
  curated_path <- list.files(
    path = path.expand(dir_path), pattern = "^ctb[0-9]{4}\\.csv$",
    full.names = TRUE, recursive = TRUE
  )
  length(curated_path)
  # 28 datasets
  print(curated_path)
}

# Read all files and store them in a list
curated_list <- lapply(curated_path, function(x) {
  data.table::fread(x, na.strings = c("NA", ""))
})

# rbind all datasets keeping only the matching columns
# Target columns
read_cols <- c(
  "dataset_id",
  "observacao_id",
  "data_ano",
  "data_fonte",
  "coord_x", "coord_y", "coord_precisao", "coord_fonte", "coord_datum",
  "pais_id", "estado_id", "municipio_id",
  "amostra_area",
  "taxon_sibcs",
  "camada_nome", "camada_id", "amostra_id",
  "profund_sup", "profund_inf",
  "carbono", "ctc", "ph", "argila", "silte", "areia", "terrafina", "dsi"
)
curated_data <- data.table::rbindlist(curated_list, fill = TRUE)
curated_data <- curated_data[, ..read_cols]
curated_data[, id := paste0(dataset_id, "-", observacao_id)]
curated_data[!is.na(data_ano), data_fonte := NA_character_]
summary_soildata(curated_data)
# Layers: 10105
# Events: 3780
# Georeferenced events: 3366

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/12_soildata.txt", sep = "\t", na.strings = c("", "NA"))
summary_soildata(soildata)
# Layers: 52256
# Events: 15171
# Georeferenced events: 12041

# FIGURE 13.1
# Check spatial distribution before merging curated data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot spatial distribution
file_path <- "res/fig/131_spatial_distribution_before_curated_data.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData before merging curated data"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Merge curated data with SoilData
# Adjust column names
data.table::setnames(soildata, old = "data_coleta_ano", new = "data_ano")
data.table::setnames(soildata, old = "data_coleta_ano_fonte", new = "data_fonte")
data.table::setnames(soildata, old = "coord_datum_epsg", new = "coord_datum")
# Filter out duplicated datasets
curated_ctb <- curated_data[, unique(dataset_id)]
soildata <- soildata[!dataset_id %in% curated_ctb]
summary_soildata(soildata)
# Layers: 51040
# Events: 14757
# Georeferenced events: 11629
# Merge curated data with SoilData
soildata <- rbind(soildata, curated_data, fill = TRUE)
summary_soildata(soildata)
# Layers: 61145
# Events: 18537
# Georeferenced events: 14995

# FIGURE 13.2
# Check spatial distribution after merging curated data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot spatial distribution
file_path <- "res/fig/132_spatial_distribution_after_curated_data.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after merging curated data"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

####################################################################################################
# Export cleaned data
summary_soildata(soildata)
# Layers: 61145
# Events: 18537
# Georeferenced events: 14995
# Datasets: 263
data.table::fwrite(soildata, "data/13_soildata.txt", sep = "\t")

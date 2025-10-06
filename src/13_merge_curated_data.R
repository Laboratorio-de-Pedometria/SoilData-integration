# title: SoilData Integration
# subtitle: Merge curated data
# author: Alessandro Samuel-Rosa
# date: 2025
# licence: MIT
# summary: This script integrates manually curated soil datasets into the main Brazilian Soil 
#          Dataset. It reads curated CSV files, standardizes them, and merges them with the main 
#          dataset, removing duplicates to prevent redundancy. For datasets still missing metadata 
#          (like title and license) after the merge, it queries the Dataverse API to fetch and 
#          populate these details. The script also generates spatial distribution plots before and 
#          after the merge to visualize the impact of the integration. The final, enriched dataset 
#          is then saved to a file.
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
  brazil <- geobr::read_state(simplified = FALSE)
  # Save the data to a file for future use
  sf::st_write(brazil, "data/brazil_states.geojson")
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

# Fix known issues in curated data
curated_data[dataset_id == "ctb0063", data_ano := ifelse(data_ano == 1, 2000, data_ano)]

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

# PREPARE FOR MERGE
# Adjust soildata column names
data.table::setnames(soildata, old = "data_coleta_ano", new = "data_ano")
data.table::setnames(soildata, old = "data_coleta_ano_fonte", new = "data_fonte")
data.table::setnames(soildata, old = "coord_datum_epsg", new = "coord_datum")

# Append dataset_titulo, organizacao_nome, and dataset_licenca to curated_data
# from soildata (first occurrence of each dataset_id)
curated_data <- merge(curated_data,
  unique(soildata[, .(dataset_id, dataset_titulo, organizacao_nome, dataset_licenca)]),
  by = "dataset_id", all.x = TRUE
)

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

# Check for missing
missing_title <- soildata[is.na(dataset_titulo), unique(dataset_id)]
print(missing_title)
# "ctb0020" "ctb0021" "ctb0024" "ctb0035" "ctb0037" "ctb0038" "ctb0039"
# "ctb0040" "ctb0041" "ctb0044" "ctb0046" "ctb0047" "ctb0048" "ctb0049"
# "ctb0050" "ctb0051" "ctb0052" "ctb0053" "ctb0054" "ctb0062" "ctb0063"
unique(soildata[dataset_id %in% missing_title, .(dataset_id, dataset_titulo, dataset_licenca)])

# Query SoilData API: get DOIs for ctbs with missing titles and licenses
missing_title_doi <- ctb_query(missing_title, doi = TRUE)
length(missing_title_doi) == length(missing_title)
# Query SoilData API: get details for ctbs with missing titles and licenses
missing_title_details <- lapply(missing_title_doi,
  dataverse::get_dataset,
  server = "https://soildata.mapbiomas.org/dataverse/soildata"
)
length(missing_title_details) == length(missing_title)

# Set titles and licenses for missing datasets
for (i in seq_along(missing_title_details)) {
  citation_fields <- missing_title_details[[i]]$metadataBlocks$citation$fields
  ctb_id <- citation_fields$value[citation_fields$typeName == "otherId"][[1]]$otherIdValue$value
  print(ctb_id)
  ctb_title <- citation_fields$value[citation_fields$typeName == "title"][[1]]
  print(ctb_title)
  ctb_license <- missing_title_details[[i]]$license$name
  if (is.null(ctb_license)) {
    ctb_license <- missing_title_details[[i]]$termsOfUse
  }
  print(ctb_license)
  soildata[dataset_id == ctb_id, dataset_titulo := ctb_title]
  soildata[dataset_id == ctb_id, dataset_licenca := ctb_license]
}
soildata[dataset_id %in% missing_title, .(dataset_id, dataset_titulo, dataset_licenca)]

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

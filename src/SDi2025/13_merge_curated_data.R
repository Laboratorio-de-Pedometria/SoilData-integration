# title: SoilData Integration
# subtitle: Merge curated data
# author: Alessandro Rosa
# date: 2026
# licence: MIT
# description: This script merges datasets curated after the release of the
# latest Brazilian Soil Dataset for the production of Collection 3 of MapBiomas
# Soil into the processed SoilData dataset. The curated data may include new
# datasets or revised versions of datasets already included in this integration
# routine. The script clones or updates the curated-data repository as needed,
# reads and standardizes its CSV files, adds organization metadata, removes
# duplicate datasets, compares spatial distributions before and after the merge,
# checks for missing dataset metadata, and exports the result to
# data/13_soildata.txt.
rm(list = ls())

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Load required packages
library(data.table)
library(sf)
library(geobr)

# Read Brazilian state boundaries
brazil <- read_brazil_states()

# Read datasets curated for MapBiomas Soil Collection 3
# dir_path <- "~/ownCloud/SoilData"
dir_path <- "~/projects/SoilData/SoilData-ctb"
dir_path <- path.expand(dir_path)
repo_url <- "https://github.com/Laboratorio-de-Pedometria/SoilData-ctb.git"
if (!dir.exists(dir_path)) {
  dir.create(dirname(dir_path), recursive = TRUE, showWarnings = FALSE)
  # Clone the curated-data repository if it is not available locally
  clone_status <- system2("git", c("clone", repo_url, dir_path))
  if (clone_status != 0L || !dir.exists(dir_path)) {
    stop("Could not clone curated-data repository from: ", repo_url)
  }
} else {
  # Check that the existing directory is a Git clone and is up to date
  git_check <- system2(
    "git", c("-C", dir_path, "rev-parse", "--is-inside-work-tree"),
    stdout = TRUE, stderr = FALSE
  )
  if (!identical(tolower(trimws(git_check)), "true")) {
    stop("Directory is not a Git repository: ", dir_path)
  }
  fetch_status <- system2("git", c("-C", dir_path, "fetch", "origin"))
  if (fetch_status != 0L) {
    stop("Could not fetch updates from: ", repo_url)
  }
  local_commit <- system2(
    "git", c("-C", dir_path, "rev-parse", "HEAD"), stdout = TRUE
  )
  remote_commit <- system2(
    "git", c("-C", dir_path, "rev-parse", "origin/main"), stdout = TRUE
  )
  if (!identical(trimws(local_commit), trimws(remote_commit))) {
    stop(
      paste("Curated-data repository is not up to date.\n",
       "Please pull origin/main before continuing: "),
      dir_path
    )
  }
}

# List curated CSV files in the directory
curated_path <- list.files(
  path = dir_path, pattern = "^ctb[0-9]{4}\\.csv$",
  full.names = TRUE, recursive = TRUE
)
length(curated_path)
# 40 curated datasets
print(curated_path)

# Read all files and store them in a list
curated_list <- lapply(curated_path, function(x) {
  curated <- data.table::fread(x, na.strings = c("NA", ""))
  data.table::setnames(curated, "ano_fonte", "data_ano_fonte")
  curated
})
length(curated_list)
# 40 curated datasets

# rbind all datasets keeping only the matching columns
# Target columns
read_cols <- c(
  "dataset_id", "dataset_titulo", "dataset_licenca",
  "observacao_id",
  "data_ano", "data_ano_fonte",
  "coord_x", "coord_y", "coord_precisao", "coord_fonte", "coord_datum",
  "pais_id", "estado_id", "municipio_id",
  "amostra_area",
  "taxon_sibcs", "taxon_st",
  "pedregosidade", "rochosidade",
  "camada_nome", "camada_id", "amostra_id",
  "profund_sup", "profund_inf",
  "terrafina",
  "argila", "silte", "areia", 
  "carbono", "ctc", "ph", "dsi"
)
curated_data <- data.table::rbindlist(curated_list, fill = TRUE)
curated_data <- curated_data[, ..read_cols]
curated_data[, id := paste0(dataset_id, "-", observacao_id)]
summary_soildata(curated_data)
# 2026 ---
# Layers: 13561 (13446?????????)
# Events: 5914
# Georeference: 5472 (yes) / 442 (no)
# Date: 5911 (yes) / 3 (no)
# Datasets: 40
# 2025 ---
# Layers: 10105
# Events: 3780
# Georeferenced events: 3366
# Datasets: 29

# Merge curated data with SoilData #############################################
# Read SoilData data processed in the previous script
file <- "data/12_soildata.txt"
soildata <- data.table::fread(file, sep = "\t", na.strings = c("", "NA"))
summary_soildata(soildata)
# 2026 ---
# Layers: 51803
# Events: 14892
# Georeference: 11792 (yes) / 3100 (no)
# Date: 14739 (yes) / 153 (no)
# Datasets: 242
# 2025 ---
# Layers: 52256
# Events: 15171
# Georeferenced events: 12041
# Datasets: 242

# FIGURE 13.1
# Check spatial distribution before merging curated data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
xy_cols <- c("coord_x", "coord_y")
soildata_sf <- sf::st_as_sf(soildata_sf, coords = xy_cols, crs = 4326)
# Plot spatial distribution
file_path <- fig_path("131_spatial_distribution_before_curated_data.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData before merging curated data"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Append organization to curated_data using the first occurrence of each dataset_id.
metadata <- soildata[, .(
  organizacao_nome = organizacao_nome[1L]
), by = dataset_id]
curated_data <- merge(curated_data, metadata, by = "dataset_id", all.x = TRUE)

# Filter out duplicated datasets
curated_ctb <- curated_data[, unique(dataset_id)]
soildata <- soildata[!dataset_id %in% curated_ctb]
summary_soildata(soildata)
# 2026 ---
# Layers: 50587
# Events: 14478
# Georeference: 11380 (yes) / 3098 (no)
# Date: 14326 (yes) / 152 (no)
# Datasets: 234
# 2025 ---
# Layers: 51040
# Events: 14757
# Georeferenced events: 11629
# Datasets: 234

# Merge curated data with SoilData
soildata <- rbind(soildata, curated_data, fill = TRUE)
summary_soildata(soildata)
# 2026 ---
# Layers: 64148
# Events: 20392
# Georeference: 16852 (yes) / 3540 (no)
# Date: 20237 (yes) / 155 (no)
# Datasets: 274
# 2025 ---
# Layers: 61145
# Events: 18537
# Georeferenced events: 14995
# Datasets: 263

# Check for missing titles and licenses in the merged dataset
missing_title <- soildata[is.na(dataset_titulo), unique(dataset_id)]
if (length(missing_title) == 0) {
  message("All datasets have titles and licenses.")
} else {
  warning(
    "The following datasets have missing titles and licenses:\n",
    paste(missing_title, collapse = ", ")
  )
}

# THE FOLLOWING CODE MAY BE DELETED IN THE FUTURE!
# # Query SoilData API: get DOIs for ctbs with missing titles and licenses
# missing_title_doi <- ctb_query(missing_title, doi = TRUE)
# length(missing_title_doi) == length(missing_title)
# # Query SoilData API: get details for ctbs with missing titles and licenses
# missing_title_details <- lapply(missing_title_doi,
#   dataverse::get_dataset,
#   server = "https://repositorio.soildata.mapbiomas.org/dataverse/soildata"
# )
# length(missing_title_details) == length(missing_title)

# # Set titles and licenses for missing datasets
# for (i in seq_along(missing_title_details)) {
#   citation_fields <- missing_title_details[[i]]$metadataBlocks$citation$fields
#   ctb_id <- citation_fields$value[
#     citation_fields$typeName == "otherId"
#   ][[1]]$otherIdValue$value
#   print(ctb_id)
#   ctb_title <- citation_fields$value[citation_fields$typeName == "title"][[1]]
#   print(ctb_title)
#   ctb_license <- missing_title_details[[i]]$license$name
#   if (is.null(ctb_license)) {
#     ctb_license <- missing_title_details[[i]]$termsOfUse
#   }
#   print(ctb_license)
#   soildata[dataset_id == ctb_id, dataset_titulo := ctb_title]
#   soildata[dataset_id == ctb_id, dataset_licenca := ctb_license]
# }
# soildata[
#   dataset_id %in% missing_title,
#   .(dataset_id, dataset_titulo, dataset_licenca)
# ]

# FIGURE 13.2
# Check spatial distribution after merging curated data
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
xy_cols <- c("coord_x", "coord_y")
soildata_sf <- sf::st_as_sf(soildata_sf, coords = xy_cols, crs = 4326)
# Plot spatial distribution
file_path <- fig_path("132_spatial_distribution_after_curated_data.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after merging curated data"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Export cleaned data ##########################################################
summary_soildata(soildata)
# 2026 ---
# Layers: 64148
# Events: 20392
# Georeference: 16852 (yes) / 3540 (no)
# Date: 20237 (yes) / 155 (no)
# Datasets: 274
# 2025 ---
# Layers: 61145
# Events: 18537
# Georeferenced events: 14995
# Datasets: 263
data.table::fwrite(soildata, "data/13_soildata.txt", sep = "\t")

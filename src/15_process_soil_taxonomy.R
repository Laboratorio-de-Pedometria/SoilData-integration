# title: SoilData Integration
# subtitle: Process Soil Taxonomy data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
# licence: MIT
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("febr")) {
  if (!require(remotes)) {
    install.packages(pkgs = "remotes")
  }
  remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
}

# Source helper functions
source("src/00_helper_functions.R")

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/14_soildata.txt", sep = "\t", na.strings = c("NA", ""))
summary_soildata(soildata)
# Layers: 57079
# Events: 16824
# Georeferenced events: 14334

# Datasets with missing soil classification or not included in the original FEBR repository
no_taxon <- c(
  "ctb0033", "ctb0035", "ctb0053", "ctb0055", "ctb0059", "ctb0024", "ctb0040",
  "ctb0049", "ctb0056", "ctb0057", "ctb0058", "ctb0060", "ctb0061", "ctb0062"
)
# Check which of the remaining datasets are missing soil classification
missing_taxon <- soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "",
  .N,
  by = dataset_id
][order(-N)]
missing_taxon <- missing_taxon[N > 50]

# Download data from the FEBR repository
taxon_data <- febr::observation(missing_taxon$dataset_id, c("taxon", "sibcs"))
taxon_data <- data.table::rbindlist(taxon_data, use.names = TRUE, fill = TRUE)
colnames(taxon_data)

# Select relevant columns with soil classification
# Various columns have soil classification information. We will select the most relevant ones in the
# following order:
# 1. sibcs_2013
# 2. sibcs_2006
# 3. taxon_sibcs_200X
# 4. taxon_sibcs_200x
# 5. sibcs_20xx
# 6. taxon_sibcs_xxx.1
# 7. sibcs_1999
# 8. sibcs_19xx
# 9. taxon_sibcs_xxx
taxon_data[, taxon_sibcs := ifelse(
  !is.na(sibcs_2013), sibcs_2013,
  ifelse(!is.na(sibcs_2006), sibcs_2006,
    ifelse(!is.na(taxon_sibcs_200X), taxon_sibcs_200X,
      ifelse(!is.na(taxon_sibcs_200x), taxon_sibcs_200x,
        ifelse(!is.na(sibcs_20xx), sibcs_20xx,
          ifelse(!is.na(taxon_sibcs_xxx.1), taxon_sibcs_xxx.1,
            ifelse(!is.na(sibcs_1999), sibcs_1999,
              ifelse(!is.na(sibcs_19xx), sibcs_19xx, taxon_sibcs_xxx)
            )
          )
        )
      )
    )
  )
)]
# Clean soil classification data
# Keep only the strings before the single capital A
taxon_data[, taxon_sibcs := gsub(" A .*", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub(" a .*", "", taxon_sibcs)]
# Keep only the strings before the word "textura"
taxon_data[, taxon_sibcs := gsub("textura", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("Textura", "", taxon_sibcs)]
# Keep only the strings before the word "Substrato"
taxon_data[, taxon_sibcs := gsub("Substrato.*", "", taxon_sibcs)]
# Keep only the strings before the word "argilosa"
taxon_data[, taxon_sibcs := gsub(" argilosa.*", "", taxon_sibcs)]
# Keep only the strings before the word "arenosa"
taxon_data[, taxon_sibcs := gsub(" arenosa.*", "", taxon_sibcs)]
# Keep only the strings before the word "muito"
taxon_data[, taxon_sibcs := gsub(" muito.*", "", taxon_sibcs)]
# Keep only the strings before the word "média"
taxon_data[, taxon_sibcs := gsub(" média.*", "", taxon_sibcs)]
# Keep only the strings before the word "media"
taxon_data[, taxon_sibcs := gsub(" media.*", "", taxon_sibcs)]
# Keep only the strings before the word "siltosa"
taxon_data[, taxon_sibcs := gsub(" siltosa.*", "", taxon_sibcs)]
# Keep only the strings before the word "cascalhento"
taxon_data[, taxon_sibcs := gsub(" cascalhento.*", "", taxon_sibcs)]
# Keep only the strings before the word "hístico"
taxon_data[, taxon_sibcs := gsub(" hístico.*", "", taxon_sibcs)]
# Keep only the strings before the word "fase"
taxon_data[, taxon_sibcs := gsub(" fase.*", "", taxon_sibcs)]
# Keep only the strings before the word "floresta"
taxon_data[, taxon_sibcs := gsub("floresta.*", "", taxon_sibcs)]
# Keep only the strings before the word "orgânica"
taxon_data[, taxon_sibcs := gsub(" orgânica.*", "", taxon_sibcs)]
# Keep only the strings before the word "campo"
taxon_data[, taxon_sibcs := gsub(" campo.*", "", taxon_sibcs)]
# Keep only the strings before the word "(?)"
taxon_data[, taxon_sibcs := gsub(" \\(\\?\\)", "", taxon_sibcs)]
# Remove "&#10;"
taxon_data[, taxon_sibcs := gsub("&#10;", "", taxon_sibcs)]
# argila de atividade baixa -> Tb
taxon_data[, taxon_sibcs := gsub("argila de atividade baixa", "Tb", taxon_sibcs)]
# argila de atividade alta -> Ta
taxon_data[, taxon_sibcs := gsub("argila de atividade alta", "Ta", taxon_sibcs)]
# Remove extra spaces
taxon_data[, taxon_sibcs := gsub("\\s+", " ", taxon_sibcs)]
# Drop period at the end of the string
taxon_data[, taxon_sibcs := gsub("\\.$", "", taxon_sibcs)]
# Drop comma at the end of the string
taxon_data[, taxon_sibcs := gsub(",$", "", taxon_sibcs)]
# trim
taxon_data[, taxon_sibcs := trimws(taxon_sibcs)]
taxon_data[, sample(unique(taxon_sibcs))]

# Create unique id for each event
taxon_data[, id := paste0(dataset_id, "-", evento_id_febr)]

# # Update soildata with taxon_data
soildata <- soildata[taxon_data, taxon_sibcs := i.taxon_sibcs, on = "id"]

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 57079
# Events: 16824
# Georeferenced events: 14334
data.table::fwrite(soildata, "data/15_soildata.txt", sep = "\t")

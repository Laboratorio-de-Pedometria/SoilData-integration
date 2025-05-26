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
# Layers: 61009
# Events: 18537
# Georeferenced events: 14994

# Check which datasets are missing the taxon_sibcs data
soildata[taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# Datasets with missing taxon_sibcs data
no_taxon <- c("ctb0033", "ctb0035", "ctb0053", "ctb0055", "ctb0059")

# ctb0657
# Pre 1999 soil classification system
ctb0657_taxon <- febr::observation("ctb0657", "taxon")
ctb0657_taxon <- data.table::as.data.table(ctb0657_taxon)
ctb0657_taxon[, id := paste0("ctb0657-", ctb0657_taxon$evento_id_febr)]
ctb0657_taxon[, taxon_sibcs := taxon_sibcs_xxx]
# Update the soildata with the taxon_sibcs from ctb0657. The id columns are the same.
soildata[ctb0657_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0657_taxon)

# Check if the taxon_sibcs was updated
soildata[taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0572
# Post 1999 soil classification system
ctb0572_taxon <- febr::observation("ctb0572", "taxon")
ctb0572_taxon <- data.table::as.data.table(ctb0572_taxon)
ctb0572_taxon[, id := paste0("ctb0572-", ctb0572_taxon$evento_id_febr)]
ctb0572_taxon[, taxon_sibcs := taxon_sibcs_200X]
# Update the soildata with the taxon_sibcs from ctb0572. The id columns are the same.
soildata[ctb0572_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0572_taxon)

# Check if the taxon_sibcs was updated
soildata[taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0829
# Pre 1999 soil classification system updated by experts
ctb0829_taxon <- febr::observation("ctb0829", "taxon")
ctb0829_taxon <- data.table::as.data.table(ctb0829_taxon)
ctb0829_taxon[, id := paste0("ctb0829-", ctb0829_taxon$evento_id_febr)]
ctb0829_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0829. The id columns are the same.
soildata[ctb0829_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0829_taxon)

# Check if the taxon_sibcs was updated
soildata[taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0770
# Pre 1999 soil classification system
ctb0770_taxon <- febr::observation("ctb0770", "taxon")
ctb0770_taxon <- data.table::as.data.table(ctb0770_taxon)
ctb0770_taxon[, id := paste0("ctb0770-", ctb0770_taxon$evento_id_febr)]
ctb0770_taxon[, taxon_sibcs := taxon_sibcs_xxx]
# Update the soildata with the taxon_sibcs from ctb0770. The id columns are the same.
soildata[ctb0770_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0770_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0631
# Pre 1999 soil classification system
ctb0631_taxon <- febr::observation("ctb0631", "sibcs")
ctb0631_taxon <- data.table::as.data.table(ctb0631_taxon)
ctb0631_taxon[, id := paste0("ctb0631-", ctb0631_taxon$evento_id_febr)]
ctb0631_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0631. The id columns are the same.
soildata[ctb0631_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0631_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0661
# Pre 1999 soil classification system
ctb0661_taxon <- febr::observation("ctb0661", "sibcs")
ctb0661_taxon <- data.table::as.data.table(ctb0661_taxon)
ctb0661_taxon[, id := paste0("ctb0661-", ctb0661_taxon$evento_id_febr)]
ctb0661_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0661. The id columns are the same.
soildata[ctb0661_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0661_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0769
# Pre 1999 soil classification system updated by experts
ctb0769_taxon <- febr::observation("ctb0769", "taxon")
ctb0769_taxon <- data.table::as.data.table(ctb0769_taxon)
ctb0769_taxon[, id := paste0("ctb0769-", ctb0769_taxon$evento_id_febr)]
ctb0769_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0769. The id columns are the same.
soildata[ctb0769_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0769_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

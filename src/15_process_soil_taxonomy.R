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

# Datasets with missing taxon_sibcs data
no_taxon <- c("ctb0033", "ctb0035", "ctb0053", "ctb0055", "ctb0059")

# Check which datasets are missing the taxon_sibcs data
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0657
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0657_taxon <- febr::observation("ctb0657", c("taxon", "sibcs"))
ctb0657_taxon <- data.table::as.data.table(ctb0657_taxon)
ctb0657_taxon[, id := paste0("ctb0657-", ctb0657_taxon$evento_id_febr)]
ctb0657_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0657_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0657. The id columns are the same.
soildata[ctb0657_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0657_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0572
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0572_taxon <- febr::observation("ctb0572", c("taxon", "sibcs"))
ctb0572_taxon <- data.table::as.data.table(ctb0572_taxon)
ctb0572_taxon[, id := paste0("ctb0572-", ctb0572_taxon$evento_id_febr)]
ctb0572_taxon[, all(taxon_sibcs_200X != sibcs_19xx)]
ctb0572_taxon[, taxon_sibcs := taxon_sibcs_200X]
# Update the soildata with the taxon_sibcs from ctb0572. The id columns are the same.
soildata[ctb0572_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0572_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0829
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0829_taxon <- febr::observation("ctb0829", c("taxon", "sibcs"))
ctb0829_taxon <- data.table::as.data.table(ctb0829_taxon)
ctb0829_taxon[, id := paste0("ctb0829-", ctb0829_taxon$evento_id_febr)]
ctb0829_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0829_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0829. The id columns are the same.
soildata[ctb0829_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0829_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0770
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0770_taxon <- febr::observation("ctb0770", c("taxon", "sibcs"))
ctb0770_taxon <- data.table::as.data.table(ctb0770_taxon)
ctb0770_taxon[, id := paste0("ctb0770-", ctb0770_taxon$evento_id_febr)]
ctb0770_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0770_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0770. The id columns are the same.
soildata[ctb0770_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0770_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0631
# Pre 1999 soil classification system
ctb0631_taxon <- febr::observation("ctb0631", c("taxon", "sibcs"))
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
ctb0661_taxon <- febr::observation("ctb0661", c("taxon", "sibcs"))
ctb0661_taxon <- data.table::as.data.table(ctb0661_taxon)
ctb0661_taxon[, id := paste0("ctb0661-", ctb0661_taxon$evento_id_febr)]
ctb0661_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0661. The id columns are the same.
soildata[ctb0661_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0661_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0769
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0769_taxon <- febr::observation("ctb0769", c("taxon", "sibcs"))
ctb0769_taxon <- data.table::as.data.table(ctb0769_taxon)
ctb0769_taxon[, id := paste0("ctb0769-", ctb0769_taxon$evento_id_febr)]
ctb0769_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0769_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0769. The id columns are the same.
soildata[ctb0769_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0769_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0677
# Pre 1999 soil classification system
ctb0677_taxon <- febr::observation("ctb0677", c("taxon", "sibcs"))
ctb0677_taxon <- data.table::as.data.table(ctb0677_taxon)
ctb0677_taxon[, id := paste0("ctb0677-", ctb0677_taxon$evento_id_febr)]
ctb0677_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0677. The id columns are the same.
soildata[ctb0677_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0677_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0607
# Pre 1999 soil classification system
ctb0607_taxon <- febr::observation("ctb0607", c("taxon", "sibcs"))
ctb0607_taxon <- data.table::as.data.table(ctb0607_taxon)
ctb0607_taxon[, id := paste0("ctb0607-", ctb0607_taxon$evento_id_febr)]
ctb0607_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0607. The id columns are the same.
soildata[ctb0607_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0607_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0832
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0832_taxon <- febr::observation("ctb0832", c("taxon", "sibcs"))
ctb0832_taxon <- data.table::as.data.table(ctb0832_taxon)
ctb0832_taxon[, id := paste0("ctb0832-", ctb0832_taxon$evento_id_febr)]
ctb0832_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0832_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0832. The id columns are the same.
soildata[ctb0832_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0832_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0645
# Pre 1999 soil classification system
ctb0645_taxon <- febr::observation("ctb0645", c("taxon", "sibcs"))
ctb0645_taxon <- data.table::as.data.table(ctb0645_taxon)
ctb0645_taxon[, id := paste0("ctb0645-", ctb0645_taxon$evento_id_febr)]
ctb0645_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0645. The id columns are the same.
soildata[ctb0645_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0645_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0713
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0713_taxon <- febr::observation("ctb0713", c("taxon", "sibcs"))
ctb0713_taxon <- data.table::as.data.table(ctb0713_taxon)
ctb0713_taxon[, id := paste0("ctb0713-", ctb0713_taxon$evento_id_febr)]
ctb0713_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0713_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0713. The id columns are the same.
soildata[ctb0713_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0713_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0787
# Pre 1999 soil classification system
ctb0787_taxon <- febr::observation("ctb0787", c("taxon", "sibcs"))
ctb0787_taxon <- data.table::as.data.table(ctb0787_taxon)
ctb0787_taxon[, id := paste0("ctb0787-", ctb0787_taxon$evento_id_febr)]
ctb0787_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0787. The id columns are the same.
soildata[ctb0787_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0787_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0678
# Pre 1999 soil classification system PARTIALLY updated by experts
ctb0678_taxon <- febr::observation("ctb0678", c("taxon", "sibcs"))
ctb0678_taxon <- data.table::as.data.table(ctb0678_taxon)
ctb0678_taxon[, id := paste0("ctb0678-", ctb0678_taxon$evento_id_febr)]
ctb0678_taxon[, all(sibcs_20xx != sibcs_19xx)]
ctb0678_taxon[, taxon_sibcs := sibcs_20xx]
# Update the soildata with the taxon_sibcs from ctb0678. The id columns are the same.
soildata[ctb0678_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0678_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0775
# Pre 1999 soil classification system updated by experts
ctb0775_taxon <- febr::observation("ctb0775", c("taxon", "sibcs"))
ctb0775_taxon <- data.table::as.data.table(ctb0775_taxon)
ctb0775_taxon[, id := paste0("ctb0775-", ctb0775_taxon$evento_id_febr)]
ctb0775_taxon[, all(taxon_sibcs_xxx != taxon_sibcs_xxx.1)]
ctb0775_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0775. The id columns are the same.
soildata[ctb0775_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0775_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0811
# Pre 1999 soil classification system
ctb0811_taxon <- febr::observation("ctb0811", c("taxon", "sibcs"))
ctb0811_taxon <- data.table::as.data.table(ctb0811_taxon)
ctb0811_taxon[, id := paste0("ctb0811-", ctb0811_taxon$evento_id_febr)]
ctb0811_taxon[, all(taxon_sibcs_xxx == taxon_sibcs_xxx.1)]
ctb0811_taxon[, taxon_sibcs := taxon_sibcs_xxx]
# Update the soildata with the taxon_sibcs from ctb0811. The id columns are the same.
soildata[ctb0811_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0811_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0646
# Pre 1999 soil classification system
ctb0646_taxon <- febr::observation("ctb0646", c("taxon", "sibcs"))
ctb0646_taxon <- data.table::as.data.table(ctb0646_taxon)
ctb0646_taxon[, id := paste0("ctb0646-", ctb0646_taxon$evento_id_febr)]
ctb0646_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0646. The id columns are the same.
soildata[ctb0646_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0646_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0717
# Pre 1999 soil classification system updated by experts
ctb0717_taxon <- febr::observation("ctb0717", c("taxon", "sibcs"))
ctb0717_taxon <- data.table::as.data.table(ctb0717_taxon)
ctb0717_taxon[, id := paste0("ctb0717-", ctb0717_taxon$evento_id_febr)]
ctb0717_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0717. The id columns are the same.
soildata[ctb0717_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0717_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0667
# Pre 1999 soil classification system
ctb0667_taxon <- febr::observation("ctb0667", c("taxon", "sibcs"))
ctb0667_taxon <- data.table::as.data.table(ctb0667_taxon)
ctb0667_taxon[, id := paste0("ctb0667-", ctb0667_taxon$evento_id_febr)]
ctb0667_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0667. The id columns are the same.
soildata[ctb0667_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0667_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0617
# Pre 1999 soil classification system
ctb0617_taxon <- febr::observation("ctb0617", c("taxon", "sibcs"))
ctb0617_taxon <- data.table::as.data.table(ctb0617_taxon)
ctb0617_taxon[, id := paste0("ctb0617-", ctb0617_taxon$evento_id_febr)]
ctb0617_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0617. The id columns are the same.
soildata[ctb0617_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0617_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0752
# Pre 1999 soil classification system updated by experts
ctb0752_taxon <- febr::observation("ctb0752", c("taxon", "sibcs"))
ctb0752_taxon <- data.table::as.data.table(ctb0752_taxon)
ctb0752_taxon[, id := paste0("ctb0752-", ctb0752_taxon$evento_id_febr)]
ctb0752_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0752. The id columns are the same.
soildata[ctb0752_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0752_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0635
# Pre 1999 soil classification system
ctb0635_taxon <- febr::observation("ctb0635", c("taxon", "sibcs"))
ctb0635_taxon <- data.table::as.data.table(ctb0635_taxon)
ctb0635_taxon[, id := paste0("ctb0635-", ctb0635_taxon$evento_id_febr)]
ctb0635_taxon[, taxon_sibcs := sibcs_19xx]
# Update the soildata with the taxon_sibcs from ctb0635. The id columns are the same.
soildata[ctb0635_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0635_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0574
# Pre 1999 soil classification system
ctb0574_taxon <- febr::observation("ctb0574", c("taxon", "sibcs"))
ctb0574_taxon <- data.table::as.data.table(ctb0574_taxon)
ctb0574_taxon[, id := paste0("ctb0574-", ctb0574_taxon$evento_id_febr)]
ctb0574_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0574. The id columns are the same.
soildata[ctb0574_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0574_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0662
# Pre 1999 soil classification system
ctb0662_taxon <- febr::observation("ctb0662", c("taxon", "sibcs"))
ctb0662_taxon <- data.table::as.data.table(ctb0662_taxon)
ctb0662_taxon[, id := paste0("ctb0662-", ctb0662_taxon$evento_id_febr)]
ctb0662_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0662. The id columns are the same.
soildata[ctb0662_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0662_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0664
# Pre 1999 soil classification system
ctb0664_taxon <- febr::observation("ctb0664", c("taxon", "sibcs"))
ctb0664_taxon <- data.table::as.data.table(ctb0664_taxon)
ctb0664_taxon[, id := paste0("ctb0664-", ctb0664_taxon$evento_id_febr)]
ctb0664_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0664. The id columns are the same.
soildata[ctb0664_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0664_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# ctb0605
# Pre 1999 soil classification system
ctb0605_taxon <- febr::observation("ctb0605", c("taxon", "sibcs"))
ctb0605_taxon <- data.table::as.data.table(ctb0605_taxon)
ctb0605_taxon[, id := paste0("ctb0605-", ctb0605_taxon$evento_id_febr)]
ctb0605_taxon[, taxon_sibcs := taxon_sibcs_xxx.1]
# Update the soildata with the taxon_sibcs from ctb0605. The id columns are the same.
soildata[ctb0605_taxon, taxon_sibcs := i.taxon_sibcs, on = "id"]
rm(ctb0605_taxon)

# Check if the taxon_sibcs was updated
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# continue...

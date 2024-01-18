# title: "Brazilian Soil Dataset 2023"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2024-01-18"
#
# Summary
# This R script handles the 'identification' obtained after integrating each of the
# 'identification'tables extracted from datasets available in the Brazilian Soil Data Repository
# (FEBR/SoilData). It computes the number of datasets and the number of occurrences of each license.

# Read the processed data from the 'identificacao' table
identificacao <- read.table("data/identificacao.txt", sep = ";", dec = ",", header = TRUE)

# Count the number of datasets in the 'identificacao' table
n_datasets <- nrow(identificacao)
n_datasets
# There are 236 datasets in the 'identificacao' table.

# Count the number of occurrences of each license in the 'identificacao' table
table(identificacao$dataset_licenca)
# There are 75 datasets with the CC BY license and 161 datasets with the CC BY-NC license.

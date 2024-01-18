# title: "Brazilian Soil Dataset 2023"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2024-01-18"

# Summary
# R script that reads the three separate data tables named 'dataset', 'observacao', and 'camada'
# from text files, merges them into a single table using common identifiers, and then writes this
# merged table back into a text file. The goal of this script is to consolidate multiple data
# sources into a single, comprehensive dataset for easier analysis and manipulation.

# Read tables 'identificacao', 'observacao' and 'camada'
dataset <- read.table("data/identificacao.txt", sep = ";", dec = ",", header = TRUE)
observacao <- read.table("data/observacao.txt", sep = ";", dec = ",", header = TRUE)
camada <- read.table("data/camada.txt", sep = ";", dec = ",", header = TRUE)

# Merge tables 'dataset', 'observacao' and 'camada'
febr <- 
  merge(dataset, observacao, by = "dataset_id") %>% 
  merge(camada, by = c("dataset_id", "observacao_id"))

# Save data in TXT format
write.table(febr,
  file = "data/brazilian-soil-dataset.txt", sep = ";", dec = ",", row.names = FALSE
)

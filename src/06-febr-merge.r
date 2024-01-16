# title: "Brazilian Soil Dataset 2020"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

# Summary
# R script that reads the three separate data tables named 'dataset', 'observacao', and 'camada'
# from text files, merges them into a single table using common identifiers, and then writes this
# merged table back into a text file. The goal of this script is to consolidate multiple data
# sources into a single, comprehensive dataset for easier analysis and manipulation.

# Read tables 'dataset', 'observacao' and 'camada'
dataset <- read.table("../data/febr-dataset.txt", sep = ";", dec = ",", header = TRUE)
observacao <- read.table("../data/febr-observacao.txt", sep = ";", dec = ",", header = TRUE)
camada <- read.table("../data/febr-camada.txt", sep = ";", dec = ",", header = TRUE)

# Merge tables 'dataset', 'observacao' and 'camada'
febr <- 
  merge(dataset, observacao, by = "dataset_id") %>% 
  merge(camada, by = c("dataset_id", "observacao_id"))

# Save data in TXT format
write.table(febr,
  file = "../data/febr-superconjunto.txt", sep = ";", dec = ",", row.names = FALSE
)

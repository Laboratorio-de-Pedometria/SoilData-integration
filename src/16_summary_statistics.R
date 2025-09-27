# title: SoilData Integration
# subtitle: Generate summary statistics
# author: Alessandro Samuel-Rosa
# date: 2025
# licence: MIT
# summary: This script generates summary statistics for the Brazilian Soil Dataset. It reads the
#          processed soil data, calculates key statistics such as the number of unique datasets,
#          total observations, and distributions of soil properties. The script also creates visual
#          representations of these statistics, including histograms and boxplots, to facilitate
#          data interpretation. Finally, it saves the summary statistics and plots to files for
#          further analysis and reporting.

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

# Read the Brazilian Soil Dataset v2023
# Check if file "data/00_brazilian_soil_dataset_2023.txt" exists. If not, read the Brazilian 
# Soil Dataset v2023 from the SoilData repository using the 'dataverse' package. Next, write the 
# file to 'data/00_brazilian_soil_dataset_2023.txt'. The dataset is available at
# https://doi.org/10.60502/SoilData/TUI25K. If the file already exists, read it using the
# 'data.table' package.
file_path <- "data/00_brazilian_soil_dataset_2023.txt"
if (!file.exists(file_path)) {
  br_soil2023 <- dataverse::get_dataframe_by_name("brazilian-soil-dataset-2023.txt",
    server = "https://soildata.mapbiomas.org/dataverse/soildata",
    dataset = "10.60502/SoilData/TUI25K", .f = data.table::fread
  )
  data.table::fwrite(br_soil2023, file_path, dec = ".", sep = ";")
} else {
  br_soil2023 <- data.table::fread(file_path, dec = ".", sep = ";")
}

# Read SoilData data processed in the previous step
soildata <- data.table::fread("data/15_soildata.txt", sep = "\t", na.strings = c("NA", ""))

# all columns in br_soil2023 are in soildata?
all(colnames(br_soil2023) %in% colnames(soildata))

# which columns in soildata are not in br_soil2023?
setdiff(colnames(soildata), colnames(br_soil2023))

summary_soildata(soildata)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255

# Drop unwanted columns from soildata:
# acidez_caoac2ph7_naoh aluminio_kcl_naoh aluminio_saturacao_calc area_conhecimento autor_nome bases_saturacao_calc bases_soma_calc calcio_kcl_eaa campanha_id_febr ctce_soma_calc data_coleta_dia data_coleta_mes dataset_versao erosao_presenca erosao_tipo fosforo_mehlich1_eam magnesio_kcl_eaa palavras_chave potassio_mehlich1_eeac publicacao_data relevo_exposicao relevo_local relevo_posicao observacao_data
soildata <- soildata[, -c(
  "acidez_caoac2ph7_naoh", "aluminio_kcl_naoh", "aluminio_saturacao_calc", "area_conhecimento",
  "autor_nome", "bases_saturacao_calc", "bases_soma_calc", "calcio_kcl_eaa", "campanha_id_febr",
  "ctce_soma_calc", "data_coleta_dia", "data_coleta_mes", "dataset_versao", "erosao_presenca",
  "erosao_presenca", "erosao_tipo", "fosforo_mehlich1_eam", "magnesio_kcl_eaa",
  "palavras_chave", "potassio_mehlich1_eeac", "publicacao_data", "relevo_exposicao",
  "relevo_local", "relevo_posicao", "observacao_data"
)]

# Rename 'data_fonte' to 'ano_fonte' to avoid confusion with 'dados_fonte' used somewere else
setnames(soildata, "data_fonte", "ano_fonte")

paste(colnames(soildata), collapse = ", ")
# dataset_id, observacao_id, dataset_titulo, organizacao_nome, dataset_licenca, sisb_id, ibge_id, coord_x, coord_y, coord_precisao, coord_fonte, pais_id, estado_id, municipio_id, amostra_tipo, amostra_quanti, amostra_area, taxon_sibcs, taxon_st, taxon_wrb, camada_id, amostra_id, camada_nome, profund_sup, profund_inf, terrafina, argila, silte, areia, carbono, ctc, ph, dsi, ce, data_ano, id, ano_fonte, coord_datum, esqueleto
# View(soildata)

# Summary statistics ##############################################################################
# estado_id
soildata[, .N, by = .(estado_id)][order(estado_id)]

# ano_fonte
soildata[, .N, by = .(ano_fonte)][order(ano_fonte)]

# data_ano
soildata[, .N, by = .(data_ano)][order(data_ano)]

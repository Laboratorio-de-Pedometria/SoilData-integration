# title: SoilData Integration
# subtitle: 
# author: Alessandro Samuel-Rosa
# date: 2025
# licence: MIT
# summary: 

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
  "erosao_presenca", "erosao_tipo", "fosforo_mehlich1_eam", "id", "magnesio_kcl_eaa",
  "palavras_chave", "potassio_mehlich1_eeac", "publicacao_data", "relevo_exposicao",
  "relevo_local", "relevo_posicao", "observacao_data"
)]

# Rename 'data_fonte' to 'ano_fonte' to avoid confusion with 'dados_fonte' used somewere else
setnames(soildata, "data_fonte", "ano_fonte")

paste(colnames(soildata), collapse = ", ")
# dataset_id, observacao_id, dataset_titulo, organizacao_nome, dataset_licenca, sisb_id, ibge_id, coord_x, coord_y, coord_precisao, coord_fonte, pais_id, estado_id, municipio_id, amostra_tipo, amostra_quanti, amostra_area, taxon_sibcs, taxon_st, taxon_wrb, camada_id, amostra_id, camada_nome, profund_sup, profund_inf, terrafina, argila, silte, areia, carbono, ctc, ph, dsi, ce, data_ano, id, ano_fonte, coord_datum, esqueleto
# View(soildata)

# dataset_id and dataset_titulo
# Should be consistent: the number of unique dataset_id should be equal to the number of unique
# dataset_titulo
if (length(unique(soildata$dataset_id)) > length(unique(soildata$dataset_titulo))) {
  print(soildata[is.na(dataset_titulo) | dataset_titulo == "", unique(dataset_id)])
}


# amostra_tipo
# If amostra_quanti == 1, then amostra_tipo should be "SIMPLES"
soildata[amostra_quanti == 1 & (is.na(amostra_tipo) | amostra_tipo == ""), amostra_tipo := "SIMPLES"]
# If amostra_quanti > 1 and amostra_tipo is NA, then amostra_tipo should be "COMPOSTA"
soildata[amostra_quanti > 1 & is.na(amostra_tipo), amostra_tipo := "COMPOSTA"]
# statistics
soildata[, .N, by = .(amostra_tipo)][order(amostra_tipo)]

# amostra_quanti
# If amostra_tipo is "SIMPLES" and amostra_quanti is NA, then amostra_quanti should be 1
soildata[amostra_tipo == "SIMPLES" & is.na(amostra_quanti), amostra_quanti := 1]
# statistics
soildata[, .N, by = .(amostra_quanti)][order(amostra_quanti)]
# cross table between amostra_tipo and amostra_quanti
table(soildata$amostra_tipo, soildata$amostra_quanti, useNA = "ifany")

# coord_datum
# If coord_datum is not NA, but coord_x or coord_y are NA, set coord_datum to NA
soildata[!is.na(coord_datum) & (is.na(coord_x) | is.na(coord_y)), coord_datum := NA_character_]
# If coord_datum == "", set to NA
soildata[coord_datum == "", coord_datum := NA_character_]
# If coord_datum == 4326, set to "EPSG:4326"
soildata[coord_datum == "4326", coord_datum := "EPSG:4326"]

# Minimum soil density (dsi)
dsi_min <- 0.5
dsi_min_taxon <- "Organossolo|Húmico|ORGÂNICO|Hístico"
dsi_min_layer <- "O|H|Bw"
# Check if the minimum dsi is greater than or equal to 0.5
min(soildata$dsi, na.rm = TRUE) >= dsi_min
# Check for dsi values less than 0.5 and not in "Organossolo|Húmico" soils or in "O|H" layers
soildata[
  dsi < dsi_min & !grepl(dsi_min_taxon, taxon_sibcs, ignore.case = TRUE) &
  !grepl(dsi_min_layer, camada_nome, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, dsi, ctc, carbono, id)
]
# Set those dsi values to NA
soildata[
  dsi < dsi_min & !grepl(dsi_min_taxon, taxon_sibcs, ignore.case = TRUE) &
  !grepl(dsi_min_layer, camada_nome, ignore.case = TRUE),
  dsi := NA_real_
]

# Maximum soil density (dsi)
dsi_max <- 2L
dsi_max_taxon <- "VERTISSOLO|vértico|FLÚVICO"
dsi_max_layer <- "v|t|Cg|Cn"
# Check if the maximum dsi is less than or equal to 2
max(soildata$dsi, na.rm = TRUE) <= dsi_max
# Check for dsi values greater than 2 and not in "VERTISSOLO" soils or in "v|t" layers
soildata[
  dsi > dsi_max & !grepl(dsi_max_taxon, taxon_sibcs, ignore.case = TRUE) &
  !grepl(dsi_max_layer, camada_nome, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, dsi, areia, carbono, id)
]
# The values appear to be correct, so we will not change them.

# Maximum pH
ph_max <- 9L
ph_max_taxon <- "carbonático|NÁTRICO|Sódico|SOLODIZADO"
# Check if the maximum pH is less than or equal to 9
max(soildata$ph, na.rm = TRUE) <= ph_max
# Check for pH values greater than 9 and not in "carbonático|NÁTRICO|Sódico|SOLODIZADO" soils
soildata[
  ph > ph_max & !grepl(ph_max_taxon, taxon_sibcs, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, ph, ctc, carbono, id)
]
# Set those pH values to NA
soildata[
  ph > ph_max & !grepl(ph_max_taxon, taxon_sibcs, ignore.case = TRUE),
  ph := NA_real_
]

# Minimum pH
ph_min <- 2L
ph_min_taxon <- "TIOMÓRFICO|Húmico"
# Check if the minimum pH is greater than or equal to 2
min(soildata$ph, na.rm = TRUE) >= ph_min
# Check for pH values less than 2 and not in "Tiomórfico|Húmico" soils
soildata[
  ph < ph_min & !grepl(ph_min_taxon, taxon_sibcs, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, ph, ctc, carbono, id)
]
# Set those pH values to NA
soildata[
  ph < ph_min & !grepl(ph_min_taxon, taxon_sibcs, ignore.case = TRUE),
  ph := NA_real_
]

# Sum of terrafina and esqueleto == 100
soildata[, terrafina := round(terrafina / 10)]
soildata[, esqueleto := round(esqueleto / 10)]
# Check if the sum is 100
all((soildata$terrafina + soildata$esqueleto) == 100, na.rm = TRUE)

# Sum clay, silt and sand
# Check if the sum is 100
all((soildata$argila + soildata$silte + soildata$areia) == 100, na.rm = TRUE)

# estado_id
# Check if the number of unique values in estado_id is 27 (26 states + Federal District)
soildata[estado_id == "", estado_id := NA_character_]
length(unique(na.omit(soildata$estado_id))) == 27





# Summary statistics ##############################################################################
# estado_id
soildata[, .N, by = .(estado_id)][order(estado_id)]

# ano_fonte
soildata[, .N, by = .(ano_fonte)][order(ano_fonte)]

# data_ano
soildata[, .N, by = .(data_ano)][order(data_ano)]

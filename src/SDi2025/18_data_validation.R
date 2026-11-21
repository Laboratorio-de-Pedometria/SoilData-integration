# title: SoilData Integration
# subtitle: Validate data
# author: Alessandro Rosa
# date: 2026
# licence: MIT
# description: This script validates and cleans the integrated SoilData
# dataset, applies consistency checks, and exports the validated result.
rm(list = ls())

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Load required packages
library(data.table)
library(sf)
library(geobr)
if (!requireNamespace("dataverse", quietly = TRUE)) {
  install.packages("dataverse")
}

# Read Brazilian state boundaries
brazil <- read_brazil_states()

# Read last version ############################################################
# Read the Brazilian Soil Dataset v2024 as a schema reference. The comparisons
# below check which v2024 columns are retained in the current integration and
# which columns have been added; v2024 is not used as a source for validation.
# The dataset is available at https://doi.org/10.60502/SoilData/BCAV2B.
file_path <- "res/tab/brazilian-soil-dataset-2024.txt"
if (!file.exists(file_path)) {
  soildata_last <- dataverse::get_dataframe_by_name(
    filename = "brazilian-soil-dataset-2024.txt",
    server = "https://repositorio.soildata.mapbiomas.org/dataverse/soildata",
    dataset = "10.60502/SoilData/BCAV2B",
    .f = data.table::fread
  )
  data.table::fwrite(soildata_last, file_path, dec = ".", sep = ";")
} else {
  soildata_last <- data.table::fread(file_path, dec = ".", sep = ";")
}

# Read the current version #####################################################
# Read SoilData data processed in the previous script
soildata_current <- data.table::fread(
  "data/17_soildata.txt",
  sep = "\t", na.strings = c("NA", "")
)

# Check columns ################################################################
# All columns in soildata_last are in soildata_current?
all(colnames(soildata_last) %in% colnames(soildata_current))

# Which columns in soildata_current are not in soildata_last?
sort(setdiff(colnames(soildata_current), colnames(soildata_last)))

# Drop unwanted columns from current soildata ##################################
soildata <- soildata_current[, -c(
  "acidez_caoac2ph7_naoh", "aluminio_kcl_naoh", "aluminio_saturacao_calc",
  "area_conhecimento", "autor_nome", "bases_saturacao_calc", "bases_soma_calc",
  "calcio_kcl_eaa", "campanha_id_febr", "ctce_soma_calc", "data_coleta_dia",
  "data_coleta_mes", "dataset_versao", "erosao_presenca", "erosao_presenca",
  "erosao_tipo", "fosforo_mehlich1_eam", "id", "id_original", "magnesio_kcl_eaa",
  "organizacao_nome", "palavras_chave", "potassio_mehlich1_eeac", "publicacao_data",
  "relevo_exposicao", "relevo_local", "relevo_posicao", "observacao_data"
)]

# Rename columns ###############################################################
# data_ano_fonte -> data_fonte
setnames(soildata, "data_ano_fonte", "data_fonte")
# Check column names, including the soil-property source columns
paste(colnames(soildata), collapse = ", ")
if (FALSE) {
  View(soildata)
}

# dataset_id and dataset_titulo ################################################
# Should be consistent: the number of unique dataset_id should be equal to the
# number of unique dataset_titulo
if (length(unique(soildata$dataset_id)) > length(unique(soildata$dataset_titulo))) {
  print(soildata[is.na(dataset_titulo) | dataset_titulo == "", unique(dataset_id)])
} else {
  message("dataset_id and dataset_titulo are consistent.")
}

# amostra_tipo #################################################################
# If amostra_quanti == 1, then amostra_tipo should be "SIMPLES"
soildata[
  amostra_quanti == 1 & (is.na(amostra_tipo) | amostra_tipo == ""),
  amostra_tipo := "SIMPLES"
]
# If amostra_quanti > 1 and amostra_tipo is NA, then amostra_tipo should be "COMPOSTA"
soildata[amostra_quanti > 1 & is.na(amostra_tipo), amostra_tipo := "COMPOSTA"]
# statistics
soildata[, .N, by = .(amostra_tipo)][order(amostra_tipo)]
#    amostra_tipo     N
#          <char> <int>
# 1:              13552
# 2:     COMPOSTA   429
# 3:      SIMPLES 48155

# amostra_quanti ###############################################################
# If amostra_tipo is "SIMPLES" and amostra_quanti is NA, then amostra_quanti 
# should be 1
soildata[amostra_tipo == "SIMPLES" & is.na(amostra_quanti), amostra_quanti := 1]
# statistics
soildata[, .N, by = .(amostra_quanti)][order(amostra_quanti)]
#    amostra_quanti     N
#             <int> <int>
# 1:              1 48155
# 2:              3   425
# 3:             NA 13556
# cross table between amostra_tipo and amostra_quanti
table(soildata$amostra_tipo, soildata$amostra_quanti, useNA = "ifany")
  #              1     3  <NA>
  #              0     0 13552
  # COMPOSTA     0   425     4
  # SIMPLES  48155     0     0

# dataset_licenca ##############################################################
# if "CC-BY-4.0", then "CC BY 4.0"
soildata[dataset_licenca == "CC-BY-4.0", dataset_licenca := "CC BY 4.0"]
# if "CC BY-NC", then "CC BY-NC 4.0"
soildata[dataset_licenca == "CC BY-NC", dataset_licenca := "CC BY-NC 4.0"]
# if OpenData: Creative Commons Attribution 4.0 (CC BY 4.0), then "CC BY 4.0"
soildata[dataset_licenca == "OpenData: Creative Commons Attribution 4.0 (CC BY 4.0)", dataset_licenca := "CC BY 4.0"]
# if notOpenData: Creative Commons Attribution-NonCommercial (CC BY-NC), then
# "CC BY-NC 4.0"
soildata[dataset_licenca == "notOpenData: Creative Commons Attribution-NonCommercial (CC BY-NC)", dataset_licenca := "CC BY-NC 4.0"]
# CC BY 4.0 Atribuição -> "CC BY 4.0"
soildata[dataset_licenca == "CC BY 4.0 Atribuição", dataset_licenca := "CC BY 4.0"]
# CC BY-NC 4.0 Atribuição-NãoComercial -> "CC BY-NC 4.0"
soildata[dataset_licenca == "CC BY-NC 4.0 Atribuição-NãoComercial", dataset_licenca := "CC BY-NC 4.0"]
#  CC BY-NC-SA 4.0 Atribuição-NãoComercial-CompartilhaIgual -> "CC BY-NC-SA 4.0"
soildata[dataset_licenca == "CC BY-NC-SA 4.0 Atribuição-NãoComercial-CompartilhaIgual", dataset_licenca := "CC BY-NC-SA 4.0"]
# CC-BY -> "CC BY 4.0"
soildata[dataset_licenca == "CC-BY", dataset_licenca := "CC BY 4.0"]
# statistics
soildata[, .N, by = .(dataset_licenca)][order(dataset_licenca)]
#    dataset_licenca     N
#             <char> <int>
# 1:       CC BY 4.0 31469
# 2:    CC BY-NC 4.0 30531
# 3: CC BY-NC-SA 4.0   136

# pais_id ######################################################################
# if BR, then Brasil
soildata[pais_id == "BR", pais_id := "Brasil"]
# if NA of "", then Brasil
soildata[is.na(pais_id) | pais_id == "", pais_id := "Brasil"]
# statistics
soildata[, .N, by = .(pais_id)][order(pais_id)]

# estado_id ####################################################################
# Rio de Janeiro -> RJ
soildata[estado_id == "Rio de Janeiro", estado_id := "RJ"]
# Check if the number of unique values in estado_id is 27 (26 states + Federal 
# District)
soildata[estado_id == "", estado_id := NA_character_]
# statistics
soildata[, .N, by = .(estado_id)][order(estado_id)]

# Intersect with Brazilian states ##############################################
soildata_sf <- unique(soildata[
  is.na(estado_id) & !(is.na(coord_x) | is.na(coord_y)),
  .(dataset_id, observacao_id, coord_x, coord_y)
])
soildata_sf <- sf::st_as_sf(soildata_sf,
  coords = c("coord_x", "coord_y"),
  crs = 4326, remove = FALSE, na.fail = FALSE
)
soildata_sf <- sf::st_transform(soildata_sf, crs = sf::st_crs(brazil))
soildata_sf <- sf::st_join(
  soildata_sf, brazil[, c("abbrev_state")],
  join = sf::st_within, left = TRUE
)
# Extract data.table
soildata_sf <- as.data.table(soildata_sf)[, .(dataset_id, observacao_id, abbrev_state)]
# Update estado_id in soildata, keeping all of its existing values and adding new ones
soildata <- merge(soildata, soildata_sf, by = c("dataset_id", "observacao_id"), all.x = TRUE)
soildata[is.na(estado_id), estado_id := abbrev_state]
soildata[, abbrev_state := NULL]

# Set manual corrections for estado_id based on dataset_id and observacao_id
soildata[
  dataset_id == "ctb0033",
  estado_id := ifelse(is.na(estado_id), "RO", estado_id),
  by = observacao_id
]
soildata[dataset_id == "ctb0053",
  estado_id := ifelse(is.na(estado_id), "RO", estado_id),
  by = observacao_id
]
soildata[
  dataset_id == "ctb0711" & observacao_id == "48",
  estado_id := ifelse(is.na(estado_id), "AM", estado_id)
]
soildata[dataset_id == "ctb0831",
  estado_id := ifelse(is.na(estado_id), "PR", estado_id),
  by = observacao_id
]
# statistics
soildata[, .N, by = .(estado_id)][order(estado_id)]
if (FALSE) {
  # View the data for the rows where estado_id is NA
  View(soildata[is.na(estado_id), .(dataset_id, observacao_id, coord_x, coord_y)])
}
soildata[is.na(estado_id), .(dataset_id, observacao_id,  coord_x, coord_y)]

# municipio_id #################################################################
# Replace "" and "-" with NA
soildata[municipio_id == "", municipio_id := NA_character_]
soildata[municipio_id == "-", municipio_id := NA_character_]
# statistics
soildata[, .N, by = .(municipio_id)]

# coord_datum ##################################################################
# If coord_datum == "", set to NA
soildata[coord_datum == "", coord_datum := NA_character_]
# If coord_datum is not NA, but coord_x or coord_y are NA, set coord_datum to NA
soildata[!is.na(coord_datum) & ((is.na(coord_x) | is.na(coord_y))), coord_datum := NA_character_]
# If coord_datum == 4326, set to "EPSG:4326"
soildata[coord_datum == "4326", coord_datum := "EPSG:4326"]
# If coord_datum == NA but coord_x and coord_y are not NA, set coord_datum to "EPSG:4326"
soildata[is.na(coord_datum) & !is.na(coord_x) & !is.na(coord_y), coord_datum := "EPSG:4326"]
# statistics
soildata[!(is.na(coord_x) | is.na(coord_y)), .N, by = .(coord_datum)][order(coord_datum)]
#    coord_datum     N
#         <char> <int>
# 1:   EPSG:4326 51206

# data_fonte ###################################################################
# If data_fonte == "", set to NA
soildata[data_fonte == "", data_fonte := NA_character_]
# Original -> "original"
soildata[data_fonte == "Original", data_fonte := "original"]
# Warn before assuming a missing source is original.
n_missing_date_source <- soildata[is.na(data_fonte) & !is.na(data_ano), .N]
if (n_missing_date_source > 0L) {
  warning(
    n_missing_date_source,
    " row(s) have a sampling year without a source; setting the source to original."
  )
  soildata[is.na(data_fonte) & !is.na(data_ano), data_fonte := "original"]
}
rm(n_missing_date_source)
# Reject legacy or unexpected labels rather than guessing the estimation method.
invalid_date_sources <- soildata[
  !is.na(data_fonte) & !data_fonte %in% c("original", "especialista", "média"),
  unique(data_fonte)
]
if (length(invalid_date_sources) > 0L) {
  stop("Unexpected sampling-year source(s): ", paste(invalid_date_sources, collapse = ", "))
}
rm(invalid_date_sources)
# statistics
soildata[, .N, by = .(data_fonte)][order(data_fonte)]

# data_ano #####################################################################
# Many datasets still have no information for data_ano (sampling year)
soildata[, .N, by = .(data_ano)][order(data_ano)]

# Minimum soil density #########################################################
dsi_min <- 0.5
dsi_min_taxon <- "Organossolo|Húmico|ORGÂNICO|Hístico"
dsi_min_layer <- "O|H|Bw"
# Check if the minimum dsi is greater than or equal to 0.5
min(soildata$dsi, na.rm = TRUE) >= dsi_min
# Check for dsi values less than 0.5 and not in "Organossolo|Húmico" soils or in "O|H" layers
soildata[
  dsi < dsi_min & !grepl(dsi_min_taxon, taxon_sibcs, ignore.case = TRUE) &
  !grepl(dsi_min_layer, camada_nome, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, dsi, ctc, carbono, dataset_id, observacao_id)
]
# Set those dsi values and their sources to NA: such low values are unjustified
soildata[
  dsi < dsi_min & !grepl(dsi_min_taxon, taxon_sibcs, ignore.case = TRUE) &
  !grepl(dsi_min_layer, camada_nome, ignore.case = TRUE),
  `:=`(dsi = NA_real_, dsi_fonte = NA_character_)
]

# Maximum soil density #########################################################
dsi_max <- 2L
dsi_max_taxon <- "VERTISSOLO|vértico|FLÚVICO"
dsi_max_layer <- "v|t|Cg|Cn"
# Check if the maximum dsi is less than or equal to 2
max(soildata$dsi, na.rm = TRUE) <= dsi_max
# Check for dsi values greater than 2 and not in "VERTISSOLO" soils or in "v|t" layers
soildata[
  dsi > dsi_max & !grepl(dsi_max_taxon, taxon_sibcs, ignore.case = TRUE) &
  !grepl(dsi_max_layer, camada_nome, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, dsi, areia, carbono, dataset_id, observacao_id)
]
# The values appear to be correct, so we will not change them.

# Maximum pH ###################################################################
ph_max <- 9L
ph_max_taxon <- "carbonático|NÁTRICO|Sódico|SOLODIZADO"
# Check if the maximum pH is less than or equal to 9
max(soildata$ph, na.rm = TRUE) <= ph_max
# Check for pH values greater than 9 and not in "carbonático|NÁTRICO|Sódico|SOLODIZADO" soils
soildata[
  ph > ph_max & !grepl(ph_max_taxon, taxon_sibcs, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, ph, ctc, carbono, dataset_id, observacao_id)
]
# Set those pH values and their sources to NA: such high values are unjustified
soildata[
  ph > ph_max & !grepl(ph_max_taxon, taxon_sibcs, ignore.case = TRUE),
  `:=`(ph = NA_real_, ph_fonte = NA_character_)
]

# Minimum pH ###################################################################
ph_min <- 2L
ph_min_taxon <- "TIOMÓRFICO|Húmico"
# Check if the minimum pH is greater than or equal to 2
min(soildata$ph, na.rm = TRUE) >= ph_min
# Check for pH values less than MIN and not in "Tiomórfico|Húmico" soils
soildata[
  ph < ph_min & !grepl(ph_min_taxon, taxon_sibcs, ignore.case = TRUE),
  .(taxon_sibcs, camada_nome, ph, ctc, carbono, dataset_id, observacao_id)
]
# Set those pH values and their sources to NA: such low values are unjustified
soildata[
  ph < ph_min & !grepl(ph_min_taxon, taxon_sibcs, ignore.case = TRUE),
  `:=`(ph = NA_real_, ph_fonte = NA_character_)
]

# Sum of terrafina and esqueleto == 1000 g/kg ##################################
all((soildata$terrafina + soildata$esqueleto) == 1000, na.rm = TRUE)

# Sum clay, silt and sand ######################################################
# Check if the sum is 1000 g/kg
all((soildata$argila + soildata$silte + soildata$areia) == 1000, na.rm = TRUE)

# Minimum ctc ##################################################################
ctc_min <- 1L
ctc_min_taxon <- "FLÚVICO|arênico|Álic|Distrófico|ESPODOS|Aluvia|fraco|Quartzosas|PODZOL|Latos"
ctc_min_layer <- "C|E|A2"
# Check if the minimum ctc is greater than or equal to MIN
min(soildata$ctc, na.rm = TRUE) >= ctc_min
# Check for pH values less than ctc_min and not in ctc_min_taxon soils and ctc_min_layer layers
# and areia < 500 and ph > 5
soildata[
  ctc < ctc_min & !grepl(ctc_min_taxon, soildata$taxon_sibcs, ignore.case = TRUE) &
  !grepl(ctc_min_layer, soildata$camada_nome, ignore.case = FALSE) & areia < 500 & ph > 5,
  .(taxon_sibcs, camada_nome, ctc, ph, carbono, areia, dataset_id, observacao_id)
]
# Set those ctc values and their sources to NA: such low values are unjustified
soildata[
  ctc < ctc_min & !grepl(ctc_min_taxon, soildata$taxon_sibcs, ignore.case = TRUE) &
  !grepl(ctc_min_layer, soildata$camada_nome, ignore.case = FALSE) & areia < 500 & ph > 5,
  `:=`(ctc = NA_real_, ctc_fonte = NA_character_)
]

# Maximum ctc ##################################################################
ctc_max <- 100
ctc_max_taxon <- "Organossolo|Húmico|vertiso|ORGÂNICO"
ctc_max_layer <- "O|H|v|t"
# Check if the maximum ctc is less than or equal to MAX
max(soildata$ctc, na.rm = TRUE) <= ctc_max
# Check for ctc values greater than ctc_max and not in ctc_max_taxon soils and ctc_max_layer layers
soildata[
  ctc > ctc_max & !grepl(ctc_max_taxon, soildata$taxon_sibcs, ignore.case = TRUE) &
  !grepl(ctc_max_layer, soildata$camada_nome, ignore.case = TRUE) & ph < 5,
  .(taxon_sibcs, camada_nome, ctc, ph, carbono, argila, dataset_id, observacao_id)
]
# Manual corrections
soildata[
  dataset_id == "ctb0718" & observacao_id == "55" & camada_nome == "A3",
  ctc := ifelse(ctc == 140.4, 144.4, ctc)
]
# We will not change other values, as they appear to be correct.

# Metadata consistency within events ##########################################
# Check that identification/metadata variables are constant across all layers
# of the same event (dataset_id, observacao_id, data_ano combination)

metadata_vars <- c(
  "dataset_id", "observacao_id", "data_ano", "data_fonte",
  "coord_x", "coord_y", "coord_datum", "coord_fonte", "coord_precisao",
  "estado_id", "municipio_id", "pais_id", "amostra_tipo", "amostra_quanti", 
  "dataset_licenca", "dataset_titulo", "dataset_autor", "taxon_sibcs" 
)

# Check which metadata vars exist in soildata
metadata_vars <- metadata_vars[metadata_vars %in% names(soildata)]

# Identify events where metadata/identification variables vary
event_metadata <- soildata[, .(
  n_layers = .N,
  n_unique_coord_x = uniqueN(coord_x),
  n_unique_coord_y = uniqueN(coord_y),
  n_unique_data_ano = uniqueN(data_ano),
  n_unique_estado_id = uniqueN(estado_id),
  n_unique_municipio_id = uniqueN(municipio_id)
), by = c("dataset_id", "observacao_id")]

# Find events with inconsistent metadata
inconsistent_events <- event_metadata[
  n_unique_coord_x > 1 | n_unique_coord_y > 1 | n_unique_data_ano > 1 |
  n_unique_estado_id > 1 | n_unique_municipio_id > 1
]

if (nrow(inconsistent_events) > 0) {
  cat("\n⚠️  WARNING: Found", nrow(inconsistent_events), 
      "events with inconsistent metadata across layers:\n")
  print(inconsistent_events)
  cat("\nThese events may have layers with conflicting identification information.\n")
  cat("Verify that jittering and coordinate updates maintain consistency.\n\n")
} else {
  cat("\n✓ All events have consistent metadata across layers\n\n")
}

# Export #######################################################################

# Save data in TXT format, saving in "res/tab/"
summary_soildata(soildata)
# SDi2025 ---
# Layers: 62250
# Events: 20259
# Georeference: 16676 (yes) / 3583 (no)
# Date: 20105 (yes) / 154 (no)
# Datasets: 271
# SDi2024 ---
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255
file_path <- "res/tab/brazilian-soil-dataset-2025.txt"
data.table::fwrite(
  soildata,
  file = file_path, sep = "\t", dec = ".", row.names = FALSE, na = "NA"
)
# Load to SoilData repository!

# title: SoilData Integration
# subtitle: Process Soil Taxonomy data
# author: Alessandro Samuel-Rosa
# date: 2025
# licence: MIT
# summary: This script processes and updates soil classification data (taxonomy) for datasets with
#          missing information. It identifies datasets with a significant number of missing
#          taxonomy entries and downloads the relevant data from the FEBR repository. The script
#          then consolidates soil classification information from multiple columns into a single
#          `taxon_sibcs` field, performs extensive cleaning and standardization of the classification
#          strings, and expands common abbreviations. Finally, it merges these updates back into the
#          main `soildata` table and saves the result.
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
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255

# Datasets with missing soil classification or not included in the original FEBR repository
# The following datasets do not have soil classification information (taxon_sibcs) in the current
# soildata or were not included in the old FEBR repository (e.g., ctb0061, ctb0062).
# Thus, we will not try to update the soil classification information for these datasets.
no_taxon <- c(
  "ctb0035", "ctb0053", "ctb0055", "ctb0059", "ctb0024", "ctb0040",
  "ctb0049", "ctb0056", "ctb0057", "ctb0058", "ctb0060", "ctb0061", "ctb0062"
)
# Check which of the remaining datasets are missing soil classification
# We will try to update only these datasets
missing_taxon <- soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "" | is.na(taxon_sibcs),
  .N,
  by = dataset_id
][order(-N)]
# Keep only datasets with more than 50 samples missing soil classification
missing_taxon <- missing_taxon[N > 50]
print(missing_taxon)

# Download data from the FEBR repository
# Check if the file already exists to avoid re-downloading
if (!file.exists("data/febr_taxon_data.rds")) {
  taxon_data <- febr::observation(missing_taxon$dataset_id, c("taxon", "sibcs"))
  taxon_data <- data.table::rbindlist(taxon_data, use.names = TRUE, fill = TRUE)
  saveRDS(taxon_data, "data/febr_taxon_data.rds")
} else {
  taxon_data <- readRDS("data/febr_taxon_data.rds")
}
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
# 9. taxon_sibcs_19xx
# 10. taxon_sibcs_xxx
# Helper to safely extract column if it exists, otherwise return NA
get_col <- function(dt, col) {
  if (col %in% names(dt)) dt[[col]] else rep(NA_character_, nrow(dt))
}
taxon_data[, taxon_sibcs := fifelse(!is.na(get_col(.SD, "sibcs_2013")), get_col(.SD, "sibcs_2013"),
  fifelse(!is.na(get_col(.SD, "sibcs_2006")), get_col(.SD, "sibcs_2006"),
    fifelse(!is.na(get_col(.SD, "taxon_sibcs_200X")), get_col(.SD, "taxon_sibcs_200X"),
      fifelse(!is.na(get_col(.SD, "taxon_sibcs_200x")), get_col(.SD, "taxon_sibcs_200x"),
        fifelse(!is.na(get_col(.SD, "sibcs_20xx")), get_col(.SD, "sibcs_20xx"),
          fifelse(!is.na(get_col(.SD, "taxon_sibcs_xxx.1")), get_col(.SD, "taxon_sibcs_xxx.1"),
            fifelse(!is.na(get_col(.SD, "sibcs_1999")), get_col(.SD, "sibcs_1999"),
              fifelse(!is.na(get_col(.SD, "sibcs_19xx")), get_col(.SD, "sibcs_19xx"),
                fifelse(!is.na(get_col(.SD, "taxon_sibcs_19xx")), get_col(.SD, "taxon_sibcs_19xx"),
                  fifelse(!is.na(get_col(.SD, "taxon_sibcs_xxx")), get_col(.SD, "taxon_sibcs_xxx"), NA_character_)
                )
              )
            )
          )
        )
      )
    )
  )
)]
# Check the new taxon_sibcs column by dataset
taxon_data[taxon_sibcs == "" | is.na(taxon_sibcs), .N, by = dataset_id][order(-N)]

# Clean soil classification data
# Keep only the strings before the single capital A
taxon_data[, taxon_sibcs := gsub(" A .*", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub(" a .*", "", taxon_sibcs)]
# Keep only the strings before the single "A."
taxon_data[, taxon_sibcs := gsub(" A\\.", "", taxon_sibcs)]
# Keep only the strings before the string " ou "
taxon_data[, taxon_sibcs := gsub(" ou .*", "", taxon_sibcs)]
# Keep only the strings before the word "textura", "Textura", or "texutra"
taxon_data[, taxon_sibcs := gsub("textura.*", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("Textura.*", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("texutra.*", "", taxon_sibcs)]
# Keep only the strings before the word "(?)"
taxon_data[, taxon_sibcs := gsub("\\(\\?\\).*", "", taxon_sibcs)]
# Keep only the strings before the word "fase"
taxon_data[, taxon_sibcs := gsub("fase.*", "", taxon_sibcs)]
# Keep only the strings before the word "floresta"
taxon_data[, taxon_sibcs := gsub("floresta.*", "", taxon_sibcs)]
# Keep only the strings before the word "relevo"
taxon_data[, taxon_sibcs := gsub("relevo.*", "", taxon_sibcs)]
# Replace "t?ofico" with "trófico"
taxon_data[, taxon_sibcs := gsub("t?ofico", "trófico", taxon_sibcs)]
# Replace "VERMELHO ? AMARELO" with "VERMELHO-AMARELO"
taxon_data[, taxon_sibcs := gsub("VERMELHO ? AMARELO", "VERMELHO-AMARELO", taxon_sibcs)]
# Replace "argila - de " with "argila de"
taxon_data[, taxon_sibcs := gsub("argila - de ", "argila de", taxon_sibcs)]
# Replace "EUTRÓFICO&#10;Tb" with "EUTRÓFICO Tb"
taxon_data[, taxon_sibcs := gsub("EUTRÓFICO&#10;Tb", "EUTRÓFICO Tb", taxon_sibcs)]
# Replace "EQUIVALENTE&#10;EUTRÓFICO" with "EQUIVALENTE EUTRÓFICO"
taxon_data[, taxon_sibcs := gsub("EQUIVALENTE&#10;EUTRÓFICO", "EQUIVALENTE EUTRÓFICO", taxon_sibcs)]
# Replace "Alítico &#10;" with "Alítico"
taxon_data[, taxon_sibcs := gsub("Alítico &#10;", "Alítico", taxon_sibcs)]
# Replace "Distrófico&#10;" with "Distrófico"
taxon_data[, taxon_sibcs := gsub("Distrófico&#10;", "Distrófico", taxon_sibcs)]
# Replace "Eutrófico&#10;" with "Eutrófico"
taxon_data[, taxon_sibcs := gsub("Eutrófico&#10;", "Eutrófico", taxon_sibcs)]
# Replace "VERMELHO-AMARELO-Ortox" with "VERMELHO-AMARELO Orto"
taxon_data[, taxon_sibcs := gsub("VERMELHO-AMARELO-Ortox", "VERMELHO-AMARELO Orto", taxon_sibcs)]
# Replace "intermediário&#10;para" with "intermediário para"
taxon_data[, taxon_sibcs := gsub("intermediário&#10;para", "intermediário para", taxon_sibcs)]
# Remove the following strings if they appear:
# (> 10mS/cm em superfície)
# "(sem definição de subgrupo)"
# (algo intermediária para Plintossolo?)
# ? NITOSSOLO BRUNO DISTRÓFICO HÚMICO ?
# ? ARGISSOLO VERMELHO Alumínico típico ?
# (rúbrico)?
# ? léptico?
# ?plíntico?
# (húmico? distrófico)
# ?)
#  A&#10;moderado
taxon_data[, taxon_sibcs := gsub("\\(> 10mS/cm em superfície\\)", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\(sem definição de subgrupo\\)", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\(algo intermediária para Plintossolo\\?\\)", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\? NITOSSOLO BRUNO DISTRÓFICO HÚMICO \\?", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\? ARGISSOLO VERMELHO Alumínico típico \\?", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\(rúbrico\\)?", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\? léptico\\?", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\?plíntico\\?", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\(húmico? distrófico\\)", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("\\?", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub(" A&#10;moderado", "", taxon_sibcs)]
# Remove "Vertic Argiustoll"
taxon_data[, taxon_sibcs := gsub("Vertic Argiustoll", "", taxon_sibcs)]
# Remove "Arenic Histic"
taxon_data[, taxon_sibcs := gsub("Arenic Histic", "", taxon_sibcs)]
# Remove "HUMIC GLEI"
taxon_data[, taxon_sibcs := gsub("HUMIC GLEI", "", taxon_sibcs)]
# Remove entire string if contains "Tropud"
taxon_data[, taxon_sibcs := gsub("Tropud.*", "", taxon_sibcs)]
# Remove entire string if contains "Typic"
taxon_data[, taxon_sibcs := gsub("Typic.*", "", taxon_sibcs)]
# Remove entire string if contains "Tropaq"
taxon_data[, taxon_sibcs := gsub("Tropaq.*", "", taxon_sibcs)]
# Remove entire string if contains "orthox"
taxon_data[, taxon_sibcs := gsub("orthox.*", "", taxon_sibcs)]
# Remove entire string if contains "Quartzipsamment"
taxon_data[, taxon_sibcs := gsub("Quartzipsamment.*", "", taxon_sibcs)]
# Remove entire string if contains "Oxic"
taxon_data[, taxon_sibcs := gsub("Oxic.*", "", taxon_sibcs)]
# Remove "(proposta de inclusão subgrupo)"
taxon_data[, taxon_sibcs := gsub("\\(proposta de inclusão subgrupo\\)", "", taxon_sibcs)]
# Remove "(Regosol Eutrófico com Fragispan)"
taxon_data[, taxon_sibcs := gsub("\\(Regosol Eutrófico com Fragispan\\)", "", taxon_sibcs)]
# Remove "(Unidade Semi-árida)"
taxon_data[, taxon_sibcs := gsub("\\(Unidade Semi-árida\\)", "", taxon_sibcs)]
# Remove "variação BÚZIO"
taxon_data[, taxon_sibcs := gsub("variação BÚZIO", "", taxon_sibcs)]
# Remove "(com B" at the end of the string
taxon_data[, taxon_sibcs := gsub("\\(com B$", "", taxon_sibcs)]
# Remove "(com" at the end of the string
taxon_data[, taxon_sibcs := gsub("\\(com$", "", taxon_sibcs)]
# Remove "com" at the end of the string
taxon_data[, taxon_sibcs := gsub("com$", "", taxon_sibcs)]
# Remove "com B" at the end of the string
taxon_data[, taxon_sibcs := gsub("com B$", "", taxon_sibcs)]
# Remove "*" at any position
taxon_data[, taxon_sibcs := gsub("\\*", "", taxon_sibcs)]
# Remove period and comma at the end of the string
taxon_data[, taxon_sibcs := gsub("\\.$", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub(",$", "", taxon_sibcs)]
# Replace multiple spaces with a single space
taxon_data[, taxon_sibcs := gsub("\\s+", " ", taxon_sibcs)]
# Remove leading and trailing spaces
taxon_data[, taxon_sibcs := trimws(taxon_sibcs)]
# Remove period at any position
taxon_data[, taxon_sibcs := gsub("\\.", "", taxon_sibcs)]
# Remove comma at the end of the string
taxon_data[, taxon_sibcs := gsub(",$", "", taxon_sibcs)]
# Remove "- " at the beginning of the string
taxon_data[, taxon_sibcs := gsub("^- ", "", taxon_sibcs)]
# Remove "A moderado"
taxon_data[, taxon_sibcs := gsub("A moderado", "", taxon_sibcs)]
# Remove "-" at the end of the string
taxon_data[, taxon_sibcs := gsub("-$", "", taxon_sibcs)]
# Remove " - Estação"
taxon_data[, taxon_sibcs := gsub(" - Estação", "", taxon_sibcs)]
# Remove "com B" at the end of the string
taxon_data[, taxon_sibcs := gsub("com B$", "", taxon_sibcs)]
# Remove parentheses " )" at the end of the string
taxon_data[, taxon_sibcs := gsub(" \\)$", "", taxon_sibcs)]
# Remove leading and trailing spaces
taxon_data[, taxon_sibcs := trimws(taxon_sibcs)]
# Replace "argila de atividade baixa" with "Tb"
taxon_data[, taxon_sibcs := gsub("argila de atividade baixa", "Tb", taxon_sibcs)]
# Replace "argila de atividade baica" with "Tb"
taxon_data[, taxon_sibcs := gsub("argila de atividade baica", "Tb", taxon_sibcs)]
# Replace "argila de atividade alta" with "Ta"
taxon_data[, taxon_sibcs := gsub("argila de atividade alta", "Ta", taxon_sibcs)]
# Replace "argila atividade alta" with "Ta"
taxon_data[, taxon_sibcs := gsub("argila atividade alta", "Ta", taxon_sibcs)]

# Check the unique values of taxon_sibcs
taxon_data[, sample(unique(taxon_sibcs))]

# Create unique id for each event
taxon_data[, id := paste0(dataset_id, "-", evento_id_febr)]
taxon_data[dataset_id == "ctb0768", id := gsub("-perfil", "", id)]

# Update soildata with taxon_data
soildata <- soildata[taxon_data, taxon_sibcs := i.taxon_sibcs, on = "id"]
# Replace "LOW" with NA
soildata[taxon_sibcs == "LOW", taxon_sibcs := NA_character_]
# Replace "Hapl" with NA
soildata[taxon_sibcs == "Hapl", taxon_sibcs := NA_character_]
# Replace common abbreviations in taxon_sibcs
# Print strings with max length smaller than 5 characters
soildata[nchar(taxon_sibcs) < 5 & taxon_sibcs != "", taxon_sibcs]
# RQo -> Neossolo Quartzarênico
soildata[, taxon_sibcs := gsub("RQo", "Neossolo Quartzarênico", taxon_sibcs,
  ignore.case = TRUE
)]
# LVa -> Latossolo Vermelho Amarelo
soildata[, taxon_sibcs := gsub("LVa ", "Latossolo Vermelho Amarelo", taxon_sibcs,
  ignore.case = TRUE
)]
# Pva -> Podzólico Vermelho Amarelo
soildata[, taxon_sibcs := gsub("Pva ", "Podzólico Vermelho Amarelo", taxon_sibcs,
  ignore.case = TRUE
)]
# PVe -> Podzólico Vermelho Escuro
soildata[, taxon_sibcs := gsub("PVe ", "Podzólico Vermelho Escuro", taxon_sibcs,
  ignore.case = TRUE
)]
# LBRa -> Latossolo Bruno
soildata[, taxon_sibcs := gsub("LBRa ", "Latossolo Bruno", taxon_sibcs,
  ignore.case = TRUE
)]
# LRd -> Latossolo Roxo distrófico
soildata[, taxon_sibcs := gsub("LRd ", "Latossolo Roxo distrófico", taxon_sibcs,
  ignore.case = TRUE
)]
# LRa -> Latossolo Roxo álico
soildata[, taxon_sibcs := gsub("LRa ", "Latossolo Roxo álico", taxon_sibcs,
  ignore.case = TRUE
)]
# TBRa -> Terra Bruna álica
soildata[, taxon_sibcs := gsub("TBRa ", "Terra Bruna álica", taxon_sibcs,
  ignore.case = TRUE
)]
# TRe -> Terra Roxa estruturada
soildata[, taxon_sibcs := gsub("TRe ", "Terra Roxa estruturada", taxon_sibcs,
  ignore.case = TRUE
)]

# Check if the taxon_sibcs was updated
# How does it compare to the previous missing_taxon?
print(missing_taxon)
soildata[dataset_id %in% missing_taxon$dataset_id & taxon_sibcs == "" | is.na(taxon_sibcs),
  .N,
  by = dataset_id
][order(-N)]

# tmp: For Wenceslau Geraldes Teixeira (Embrapa Solos)
if (FALSE) {
  # Remove all records for well drained soils based on soil classification
  # Drop soildata records with "VERMELHO" in taxon_sibcs
  drop <- paste0("VERMELHO|VERMELHA|VEMELHO|vermelha")
  wenceslau <- soildata[!grepl(drop, taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "Amarelo" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("AMARELO|AMARELA", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "CRÔMICO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("CRÔMICO|CROMICO|CROMADO", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "BRUNO-ACINZENTADO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("BRUNO-ACINZENTADO|Brunos", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "EBÂNICO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("EBÂNICO|EBANICO", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "LATOSSOLO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("LATOSSOLO|LATOSOL|latoss|Oxic", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "Nitossolo" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("NITOSSOLO|NITOSOL|Terra roxa", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "Terra Bruna" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("TERRA BRUNA|BRUNOS", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "ORGÂNICO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("ORGÂNICO|ORGANOSSOLO", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "LITÓLICO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("LITÓLICO|LITOLICO|LITOSOL", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "REGOLITICO" in taxon_sibcs
  drop <- paste0("REGOLÍTICO|REGOLITICO|RIGOLÍTICO")
  wenceslau <- wenceslau[!grepl(drop, taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "QUARTZARÊNICO" in taxon_sibcs
  drop <- paste0("QUARTZARÊNICO|QUARTZARENICO|QUARTZOSA|QUARTZARÊNICÓ|QUARTSOZA")
  wenceslau <- wenceslau[!grepl(drop, taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "CHERNOSSOLO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("CHERNOSSOLO", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "CAMBISSOLO" in taxon_sibcs
  wenceslau <- wenceslau[!grepl("CAMBISSOLO|CAMBISOL", taxon_sibcs, ignore.case = TRUE)]
  # Drop soildata records with "PÉTRICO" in taxon_sibcs
  drop <- paste0("PÉTRICO|PETRICO|Laterítico|Concrecionário")
  wenceslau <- wenceslau[!grepl(drop, taxon_sibcs, ignore.case = TRUE)]
  wenceslau[, sample(unique(taxon_sibcs))]
  # write to disk
  data.table::fwrite(wenceslau, "data/15_soildata_wenceslau.txt", sep = "\t")
}

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255
data.table::fwrite(soildata, "data/15_soildata.txt", sep = "\t")

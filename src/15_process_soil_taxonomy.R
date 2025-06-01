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
# Keep only the strings before the string " / "
taxon_data[, taxon_sibcs := gsub(" / .*", "", taxon_sibcs)]
# Keep only the strings before the word "textura"
taxon_data[, taxon_sibcs := gsub("textura", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("Textura", "", taxon_sibcs)]
taxon_data[, taxon_sibcs := gsub("texutra", "", taxon_sibcs)]
# Keep only the strings before the word "Substrato"
taxon_data[, taxon_sibcs := gsub("Substrato.*", "", taxon_sibcs)]
# Keep only the strings before the word "argilosa"
taxon_data[, taxon_sibcs := gsub("argilosa.*", "", taxon_sibcs)]
# Keep only the strings before the word "arenosa"
taxon_data[, taxon_sibcs := gsub("arenosa.*", "", taxon_sibcs)]
# Keep only the strings before the word "muito"
taxon_data[, taxon_sibcs := gsub("muito.*", "", taxon_sibcs)]
# Keep only the strings before the word "média"
taxon_data[, taxon_sibcs := gsub("média.*", "", taxon_sibcs)]
# Keep only the strings before the word "media"
taxon_data[, taxon_sibcs := gsub("media.*", "", taxon_sibcs)]
# Keep only the strings before the word "mádia/"
taxon_data[, taxon_sibcs := gsub("mádia/.*", "", taxon_sibcs)]
# Keep only the strings before the word "siltosa"
taxon_data[, taxon_sibcs := gsub("siltosa.*", "", taxon_sibcs)]
# Keep only the strings before the word "cascalhento"
taxon_data[, taxon_sibcs := gsub("cascalhento.*", "", taxon_sibcs)]
# Keep only the strings before the word "hístico"
taxon_data[, taxon_sibcs := gsub("hístico.*", "", taxon_sibcs)]
# Keep only the strings before the word "fase"
taxon_data[, taxon_sibcs := gsub("fase.*", "", taxon_sibcs)]
# Keep only the strings before the word "unidade"
taxon_data[, taxon_sibcs := gsub("unidade.*", "", taxon_sibcs)]
# Keep only the strings before the word "floresta"
taxon_data[, taxon_sibcs := gsub("floresta.*", "", taxon_sibcs)]
# Keep only the strings before the word "orgânica"
taxon_data[, taxon_sibcs := gsub("orgânica.*", "", taxon_sibcs)]
# Keep only the strings before the word "campo"
taxon_data[, taxon_sibcs := gsub("campo.*", "", taxon_sibcs)]
# Keep only the strings before the word "(?)"
taxon_data[, taxon_sibcs := gsub("\\(\\?\\)", "", taxon_sibcs)]
# Keep only the strings before the symbol "?"
taxon_data[, taxon_sibcs := gsub("\\?", "", taxon_sibcs)]
# Remove "&#10;"
taxon_data[, taxon_sibcs := gsub("&#10;", "", taxon_sibcs)]
# argila de atividade baixa -> Tb
taxon_data[, taxon_sibcs := gsub("argila de atividade baixa", "Tb", taxon_sibcs)]
# argila de atividade baica -> Tb
taxon_data[, taxon_sibcs := gsub("argila de atividade baica", "Tb", taxon_sibcs)]
# argila de atividade alta -> Ta
taxon_data[, taxon_sibcs := gsub("argila de atividade alta", "Ta", taxon_sibcs)]
# argila atividade alta -> Ta
taxon_data[, taxon_sibcs := gsub("argila atividade alta", "Ta", taxon_sibcs)]
# Remove extra spaces
taxon_data[, taxon_sibcs := gsub("\\s+", " ", taxon_sibcs)]
# Drop period at the end of the string
taxon_data[, taxon_sibcs := gsub("\\.$", "", taxon_sibcs)]
# Drop commas
taxon_data[, taxon_sibcs := gsub(",", "", taxon_sibcs)]
# Remove "(sem definição de subgrupo)"
taxon_data[, taxon_sibcs := gsub("\\(sem definição de subgrupo\\)", "", taxon_sibcs)]
# Remove "(proposta de inclusão subgrupo)"
taxon_data[, taxon_sibcs := gsub("\\(proposta de inclusão subgrupo\\)", "", taxon_sibcs)]
# Remove "aterro com calhaus"
taxon_data[, taxon_sibcs := gsub("aterro com calhaus", "", taxon_sibcs)]
# trim
taxon_data[, taxon_sibcs := trimws(taxon_sibcs)]

# Check the unique values of taxon_sibcs
taxon_data[, sample(unique(taxon_sibcs))]

# Create unique id for each event
taxon_data[, id := paste0(dataset_id, "-", evento_id_febr)]
taxon_data[dataset_id == "ctb0768", id := gsub("-perfil", "", id)]

# Update soildata with taxon_data
soildata <- soildata[taxon_data, taxon_sibcs := i.taxon_sibcs, on = "id"]

# Clean data
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
soildata[!(dataset_id %in% no_taxon) & taxon_sibcs == "", .N, by = dataset_id][order(-N)]

# tmp: For Wenceslau
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
# Layers: 57079
# Events: 16824
# Georeferenced events: 14334
data.table::fwrite(soildata, "data/15_soildata.txt", sep = "\t")

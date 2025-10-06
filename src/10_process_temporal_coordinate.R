# title: SoilData Integration
# subtitle: Process time coordinate
# author: Alessandro Samuel-Rosa
# date: 2025
# licence: MIT
# summary: This script processes the temporal coordinate (sampling year) of the Brazilian Soil 
#          Dataset. It starts by extracting the year from the full date. Missing sampling years are 
#          then recovered using data from a collaborative spreadsheet. For the remaining missing 
#          years, an estimated sampling year is attributed based on information about the source 
#          soil survey project. A new variable is created to indicate the source of the sampling 
#          year (original or estimated). Finally, the temporal distribution of the samples is 
#          plotted, and the processed dataset is saved to a file.
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("dataverse")) {
  install.packages("dataverse")
  library(dataverse)
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
nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")]))
# 14043 events
nrow(br_soil2023)
# 50470 layers

# Process time coordinate (sampling year)
br_soil2023[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
br_soil2023[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Clean odd sampling date
br_soil2023[data_coleta_ano < 1950, data_coleta_ano := NA_integer_]

# Temporal distribution of samples with known sampling date
nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")]))
# 14043 events
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 4848 without sampling date
br_soil2023[, na_year := FALSE]
br_soil2023[is.na(data_coleta_ano), na_year := TRUE]
missing_time <- is.na(br_soil2023[["data_coleta_ano"]])
# Plot histogram
file_path <- "res/fig/101_temporal_distribution_before_rescue.png"
png(file_path, width = 8, height = 5, units = "in", res = 300)
hist(br_soil2023[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)), 
  main = "Temporal distribution of samples with known sampling date\nbefore data rescue",
  xlab = "Year"
)
rug(br_soil2023[["data_coleta_ano"]])
dev.off()

# The following code is commented out because it is not necessary to write the table to disk with
# events missing date. The data is already available in the Google Sheets spreadsheet.
# # Write table to disk with events missing date
# # Only the surface layer (profund_sup == 0) of each event is exported.
# # The field dataset_id is reset as a URL to facilitate access to the respective webpage on FEBR.
# # The recovery of the sampling date will be done collectively by our team of data curators using a
# # Google Sheets spreadsheet to register the data.
# no_time_coord <- br_soil2023[
#   is.na(data_coleta_ano) & profund_sup == 0,
#   c(
#     "dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id",
#     "data_coleta_dia", "data_coleta_mes", "data_coleta_ano"
#   )
# ]
# no_time_coord[, dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")]
# data.table::fwrite(no_time_coord, "data/no-time-coord.csv", sep = "\t", dec = ",")

# Read Google Sheets spreadsheet containing the recovered sampling dates
# It is not necessary to set the table because the spreadsheet contains only one.
key <- "1UbuI_oMzFmclztmhZQYsuU0mn_Lx3NhSeBoFw0m4lv0"
file <- paste0("http://docs.google.com/spreadsheets/d/", key, "/pub?output=csv")
recovered_time <- data.table::fread(file, header = TRUE, na.strings = c("-", ""), sep = ",")
recovered_time[, data_coleta_ano := as.integer(data_coleta_ano)]
print(recovered_time)

# Check the range of values
# Any error present in the downloaded data is corrected in the Google Sheets spreadsheet
range(recovered_time[["data_coleta_ano"]], na.rm = TRUE)
# 1957 2007

# Fill up the original table using the data recovered by our team of data curators
recovered_time[, dados_id := gsub("https://www.pedometria.org/febr/", "", dados_id)]
recovered_time[, dados_id := gsub("/", "", dados_id)]
recovered_time[, id := paste0(dados_id, "-", evento_id_febr)]
br_soil2023[, id := paste0(dataset_id, "-", observacao_id)]
idx_recovered <- match(br_soil2023[missing_time, id], recovered_time[["id"]])
br_soil2023[missing_time, data_coleta_ano := recovered_time[idx_recovered, data_coleta_ano]]

# Temporal distribution of samples with known sampling date after data rescue
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 3397 events remain without a known sampling date
br_soil2023[, na_year := FALSE]
br_soil2023[is.na(data_coleta_ano), na_year := TRUE]
missing_time <- is.na(br_soil2023[["data_coleta_ano"]])
# Plot histogram
file_path <- "res/fig/102_temporal_distribution_after_rescue.png"
png(file_path, width = 8, height = 5, units = "in", res = 300)
hist(br_soil2023[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)), 
  main = "Temporal distribution of samples with known sampling date\nafter data rescue",
  xlab = "Year"
)
rug(br_soil2023[["data_coleta_ano"]])
dev.off()
br_soil2023[, na_year := NULL]

# Attribute the most likely (estimate) temporal coordinate #########################################
# Create a second column "data_coleta_ano_fonte". If we the sampling date is being estimated
# (target_year), register data_coleta_ano_fonte = "estimativa". If the year is from the original
# data, it will be "original".
br_soil2023[, data_coleta_ano_fonte := NA_character_]

# Inventário das terras em microbacias hidrográficas, Santa Catarina
# These are various datasets from the same project.
target_year <- 1995
# Set the sampling year = target_year and data_coleta_ano_fonte = "estimativa"
br_soil2023[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    is.na(data_coleta_ano),
  `:=`(data_coleta_ano = target_year, data_coleta_ano_fonte = "estimativa")
]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 3323 event remaining without year

# LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP
target_year <- 1995
# Set sampling year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[
  dataset_id == "ctb0815" & is.na(data_coleta_ano),
  `:=`(data_coleta_ano = target_year, data_coleta_ano_fonte = "estimativa")
]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0815" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 3240 events remain without sampling year

# Define an arbitrarily low year below the actual minimum
# Use this year as the value for events with NAs (DELETE LATER ON -- THIS IS FOR VISUALIZATION ONLY)
# This allows these data to be shown in the histogram in a separate column from the other data.
year_min <- min(br_soil2023[, data_coleta_ano], na.rm = TRUE)
year_min <- (floor(year_min / 10) * 10) - 2
print(year_min)
# 1948

# RADAMBRASIL: set sampling year to year_min
# For datasets from the RADAMBRASIL project, the sampling year is set to `year_min` because all 
# sampling occurred before 1985, which is the earliest year modeled by the MapBiomas Soil project. 
# Although the Brazilian Soil Dataset is not directly defined by the MapBiomas Soil project, this
# adjustment is necessary due to dependencies in data processing. A more accurate sampling date
# will be determined or estimated in the future.
idx <- br_soil2023[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  id
]
# Set sampling_year to year_min and data_coleta_ano_fonte to "estimativa"
br_soil2023[id %in% idx, data_coleta_ano := year_min]
br_soil2023[id %in% idx, data_coleta_ano_fonte := "estimativa"]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1819 events remain without sampling date

# How many events:
# 1) have spatial coordinates (coord_x and coord_y) and
# 2) but do not have a sampling date (data_coleta_ano)?
nrow(unique(br_soil2023[
  is.na(data_coleta_ano) & !is.na(coord_x) & !is.na(coord_y),
  c("dataset_id", "observacao_id")
]))
# 665 events

# Set the sampling year to 1999 for the following datasets:
# (we checked the source document and found that the sampling year is about 1999)
# ctb0801
target_year <- 1999
# Set sampling year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0801" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0801" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1807 events

# Set the sampling year to 1998 for the following datasets:
# (we checked the source document and found that the sampling year is about 1998)
# ctb0807
target_year <- 1998
# Set sampling year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0807" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0807" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1806 events

# Set the sampling year to 1994 for the following datasets:
# (we checked the source document and found that the sampling year is about 1994)
# ctb0779
target_year <- 1994
# Set sampling year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0779" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0779" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1789 events

# Set the sampling year to 1991 for the following datasets:
# (we checked the source document and found that the sampling year is about 1991)
# ctb0802
target_year <- 1991
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0802" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0802" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1776 events

# Set the sampling year to 1989 for the following datasets:
# (we checked the source document and found that the sampling year is about 1989)
# ctb0604
target_year <- 1989
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0604" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0604" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1753 events

# Set the sampling year to 1983 for the following datasets:
# (we checked the source document and found that the sampling year is about 1983)
# ctb0658
target_year <- 1983
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0658" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0658" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1742 events

# Set the sampling year to 1981 for the following datasets:
# (we checked the source document and found that the sampling year is about 1981)
# ctb0655
target_year <- 1981
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0655" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0655" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1732 events

# Set the sampling year to 1980 for the following datasets:
# (we checked the source document and found that the sampling year is about 1980)
# ctb0810, ctb0814
target_year <- 1980
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0810", "ctb0814") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0810", "ctb0814") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1617 events

# Set the sampling year to 1978 for the following datasets:
# (we checked the source document and found that the sampling year is about 1978)
# ctb0776, ctb0819
target_year <- 1978
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0776", "ctb0819") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0776", "ctb0819") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1552 events

# Set the sampling year to 1977 for the following datasets:
# (we checked the source document and found that the sampling year is about 1977)
# ctb0660, ctb0788
target_year <- 1977
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0660", "ctb0788") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0660", "ctb0788") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1488 events

# Set the sampling year to 1976 for the following datasets:
# (we checked the source document and found that the sampling year is about 1976)
# ctb0648, ctb0785
target_year <- 1976
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0648", "ctb0785") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0648", "ctb0785") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1387 events remain without sampling date

# Set the sampling year to 1974 for the following datasets:
# (we checked the source document and found that the sampling year is about 1974)
# ctb0789, ctb0818
target_year <- 1974
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0789", "ctb0818") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0789", "ctb0818") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1331 events remain without sampling date

# Set the sampling year to 1971 for the following datasets:
# (we checked the source document and found that the sampling year is about 1971)
# ctb0783, ctb0827
target_year <- 1971
# Set sampling year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0783", "ctb0827") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0783", "ctb0827") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1198 events remain without sampling date

# Set the sampling year to 1970 for the following datasets:
# (we checked the source document and found that the sampling year is about 1970)
# ctb0797
target_year <- 1970
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0797" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0797" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1141 events remain without sampling date

# Set the sampling year to 1969 for the following datasets:
# (we checked the source document and found that the sampling year is about 1969)
# ctb0798
target_year <- 1969
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0798" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0798" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1130 events remain without sampling date

# Set the sampling year to 1967 for the following datasets:
# (we checked the source document and found that the sampling year is about 1967)
# ctb0693, ctb0804
target_year <- 1967
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0693", "ctb0804") & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0693", "ctb0804") & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1106 events remain without sampling date

# Set the sampling year to 1959 for the following datasets
# (we checked the source document and found that the sampling year is about 1959)
# ctb0787
target_year <- 1959
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0787" & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0787" & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1020 events remain without sampling date

# Set sampling year to year_min for the following datasets:
# (we checked the source document and found that the sampling year is < 1985; similar to 
# RADAMBRASIL)
# ctb0023, ctb0028, ctb0603, ctb0608, ctb0635, ctb0666, ctb0682, ctb0829, ctb0702
target_year <- year_min
ctb <- c(
  "ctb0023", "ctb0028", "ctb0603", "ctb0608", "ctb0635", "ctb0666", "ctb0682", "ctb0829",
  "ctb0702"
)
# Set sampling_year to target_year and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% ctb & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = target_year,
  data_coleta_ano_fonte = "estimativa"
)]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% ctb & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 848 events remain without sampling date

# Use the average sampling date of the source soil survey for the following datasets:
# ctb0030, ctb0032, ctb0570, ctb0572, ctb0574, ctb0617, ctb0631, ctb0639, ctb0642, ctb0645, ctb0656
# ctb0657, ctb0663, ctb0667, ctb0668, ctb0672, ctb0673, ctb0674, ctb0675, ctb0677, ctb0679, ctb0684
# ctb0686, ctb0691, ctb0694, ctb0700, ctb0750, ctb0774, ctb0775, ctb0777, ctb0781, ctb0795, ctb0808
# ctb0809, ctb0811, ctb0820, ctb0821, ctb0822, ctb0826, ctb0831, ctb0832
ctb <- c(
  "ctb0030", "ctb0032", "ctb0570", "ctb0572", "ctb0574", "ctb0617", "ctb0631", "ctb0639",
  "ctb0642", "ctb0645", "ctb0656", "ctb0657", "ctb0663", "ctb0667", "ctb0668", "ctb0672",
  "ctb0673", "ctb0674", "ctb0675", "ctb0677", "ctb0679", "ctb0684", "ctb0686", "ctb0691",
  "ctb0694", "ctb0700", "ctb0750", "ctb0774", "ctb0775", "ctb0777", "ctb0781", "ctb0795",
  "ctb0808", "ctb0809", "ctb0811", "ctb0820", "ctb0821", "ctb0822", "ctb0826", "ctb0831",
  "ctb0832"
)
average_year <- br_soil2023[dataset_id %in% ctb,
  .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
  by = dataset_id
]
idx_averaged <- match(
  br_soil2023[is.na(data_coleta_ano) & dataset_id %in% ctb, dataset_id],
  average_year[, dataset_id]
)
# Set sampling_year to the average sampling year and data_coleta_ano_fonte to "estimativa"
br_soil2023[
  is.na(data_coleta_ano) & dataset_id %in% ctb,
  `:=`(
    data_coleta_ano = average_year[idx_averaged, data_coleta_ano],
    data_coleta_ano_fonte = "estimativa"
  )
]
# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  is.na(data_coleta_ano) & dataset_id %in% ctb & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 66 events remain without sampling date

# ctb0009
# events missing the sampling date are from ctb0003, thus they are already included in the dataset
ctb <- "ctb0009"
br_soil2023 <- br_soil2023[!(dataset_id %in% ctb & is.na(data_coleta_ano)), ]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 34 events remain without sampling date, all from ctb0029

# ctb0029
# events from municipio_id %in% c("Santa Maria", "Itaara"), amostra_tipo == "COMPOSTA",
# amostra_quanti == 3, and data_ano == 2009 are from ctb0003, thus they are already included in
# the dataset and can be removed
ctb <- "ctb0029"
br_soil2023 <- br_soil2023[!(
  dataset_id == ctb & municipio_id %in% c("Santa Maria", "Itaara") &
    amostra_tipo == "COMPOSTA" & amostra_quanti == 3 & data_coleta_ano == 2009
), ]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 34 events remain without sampling date, all from ctb0029
# Soil samples were taken around 2009, but the exact date is not known.
# ctb0029: Set sampling year to 2009 and data_coleta_ano_fonte to "estimativa"
br_soil2023[dataset_id == ctb & is.na(data_coleta_ano), `:=`(
  data_coleta_ano = 2009,
  data_coleta_ano_fonte = "estimativa"
)]

# Set data_coleta_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == ctb & !is.na(data_coleta_ano) & is.na(data_coleta_ano_fonte),
  data_coleta_ano_fonte := "original"
]

# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 0 events remain without sampling date

# Check how many events have spatial coordinates (coord_x and coord_y) and
nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")])) # 13973 events
nrow(br_soil2023)
# 50400 layers

# Temporal distribution of samples with known sampling date after data rescue and estimation
missing_time <- is.na(br_soil2023[["data_coleta_ano"]])
# Plot histogram
file_path <- "res/fig/103_temporal_distribution_after_estimation.png"
png(file_path, width = 8, height = 5, units = "in", res = 300)
x <- br_soil2023[, data_coleta_ano[1], by = c("dataset_id", "observacao_id")][, V1]
hist(x,
  xlab = "Year", main = "Temporal distribution of events after data rescue and estimation",
  sub = paste0("n = ", sum(!missing_time))
)
rug(x)
dev.off()
rm(missing_time, file_path, x)

# Remove year_min from the dataset, updating data_coleta_ano_fonte to NA_character_
br_soil2023[data_coleta_ano == year_min, data_coleta_ano_fonte := NA_character_]
br_soil2023[data_coleta_ano == year_min, data_coleta_ano := NA_integer_]

# Check for consisteny of data_coleta_ano and data_coleta_ano_fonte
# Should be an empty table
nrow(br_soil2023[
  is.na(data_coleta_ano) & !is.na(data_coleta_ano_fonte),
  .(dataset_id, observacao_id, data_coleta_ano, data_coleta_ano_fonte)
]) == 0
# TRUE

# Write data to disk ###############################################################################
summary_soildata(br_soil2023)
# Layers: 50400
# Events: 13973
# Georeferenced events: 10942
# Datasets: 235
data.table::fwrite(br_soil2023, "data/10_soildata.txt", sep = "\t")

# title: SoilData Integration
# subtitle: Process time coordinate
# author: Alessandro Samuel-Rosa
# date: 2025
# licence: MIT
# description: This script processes the temporal coordinate (sampling year) of
# the Brazilian Soil Dataset. Processing always starts from v2023 and will
# continue to do so until all datasets originally included in v2023 have
# completed individual processing in the SoilData-ctb repository. The script
# extracts the year from the full date, recovers missing sampling years using a
# collaborative spreadsheet, and attributes estimated years for remaining
# missing values based on source soil survey information. It also creates a
# variable indicating the source of the sampling year (original or estimated),
# plots the temporal distribution of samples, and saves the processed dataset.
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
source("src/SDi2025/00_helper_functions.R")

# Read the Brazilian Soil Dataset v2023 (ALWAYS START FROM v2023) ##############
# Check if file "data/00_brazilian_soil_dataset_2023.txt" exists. If not, read
# the Brazilian Soil Dataset v2023 from the SoilData repository using the
# 'dataverse' package. Next, write the downloaded file to
# 'data/00_brazilian_soil_dataset_2023.txt'. The dataset is available at
# https://doi.org/10.60502/SoilData/TUI25K. If the file already exists, read it
# using the 'data.table' package.
file_path <- "res/tab/brazilian-soil-dataset-2023.txt"
if (!file.exists(file_path)) {
  br_soil2023 <- dataverse::get_dataframe_by_name(
    filename = "brazilian-soil-dataset-2023.txt",
    server = "https://repositorio.soildata.mapbiomas.org/dataverse/soildata",
    dataset = "10.60502/SoilData/TUI25K",
    .f = data.table::fread
  )
  data.table::fwrite(br_soil2023, file_path, dec = ".", sep = ";")
} else {
  br_soil2023 <- data.table::fread(file_path, dec = ".", sep = ";")
}
str(br_soil2023)

# Process time coordinate (sampling year)
br_soil2023[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
br_soil2023[, data_ano := as.integer(format(observacao_data, "%Y"))]
# Check data
summary_soildata(br_soil2023)
# Layers: 50470
# Events: 14043
# Georeference: 11012 (yes) / 3031 (no)
# Date: 9223 (yes) / 4820 (no)
# Datasets: 235

# Found wrong date in dataset_id = ctb0032, observacao_id = RO1725
# Instead of 1939, it should be 1997 -- This has already been corrected in the
# source spreadsheet.
br_soil2023[
  dataset_id == "ctb0032" & observacao_id == "RO1725",
  data_ano := ifelse(data_ano == 1939, 1997, data_ano)
]

# If necessary, clean any odd sampling date
target_year <- 1950
n_below_target <- sum(br_soil2023[["data_ano"]] < target_year, na.rm = TRUE)
if (n_below_target > 0) {
  warning(
    n_below_target, " sampling year(s) are < ", target_year, ". Setting them to NA."
  )
  br_soil2023[data_ano < target_year, data_ano := NA_integer_]
} else {
  cat("All sampling years are >= ", target_year, ". No changes made.\n", sep = "")
}

# Some events have both known and unknown sampling dates for their layers. The
# reason for this are errors in the source data (SISB), specifically, data from
# different events having the same identifier. Issues occurs in:
# ctb0683: one event
# ctb0759: nine events
# ctb0760: 13 events
# ctb0766: one event
# ctb0771: one event
# ctb0809: one event
# ctb0832 one event.
br_soil2023[, 
  has_date := any(!is.na(data_ano)), by = .(dataset_id, observacao_id)]
br_soil2023[, no_date := any(is.na(data_ano)), by = .(dataset_id, observacao_id)]
print(br_soil2023[has_date == TRUE & no_date == TRUE,
  by = .(dataset_id, observacao_id), .N
])
#     dataset_id observacao_id     N
#         <char>        <char> <int>
#  1:    ctb0683             5    16
#  2:    ctb0759            11    12
#  3:    ctb0759            14    10
#  4:    ctb0759            15    21
#  5:    ctb0759            19    14
#  6:    ctb0759            28    14
#  7:    ctb0759            50    30
#  8:    ctb0759            53    21
#  9:    ctb0759             7    10
# 10:    ctb0759             8    12
# 11:    ctb0760             1    12
# 12:    ctb0760            10    14
# 13:    ctb0760            11    16
# 14:    ctb0760             2    14
# 15:    ctb0760             3     8
# 16:    ctb0760             4    10
# 17:    ctb0760             5    12
# 18:    ctb0760             6    12
# 19:    ctb0760       7-EXTRA     4
# 20:    ctb0760             8     8
# 21:    ctb0760       8-EXTRA     6
# 22:    ctb0760             9    10
# 23:    ctb0760       9-EXTRA     4
# 24:    ctb0766            66     4
# 25:    ctb0771            40    27
# 26:    ctb0809       Exame-8     8
# 27:    ctb0832      E-Rio-30     6
# For these events, keep only the layers with a known sampling date and drop the
# layers without a known sampling date. Corrections in the source data will be
# made in the future.
br_soil2023 <- br_soil2023[
  !(has_date == TRUE & no_date == TRUE & is.na(data_ano))
]
summary_soildata(br_soil2023)
# Layers: 50286 (we lost 184 layers)
# Events: 14043 (no event was lost)
# Georeference: 11012 (yes) / 3031 (no)
# Date: 9223 (yes) / 4820 (no)
# Datasets: 235

# FIGURE. Temporal distribution of samples with known sampling date
br_soil2023[, na_year := FALSE]
br_soil2023[is.na(data_ano), na_year := TRUE]
missing_time <- is.na(br_soil2023[["data_ano"]])
# Plot histogram
file_path <- fig_path("101_temporal_distribution_before_rescue.png")
png(file_path, width = 8, height = 5, units = "in", res = 300)
hist(br_soil2023[["data_ano"]], sub = paste0("n = ", sum(!missing_time)), 
  main = paste0("Temporal distribution of samples with known sampling date\n", "before data rescue"),
  xlab = "Year"
)
rug(br_soil2023[["data_ano"]])
dev.off()

# THE FOLLOWING BLOCK IS NOT NECESSARY
# It is not necessary to write the table to disk with events missing date. The
# data is already available in the Google Sheets spreadsheet.
# # Write table to disk with events missing date
# # Only the surface layer (profund_sup == 0) of each event is exported.
# # The field dataset_id is reset as a URL to facilitate access to the
# # respective webpage on FEBR.
# # The recovery of the sampling date will be done collectively by our team of
# # data curators using a Google Sheets spreadsheet to register the data.
# no_time_coord <- br_soil2023[
#   is.na(data_ano) & profund_sup == 0,
#   c(
#     "dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id",
#     "data_coleta_dia", "data_coleta_mes", "data_ano"
#   )
# ]
# no_time_coord[
#   ,
#   dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")
# ]
# data.table::fwrite(no_time_coord, "data/no-time-coord.csv", sep = "\t", dec = ",")

# Read Google Sheets spreadsheet containing the recovered sampling dates
# It is not necessary to set the table because there is only one.
key <- "1UbuI_oMzFmclztmhZQYsuU0mn_Lx3NhSeBoFw0m4lv0"
file <- paste0("http://docs.google.com/spreadsheets/d/", key, "/pub?output=csv")
recovered_time <-
  data.table::fread(file, header = TRUE, na.strings = c("-", ""), sep = ",")
recovered_time[, data_coleta_ano := as.integer(data_coleta_ano)]
print(recovered_time)

# Check the range of recovered values
# Any error present in the downloaded data is corrected in the Google Sheets
# spreadsheet
range(recovered_time[["data_coleta_ano"]], na.rm = TRUE)
# 1957 2007 This is ok!

# Fill up the original table using the data recovered by our team
recovered_time[
  ,
  dados_id := gsub("https://www.pedometria.org/febr/", "", dados_id)
]
recovered_time[, dados_id := gsub("/", "", dados_id)]
recovered_time[, id := paste0(dados_id, "-", evento_id_febr)]
br_soil2023[, id := paste0(dataset_id, "-", observacao_id)]
idx_recovered <- match(br_soil2023[missing_time, id], recovered_time[["id"]])
br_soil2023[
  missing_time,
  data_ano := recovered_time[idx_recovered, data_coleta_ano]
]
summary_soildata(br_soil2023)
# Layers: 50286
# Events: 14043
# Georeference: 11012 (yes) / 3031 (no)
# Date: 10653 (yes) / 3390 (no)
# Datasets: 235

# FIGURE. Temporal distribution of samples with known sampling date after data 
# rescue
br_soil2023[, na_year := FALSE]
br_soil2023[is.na(data_ano), na_year := TRUE]
missing_time <- is.na(br_soil2023[["data_ano"]])
# Plot histogram
file_path <- fig_path("102_temporal_distribution_after_rescue.png")
png(file_path, width = 8, height = 5, units = "in", res = 300)
hist(br_soil2023[["data_ano"]], sub = paste0("n = ", sum(!missing_time)), 
  main = paste0("Temporal distribution of samples with known sampling date\n", "after data rescue"),
  xlab = "Year"
)
rug(br_soil2023[["data_ano"]])
dev.off()
br_soil2023[, na_year := NULL]

# Attribute the most likely (estimate) temporal coordinate #####################
# Create a second column "data_ano_fonte". If we the sampling date is being
# estimated/guessed (target_year), register data_ano_fonte = "estimativa".
# If the sampling data comes from the original data set, set 
# data_ano_fonte = "original".
br_soil2023[, data_ano_fonte := NA_character_]

# Inventário das terras em microbacias hidrográficas, Santa Catarina
# These are various datasets from the same project. We can confortably attribute
# the same sampling year to all of them.
target_year <- 1995
# Set the sampling year = target_year and data_ano_fonte = "estimativa"
br_soil2023[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    is.na(data_ano),
  `:=`(data_ano = target_year, data_ano_fonte = "estimativa")
]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 10727 (yes) / 3316 (no)

# LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP
target_year <- 1995
# Set sampling year to target_year and data_ano_fonte to "estimativa"
br_soil2023[
  dataset_id == "ctb0815" & is.na(data_ano),
  `:=`(data_ano = target_year, data_ano_fonte = "estimativa")
]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0815" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 10810 (yes) / 3233 (no)

# THIS IS FOR VISUALIZATION PURPOSES ONLY -- DELETE LATER ON
# Define an arbitrarily low year below the actual minimum. Use this year as the
# value for events with NAs. This allows these data to be shown in the histogram
# in a separate column from the other data.
year_min <- min(br_soil2023[, data_ano], na.rm = TRUE)
year_min <- (floor(year_min / 10) * 10) - 2
print(year_min)
# 1948

# RADAMBRASIL: set sampling year to year_min
# For datasets from the RADAMBRASIL project, the sampling year is set to
# `year_min` because all sampling occurred before 1985, which is the earliest
# year modeled by the MapBiomas Soil project. Although the Brazilian Soil
# Dataset is not directly defined by the MapBiomas Soil project, this adjustment
# is necessary due to dependencies in data processing. A more accurate sampling
# date will be determined or estimated in the future.
idx <- br_soil2023[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_ano),
  id
]
# Set sampling_year to year_min and data_ano_fonte to "estimativa"
br_soil2023[id %in% idx, data_ano := year_min]
br_soil2023[id %in% idx, data_ano_fonte := "estimativa"]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12227 (yes) / 1816 (no)

# How many events:
# 1) have spatial coordinates (coord_x and coord_y) and
# 2) do not have a sampling date (data_ano)?
nrow(unique(br_soil2023[
  is.na(data_ano) & !is.na(coord_x) & !is.na(coord_y),
  c("dataset_id", "observacao_id")
]))
# 664 events

# Set the sampling year to 1999 for the following datasets:
# (we checked the source document and found that the sampling year is about
# 1999, so this is a reasonable estimate)
# ctb0801
target_year <- 1999
# Set sampling year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0801" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0801" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12239 (yes) / 1804 (no)

# Set the sampling year to 1998 for the following datasets:
# (we checked the source document and found that the sampling year is about
# 1998, so this is a reasonable estimate)
# ctb0807
target_year <- 1998
# Set sampling year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0807" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0807" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12240 (yes) / 1803 (no)

# Set the sampling year to 1994 for the following datasets:
# (we checked the source document and found that the sampling year is about
# 1994, so this is a reasonable estimate)
# ctb0779
target_year <- 1994
# Set sampling year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0779" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0779" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12257 (yes) / 1786 (no)

# Set the sampling year to 1991 for the following datasets:
# (we checked the source document and found that the sampling year is about
# 1991, so this is a reasonable estimate)
# ctb0802
target_year <- 1991
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0802" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0802" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12270 (yes) / 1773 (no)

# Set the sampling year to 1989 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1989, so this is a reasonable estimate)
# ctb0604
target_year <- 1989
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0604" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0604" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12293 (yes) / 1750 (no)

# Set the sampling year to 1983 for the following datasets:
# (we checked the source document and found that the sampling year is about
# 1983, so this is a reasonable estimate)
# ctb0658
target_year <- 1983
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0658" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0658" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12304 (yes) / 1739 (no)

# Set the sampling year to 1981 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1981, so this is a reasonable estimate)
# ctb0655
target_year <- 1981
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0655" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0655" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12314 (yes) / 1729 (no)

# Set the sampling year to 1980 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1980, so this is a reasonable estimate)
# ctb0810, ctb0814
target_year <- 1980
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0810", "ctb0814") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0810", "ctb0814") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12429 (yes) / 1614 (no)

# Set the sampling year to 1978 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1978, so this is a reasonable estimate)
# ctb0776, ctb0819
target_year <- 1978
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0776", "ctb0819") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0776", "ctb0819") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12494 (yes) / 1549 (no)

# Set the sampling year to 1977 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1977, so this is a reasonable estimate)
# ctb0660, ctb0788
target_year <- 1977
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0660", "ctb0788") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0660", "ctb0788") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12558 (yes) / 1485 (no)

# Set the sampling year to 1976 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1976, so this is a reasonable estimate)
# ctb0648, ctb0785
target_year <- 1976
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0648", "ctb0785") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0648", "ctb0785") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12659 (yes) / 1384 (no)

# Set the sampling year to 1974 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1974, so this is a reasonable estimate)
# ctb0789, ctb0818
target_year <- 1974
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0789", "ctb0818") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0789", "ctb0818") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12715 (yes) / 1328 (no)

# Set the sampling year to 1971 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1971, so this is a reasonable estimate)
# ctb0783, ctb0827
target_year <- 1971
# Set sampling year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0783", "ctb0827") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0783", "ctb0827") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12848 (yes) / 1195 (no)

# Set the sampling year to 1970 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1970, so this is a reasonable estimate)
# ctb0797
target_year <- 1970
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0797" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0797" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12905 (yes) / 1138 (no)

# Set the sampling year to 1969 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1969, so this is a reasonable estimate)
# ctb0798
target_year <- 1969
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0798" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0798" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12916 (yes) / 1127 (no)

# Set the sampling year to 1967 for the following datasets:
# (we checked the source document and found that the sampling year is about 
# 1967, so this is a reasonable estimate)
# ctb0693, ctb0804
target_year <- 1967
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% c("ctb0693", "ctb0804") & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% c("ctb0693", "ctb0804") & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 12940 (yes) / 1103 (no)

# Set the sampling year to 1959 for the following datasets
# (we checked the source document and found that the sampling year is about 
# 1959, so this is a reasonable estimate)
# ctb0787
target_year <- 1959
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == "ctb0787" & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == "ctb0787" & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
summary_date(br_soil2023)
# Date: 13026 (yes) / 1017 (no)

# Set sampling year to year_min for the following datasets:
# (we checked the source document and found that the sampling year is < 1985; similar to 
# RADAMBRASIL)
# ctb0023, ctb0028, ctb0603, ctb0608, ctb0635, ctb0666, ctb0682, ctb0829, ctb0702
target_year <- year_min
ctb <- c(
  "ctb0023", "ctb0028", "ctb0603", "ctb0608", "ctb0635", "ctb0666", "ctb0682", "ctb0829",
  "ctb0702"
)
# Set sampling_year to target_year and data_ano_fonte to "estimativa"
br_soil2023[dataset_id %in% ctb & is.na(data_ano), `:=`(
  data_ano = target_year,
  data_ano_fonte = "estimativa"
)]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id %in% ctb & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_ano), c("dataset_id", "observacao_id")]))
# 847 events remain without sampling date

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
  .(data_ano = round(mean(data_ano, na.rm = TRUE))),
  by = dataset_id
]
idx_averaged <- match(
  br_soil2023[is.na(data_ano) & dataset_id %in% ctb, dataset_id],
  average_year[, dataset_id]
)
# Set sampling_year to the average sampling year and data_ano_fonte to "estimativa"
br_soil2023[
  is.na(data_ano) & dataset_id %in% ctb,
  `:=`(
    data_ano = average_year[idx_averaged, data_ano],
    data_ano_fonte = "estimativa"
  )
]
# Set data_ano_fonte = original in the remaining events
br_soil2023[
  is.na(data_ano) & dataset_id %in% ctb & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_ano), c("dataset_id", "observacao_id")]))
# 66 events remain without sampling date

# ctb0009
# events missing the sampling date are from ctb0003, thus they are already included in the dataset
ctb <- "ctb0009"
br_soil2023 <- br_soil2023[!(dataset_id %in% ctb & is.na(data_ano)), ]
nrow(unique(br_soil2023[is.na(data_ano), c("dataset_id", "observacao_id")]))
# 34 events remain without sampling date, all from ctb0029

# ctb0029
# events from municipio_id %in% c("Santa Maria", "Itaara"), amostra_tipo == "COMPOSTA",
# amostra_quanti == 3, and data_ano == 2009 are from ctb0003, thus they are already included in
# the dataset and can be removed
ctb <- "ctb0029"
br_soil2023 <- br_soil2023[!(
  dataset_id == ctb & municipio_id %in% c("Santa Maria", "Itaara") &
    amostra_tipo == "COMPOSTA" & amostra_quanti == 3 & data_ano == 2009
), ]
# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_ano), c("dataset_id", "observacao_id")]))
# 34 events remain without sampling date, all from ctb0029
# Soil samples were taken around 2009, but the exact date is not known.
# ctb0029: Set sampling year to 2009 and data_ano_fonte to "estimativa"
br_soil2023[dataset_id == ctb & is.na(data_ano), `:=`(
  data_ano = 2009,
  data_ano_fonte = "estimativa"
)]

# Set data_ano_fonte = original in the remaining events
br_soil2023[
  dataset_id == ctb & !is.na(data_ano) & is.na(data_ano_fonte),
  data_ano_fonte := "original"
]

# Check how many events remain without sampling date
nrow(unique(br_soil2023[is.na(data_ano), c("dataset_id", "observacao_id")]))
# 0 events remain without sampling date

# Check how many events have spatial coordinates (coord_x and coord_y) and
nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")])) # 13973 events
nrow(br_soil2023)
# 50400 layers

# Temporal distribution of samples with known sampling date after data rescue and estimation
missing_time <- is.na(br_soil2023[["data_ano"]])
# Plot histogram
file_path <- "res/fig/103_temporal_distribution_after_estimation.png"
png(file_path, width = 8, height = 5, units = "in", res = 300)
x <- br_soil2023[, data_ano[1], by = c("dataset_id", "observacao_id")][, V1]
hist(x,
  xlab = "Year", main = "Temporal distribution of events after data rescue and estimation",
  sub = paste0("n = ", sum(!missing_time))
)
rug(x)
dev.off()
rm(missing_time, file_path, x)

# Remove year_min from the dataset, updating data_ano_fonte to NA_character_
br_soil2023[data_ano == year_min, data_ano_fonte := NA_character_]
br_soil2023[data_ano == year_min, data_ano := NA_integer_]

# Check for consisteny of data_coleta_ano and data_ano_fonte
# Should be an empty table
nrow(br_soil2023[
  is.na(data_ano) & !is.na(data_ano_fonte),
  .(dataset_id, observacao_id, data_ano, data_ano_fonte)
]) == 0
# TRUE

# Write data to disk ###############################################################################
summary_soildata(br_soil2023)
# Layers: 50400
# Events: 13973
# Georeference: 10942 (yes) / 3031 (no)
# Date: 12380 (yes) / 1593 (no)
# Datasets: 235
data.table::fwrite(br_soil2023, "data/10_soildata.txt", sep = "\t")

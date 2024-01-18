# title: "Brazilian Soil Dataset 2019"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"
# 
# Summary
# This R script downloads and processes soil observation data from the Brazilian Soil Data
# Repository (FEBR). It uses the observation() function of the R package febr, setting the
# harmonization argument to the third level, and applies specific standardization parameters. The
# script then processes the data for three soil taxonomic classification systems, merging columns
# with soil taxonomic classifications from different versions of the same system, prioritizing the
# most recent classifications. The goal is to harmonize and standardize the soil observation data
# for further analysis.

# Download and process data from the 'observacao' table of the FEBR database.
# Set the harmonization argument to the third level, that is, return variables as is.
# The standard variables downloaded are the following:
# dataset_id. Identification code of the dataset in the FEBR to which an observation belongs.
# evento_id_febr. Identification code of an observation in a dataset.
# evento_data. Date (dd-mm-yyyy) in which an observation was made.
# coord_datum. EPSG code of the coordinate reference system.
# coord_longitude. Longitude (deg) or easting (m).
# coord_latitude. Latitude (deg) or northing (m).
# coord_precisao. Precision with which the spatial coordinates were determined (m).
# coord_fonte. Source of the spatial coordinates.
# pais_id. Code (ISO 3166-1 alpha-2) of the county where an observation was made.
# estado_sigla. Acronym of the Brazilian federative unit where an observation was made.
# municipio_nome. Name of the Brazilian municipality where as observation was made.
# subamostra_quanti. Number of sub samples taken (used to indicate composite sampling).
# amostra_area. Sampling area (used to indicate areal or block sampling).
vars <- "taxon_"
observacao <- febr::observation(
  dataset = "all",
  variable = vars,
  stack = TRUE,
  standardization = list(
    crs =  "EPSG:4674", 
    time.format = "%d-%m-%Y", 
    units = FALSE, round = FALSE),
  harmonization = list(harmonize = TRUE, level = 3))

# Data processing
# Data on the soil taxonomic classification are processed as follows:
# 1. Merge columns with soil taxonomic classifications from different versions of the same taxonomic
# classification system. Priority is given to the classification on the most recent version of a
# system. In the case of the Brazilian Soil Classification System, 'sibcs,' classifications up to
# the 1999 version are ignored because the number of classes and the nomenclature used differ from
# later versions. In this case, observations with only the old taxonomic classification of 'sibcs'
# (1999 and before) are left without data ('NA_character'). The identification codes for the three
# resulting columns from the fusion of each of the three taxonomic classification systems are
# 'taxon_sibcs,' 'taxon_st,' and 'taxon_wrb.'
# 2. For 'taxon_sibcs,' when the taxonomic classification is recorded in acronym form, replace it
# with the corresponding full name. Next, eliminate lower categorical levels, retaining only the
# first (order) and the second (suborder).
# 3. For 'taxon_sibcs,' 'taxon_st_,' and 'taxon_wrb_,' clean and standardize the formatting of
# character strings representing each soil taxonomic classification (uppercase, without diacritics,
# and without periods).

# The columns in the resulting data table are organized as follows:
# 1. Discard columns 'taxon_sibcs_<...>', 'taxon_st_<...>', and 'taxon_wrb_<...>', where '<...>' is
# the year of the classification system. These columns have been processed as described above and
# replaced by the columns 'taxon_sibcs_', 'taxon_st_', and 'taxon_wrb', respectively.
# 2. Discard the 'coord_sistema' column since the spatial coordinates for all observations are
# standardized to the SIRGAS 2000 coordinate reference system (EPSG:4674).
# 3. Arrange the columns with identification data ('observacao_id', 'sisb_id', and 'ibge_id') side
# by side at the beginning of the data table.

# The data type of the columns is also defined here:
# 1. The data in the 'coord_precisao' column is set to the real type using 'as.numeric()'.
sibcs_tabela <- 
  "1yJ_XnsJhnhJSfC3WRimfu_3_owXxpfSKaoxCiMD2_Z0" %>% 
  googlesheets::gs_key() %>% 
  googlesheets::gs_read_csv()
sibcs_siglas <- sibcs_tabela$campo_codigo
sibcs_tabela <- as.list(sibcs_tabela$campo_nome)
names(sibcs_tabela) <- sibcs_siglas
pruneStrings <- 
  function (x, n, split = " ", collapse = " ") {
    res <- 
      sapply(x, function (x) {
        res <- strsplit(x = x, split = split)[[1]][1:n]
        res <- paste(res, collapse = collapse)
        res <- as.character(res)
        res <- gsub(pattern = "NA NA", replacement = NA_character_, x = res)
        res <- gsub(pattern = " NA$", "", res)
        return (res)
      })
    return (res)
  }
observacao <- 
  observacao %>% 
  dplyr::mutate(
    taxon_sibcs = dplyr::coalesce(
      taxon_sibcs_2013, taxon_sibcs_2009, taxon_sibcs_2006, taxon_sibcs_2003),
      # taxon_sibcs_1999,
      # taxon_sibcs_19xx, taxon_sibcs_xxx, taxon_sibcs_xxx_1, taxon_sibcs_xxx_2, taxon_sibcs_xxxx),
    taxon_sibcs = dplyr::recode(.data$taxon_sibcs, !!!sibcs_tabela),
    taxon_sibcs = pruneStrings(taxon_sibcs, n = 2),
    taxon_sibcs = toupper(taxon_sibcs),
    taxon_sibcs = gsub(pattern = ".", "", .data$taxon_sibcs, fixed = TRUE),
    taxon_sibcs = chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", .data$taxon_sibcs),
    taxon_sibcs = chartr("âêîôûÂÊÎÔÛ", "aeiouAEIOU", .data$taxon_sibcs),
    taxon_st = dplyr::coalesce(taxon_st_2010, taxon_st_1999, taxon_st_xxx),
    taxon_st = toupper(taxon_st),
    taxon_st = gsub(pattern = ".", "", .data$taxon_st, fixed = TRUE),
    taxon_wrb = dplyr::coalesce(taxon_wrb_2006, taxon_wrb_1998, taxon_wrb_xxx),
    taxon_wrb = toupper(taxon_wrb),
    taxon_wrb = gsub(pattern = ".", "", .data$taxon_wrb, fixed = TRUE),
    coord_precisao = as.numeric(coord_precisao),
    coord_x = round(coord_x, 8),
    coord_y = round(coord_y, 8),
    amostra_tipo = toupper(amostra_tipo),
    amostra_quanti = as.integer(amostra_quanti),
    municipio_id = gsub(pattern = ".", "", .data$municipio_id, fixed = TRUE)
  ) %>% 
  dplyr::select(
    dataset_id, observacao_id, sisb_id, ibge_id, observacao_data, coord_x, coord_y, coord_precisao, 
    coord_fonte, pais_id, estado_id, municipio_id, amostra_tipo, amostra_quanti, amostra_area,
    taxon_sibcs, taxon_st, taxon_wrb)

# Save the data in TXT format
write.table(observacao, file = "data/observacao.txt", sep = ";", dec = ",", row.names = FALSE)

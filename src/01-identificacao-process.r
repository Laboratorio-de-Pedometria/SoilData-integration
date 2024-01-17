# title: "Brazilian Soil Dataset 2020"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"
# 
# Summary
# This R script handles the 'identification' table extracted from datasets available in the
# Brazilian Soil Data Repository (FEBR). It selectively retrieves particular rows, including
# 'dataset_id,' 'dataset_titulo,' and 'dataset_licenca,' among others. Additionally, it splits
# multiple entries in the 'area_conhecimento,' 'autor_nome,' and 'organizacao_nome' fields,
# retaining only the initial entry. The objective is to streamline and standardize the dataset for
# subsequent analysis.
 
# Load packages
library(febr)
# library(dplyr)
# library(magrittr)
 
# Read the identification table from the Brazilian Soil Data Repository
identificacao <- febr::identification(dataset = "all", febr.repo = "~/ownCloud/febr-repo/publico")
dataset <- febr::dataset(dataset = 'all')
# str(dataset, 1)
# sapply(dataset, colnames)
 
# Selectively extract specific rows from the identification table
# and split multiple entries in 'area_conhecimento', 'autor_nome', and 'organizacao_nome' fields,
# retaining only the first entry.
dts_rows <- c(
  "dataset_id", "dataset_titulo", "dataset_licenca", "dataset_versao",
  "publicacao_data", "palavras_chave"
)
dataset <- 
  sapply(dataset, function (x) {
    i1 <- match(dts_rows, x[[1]])
    x1 <- x[i1, 2]
    # Keep only the first area of knowledge listed
    i2 <- match("area_conhecimento", x[[1]])
    x2 <- x[i2, 2] %>% stringr::str_split_fixed(pattern = ";", n = Inf)
    # Keep only the first author listed
    i3 <- match("autor_nome", x[[1]])
    x3 <- x[i3, 2] %>% stringr::str_split_fixed(pattern = ";", n = Inf)
    # Keep only the first institution listed
    i4 <- match("organizacao_nome", x[[1]])
    x4 <- x[i4, 2] %>% stringr::str_split_fixed(pattern = ";", n = Inf)
    # Return a data frame with the selected rows
    c(x1, x2[1], x3[1], x4[1])
  })
dataset <- t(dataset) %>% data.frame()
colnames(dataset) <- c(dts_rows, "area_conhecimento", "autor_nome", "organizacao_nome")

# Remove double spaces in the title
dataset$dataset_titulo <- gsub(pattern = '  ', replacement = ' ', x = dataset$dataset_titulo)

# Set the order of columns
col_order <- c(
  "dataset_id", "dataset_titulo", "autor_nome", "organizacao_nome", "dataset_licenca",
  "dataset_versao", "publicacao_data", "palavras_chave", "area_conhecimento"
)
dataset <- dataset[col_order]
# Alterar licença dos conjuntos de dados da Embrapa
# dataset %<>%
#   mutate(
#     dataset_id = as.character(dataset_id),
#     tmp = gsub("ctb", "", dataset_id),
#     tmp = as.integer(tmp),
#     dataset_licenca = as.character(dataset_licenca),
#     dataset_licenca = ifelse(tmp > 100, "CC BY-NC 4.0", dataset_licenca)
#   ) %>%
#   select(-tmp)

# Save the dataset in TXT format
write.table(dataset,
  file = glue::glue("../data/febr-dataset.txt"), sep = ";", dec = ",",
  row.names = FALSE
)

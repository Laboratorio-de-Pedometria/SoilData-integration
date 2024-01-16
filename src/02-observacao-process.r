# title: "Brazilian Soil Dataset 2020"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

## Tabelas "observacao"

### Descarregamento

# Os dados das tabelas "observacao" de cada conjunto de dados são descarregados utilizando o nível
# 3 de harmonização.
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

### Processamento

# Para os três sistemas de classificação taxonômica do solo, o processamento dos dados segue os
# seguintes passos:
# 1. Fusão das colunas com a classificação taxonômica do solo nas diferentes versões de um mesmo
# sistema de classificação taxonômica. Prioridade é dada à classificação mais recente. No caso do
# Sistema Brasileiro de Classificação do Solos, 'sibcs', classificações até 1999 são ignoradas, pois
# o número de classes e a nomenclatura utilizada são diferentes da versão atual. Nesse caso,
# observações apenas com a classificação taxonômica antiga do 'sibcs' ficam sem dados
# ('NA_character'). Os códigos de identificação das colunas resultantes da fusão das colunas de cada
# um dos três sistema de classificação taxonômica são 'taxon_sibcs', 'taxon_st' e 'taxon_wrb'.
# 2. Para 'taxon_sibcs', substituição da classificação taxonômica registrada na forma de sigla pelo
# nome correspondente por extenso, seguida da eliminação de níveis categóricos inferiores,
# mantendo-se apenas o primeiro (ordem) e o segundo (subordem).
# 3. Para 'taxon_sibcs', 'taxon_st_' e 'taxon_wrb_', limpeza e padronização da formatação das
# correntes de caracteres que representam cada classificação taxonômica do solo (caixa alta, sem
# acentuação, sem ponto final).

# As colunas são organizadas de maneira a:
# 1. Descartar as colunas 'taxon_sibcs_<...>', 'taxon_st_<...>' e 'taxon_wrb_<...>', processadas
# acima e substituídas pelas colunas 'taxon_sibcs_', 'taxon_st_' e 'taxon_wrb', respectivamente.
# 2. Descartar a colunas 'coord_sistema', uma vez que as coordenadas espaciais de todas as
# observações foram padronizadas para o sistema de referência de coordenadas SIRGAS 2000
# (EPSG:4674).
# 3. Posicionar as colunas com dados de identificação -- 'observacao_id', 'sisb_id' e 'ibge_id'
# lado-a-lado.

# O tipo de dados das colunas também é definido aqui:
# 1. Os dados da coluna 'coord_precisao' são definidos como do tipo real usando 'as.numeric()'.
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

### Salvar dados
# Salvar os dados no formato TXT
write.table(observacao,
  file = glue::glue("../data/febr-observacao.txt"), sep = ";", dec = ",", row.names = FALSE
)
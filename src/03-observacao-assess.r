# title: "Brazilian Soil Dataset 2020"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

### Análise exploratória dos dados
observacao <- read.table(glue::glue("../data/febr-observacao.txt"),
  sep = ";", dec = ",",
  header = TRUE
)

# Avaliar o resultado usando alguns registros selecionados aleatoriamente
mess <-
  function (x) {
    n <- nrow(x)
    x[sample(x = seq(n), size = n), ]
  }
mess(observacao)

tmp <-
  observacao %>% 
  dplyr::summarise(
    Total = n(),
    `Lat/Long` = sum(!is.na(coord_x)),
    Precisão = median(coord_precisao, na.rm = TRUE),
    GPS = sum(coord_fonte == "GPS", na.rm = TRUE),
    MAPA = sum(coord_fonte == "MAPA", na.rm = TRUE),
    WEB = sum(coord_fonte == "WEB", na.rm = TRUE)
  ) %T>% 
  print()

# O FEBR dispõe de um total de 15158 observações do solo. Destas, 11970 possuem coordenadas
# espaciais, ou seja, 11970 / 15158 * 100 = 78.9682%. A precisão mediana das coordenadas espaciais é
# de 100 m. A fonte de boa parte das coordenadas é desconhecida, sendo o GPS a fonte mais comum
# (5309). Boa parte das coordenadas foi estimada usando serviços de mapas online (693), ou então
# foram estimadas usando, por exemplo, mapas base (693).

br <- sf::read_sf("../data/vector/br.shp")
tmp <- 
  observacao$estado_id %>% 
  table()
idx <- match(br$SigUF, names(tmp))
dens <- tmp[idx] / br$area
dens <- dens[order(dens)]
png("../res/fig/febr-observacao-espaco.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2
)
# png("../res/fig/febr-observacao-espaco-saopaulo.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
# png("../res/fig/febr-observacao-espaco-goias.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
plot(
  br[1], graticule = TRUE, axes = TRUE, 
  sub = Sys.Date(), 
  # main = "Distribuição espacial das observações",
  # main = "Spatial distribution of observations",
  main = "",
  col = 0, reset = FALSE)
observacao %>%
  dplyr::filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  sf::st_as_sf(coords = c('coord_x', 'coord_y'), crs = 4326) %>%
  # dplyr::select(estado_id) %>%
  # plot(
  # cex = ifelse(.$estado_id == "GO", 0.5, 0.3),
  # pch = ifelse(.$estado_id == "GO", 20, 1),
  # col = ifelse(.$estado_id == "GO", "firebrick1", "lightgray"), add = TRUE)
  # plot(
  # cex = ifelse(.$estado_id == "SP", 0.5, 0.3),
  # pch = ifelse(.$estado_id == "SP", 20, 1),
  # col = ifelse(.$estado_id == "SP", "firebrick1", "lightgray"), add = TRUE)
  plot(cex = 0.5, col = "firebrick1", add = TRUE)
dev.off()

tmp2 <- 
  data.frame(
    n_max = max(tmp),
    n_min = min(tmp),
    d_max = max(dens),
    d_min = min(dens)
  ) %T>% 
  print()

# A distribuição espacial das observações do solo é bastante heterogênea, com inúmeros agrupamentos
# de observações aparecendo em várias partes do território nacional. Consequentemente, amplos vazios
# de observações aparecem. O estado de Rondônia concentra o maior número de observações. Enquanto
# isso, o menor número de observações parece no estado do Tocantis. Devido ao seu relativamente 
# grande território, o estado do Tocantis também possui a menor densidade de observação. A maior
# densidade de observação é no Distrito Federal. Em geral, os estados das regiões Centro-Oeste, 
# Norte e Nordeste são aqueles com as menores densidades de observação, refletindo o histórico de
# ocupação do território brasileiro e os investimentos feitos no ensino e pesquisa via universidades
# e centros de pesquisa agronômica.

png("../res/fig/febr-observacao-uf.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-uf-saopaulo.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
# png("../res/fig/febr-observacao-uf-goias.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
tmp %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Número",
    # ylab = "Federative unit", xlab = "Number",
    horiz = TRUE, ann = TRUE)
grid()
dens %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Densidade (por 1000 km²)",
    # ylab = "Federative unit", xlab = "Density (per 1000 km²)",
    horiz = TRUE, ann = TRUE)
title(
  main = "Distribuição das observações pelas unidades da federação",
  # main = "Observations by federative unit",
  sub = Sys.Date(), outer = TRUE)
grid()
dev.off()

# Das observações sem coordenadas espaciais, a maioria se encontra nos estados da Bahia, Minas
# Gerais, Paraná e Pará. Três conjuntos de dados concentram parte considerável dessas observações
# (ctb0657, ctb0831 e ctb0775), cada um com mais de 100 observações sem coordenadas espaciais.
# Outros 14 conjuntos de dados possuem entre 50 e 100 observações sem coordenadas espaciais.

png("../res/fig/febr-observacao-sem-coordenadas.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2
)
# png("../res/fig/febr-observacao-sem-coordenadas-goias.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
# png("../res/fig/febr-observacao-sem-coordenadas-saopaulo.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
observacao %>% 
  dplyr::filter(is.na(coord_x)) %>% 
  dplyr::select(estado_id) %>% 
  dplyr::mutate(estado_id = as.factor(estado_id)) %>% 
  table() %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Número de observações",
    # ylab = "Federative unit", xlab = "Number",
    horiz = TRUE)
grid()
observacao %>% 
  dplyr::filter(is.na(coord_x)) %>% 
  dplyr::select(dataset_id) %>% 
  dplyr::mutate(dataset_id = as.factor(dataset_id)) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(27L) %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    names.arg = gsub("ctb0", "", names(.)),
    ylab = "Conjunto de dados", xlab = "Número de observações",
    # ylab = "Dataset ID", xlab = "Number",
    horiz = TRUE)
grid()
title(
  main = "Distribuição das observações sem coordenadas espaciais",
  # main = "Observations without spatial coordinates",
  sub = Sys.Date(), outer = TRUE)
dev.off()

observacao %>%
  dplyr::mutate(observacao_data = as.Date(observacao_data) %>% format("%Y")) %>%
  dplyr::summarise(
    Total = dplyr::n(),
    Data = sum(!is.na(observacao_data)),
    `<1960` = sum(observacao_data < 1960, na.rm = TRUE),
    `1960-1990` = (sum(observacao_data < 1990, na.rm = TRUE) - `<1960`),
    `1990-2010` = (sum(observacao_data < 2010, na.rm = TRUE) - `<1960` - `1960-1990`),
    `2010-2019` = (sum(observacao_data < 2019, na.rm = TRUE) - `<1960` - `1960-1990` - `1990-2010`)
  ) %>%
  dplyr::mutate(
    `<1960` = round(`<1960` / Data * 100, 2),
    `1960-1990` = round(`1960-1990` / Data * 100, 2),
    `1990-2010` = round(`1990-2010` / Data * 100, 2),
    `2010-2019` = round(`2010-2019` / Data * 100, 2)
  )

# Apenas cerca de 10087 / 15158 * 100 = 66.54572% das observações do solo possuem registro da data
# de observação. Destas, metade (49.26%) foram obtidas entre 1990 e 2010. Isso se deve a um pico no
# ano de 1997, ano em que foram obtidas as observações do solo do maior conjunto de dados disponível
# no FEBR. Trata-se do conjunto de dados do Zoneamento Agroecológico do Estado de Rondônia, que
# possui mais de 2000 observações do solo. Das observações sem data de observação, acredita-se que a
# maioria foi produzida na década de 1970, período de maior realização de levantamentos de solo no
# Brasil.

# A distribuição das observações do solo no tempo mostra que há um vazio entre as décadas de 1980 e
# 1990. Esse período foi marcado pelo término dos principais projetos/programas de mapeamento do
# solo em larga escala no Brasil. Contudo, como parte considerável das observações do solo não
# possui informação sobre a data de observação, não é possível identificar a razão do vazio com
# precisão.

observacao[!is.na(observacao$coord_x), c("coord_x", "coord_y", "observacao_data")] %>% 
  duplicated() %>% 
  sum()

png("../res/fig/febr-observacao-tempo.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
# png("../res/fig/febr-observacao-tempo-saopaulo.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
# png("../res/fig/febr-observacao-tempo-goias.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
par(oma = c(0, 0, 0, 0), las = 1, mar = c(4, 4, 2, 1))
tmp <- 
  observacao %>% 
  select(observacao_data) %>% 
  mutate(
    observacao_data = as.Date(observacao_data),
    observacao_data = format(observacao_data, "%Y")) %>% 
  table()
barplot(tmp, col = "lightgray", border = "lightgray")
# barplot(tmp, col = "firebrick1", border = "firebrick1")
grid()
tmp %>% 
  barplot(
    # col = "firebrick1", border = "firebrick1",
    col = "lightgray", border = "lightgray",
    # sub = glue::glue("Versão {Sys.Date()}"),
    xlab = "Ano", ylab = "Número de observações",
    # xlab = "Year", ylab = "Number",
    main = "Distribuição temporal das observações",
    # main = "Temporal distribution of observations",
    add = TRUE, ann = TRUE)
# tmp2 <-
# observacao %>%
#   # filter(estado_id == "GO") %>%
#   filter(estado_id == "SP") %>%
#   select(observacao_data) %>%
#   mutate(
#     observacao_data = as.Date(observacao_data),
#     observacao_data = format(observacao_data, "%Y")) %>%
#   table()
# id <- which(names(tmp) %in% names(tmp2))
# tmp[id] <- tmp2
# tmp[-id] <- NA
# barplot(tmp, col = "firebrick1", border = "firebrick1", add = TRUE, xaxt = "n", yaxt = "n")
dev.off()

# A maioria das observações com data de observação desconhecida pertencem a conjuntos de dados que
# abrangem os estados do Amazonas, Santa Catarina, Bahia, Rio Grande do Sul, Pará, Minas Gerais e
# Paraná. Dentre estes, três conjuntos de dados se destacam pelo grande número de observações sem
# data. São eles: ctb0572, ctb0770 e ctb0657.

png("../res/fig/febr-observacao-sem-data.png",
  width = 480 * 2, height = 480 * 2, res = 72 * 2
)
# png("../res/fig/febr-observacao-sem-data-goias.png",
#   width = 480 * 2, height = 480 * 2, res = 72 * 2
# )
# png("../res/fig/febr-observacao-sem-data-saopaulo.png",
#   width = 480 * 3, height = 480 * 2, res = 72 * 2
# )
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
observacao %>% 
  dplyr::filter(is.na(observacao_data)) %>% 
  dplyr::select(estado_id) %>% 
  dplyr::mutate(estado_id = as.factor(estado_id)) %>% 
  table() %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    ylab = "Unidade da federação", xlab = "Número de observações",
    # ylab = "Federative unit", xlab = "Number",
    horiz = TRUE)
grid()
observacao %>% 
  dplyr::filter(is.na(observacao_data)) %>% 
  dplyr::select(dataset_id) %>% 
  dplyr::mutate(dataset_id = as.factor(dataset_id)) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(27L) %>% 
  sort() %>% 
  barplot(
    col = "firebrick1", border = "firebrick1",
    # col = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "GO", "firebrick1", "lightgray"),
    # col = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    # border = ifelse(names(.) == "SP", "firebrick1", "lightgray"),
    names.arg = gsub("ctb0", "", names(.)),
    ylab = "Conjunto de dados", xlab = "Número de observações",
    # ylab = "Dataset ID", xlab = "Number",
    horiz = TRUE)
grid()
title(
  main = "Distribuição das observações com data desconhecida",
  # main = "Observations without temporal coordinate",
  sub = Sys.Date(), outer = TRUE)
dev.off()
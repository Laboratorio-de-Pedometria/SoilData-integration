# title: "Brazilian Soil Dataset 2020"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

# Summary
# This R script assesses the 'observacao' table extracted from datasets available in the Brazilian
# Soil Data Repository (FEBR). It evaluates the result using some randomly selected records and
# summarizes the data frame, calculating the total count, the count of non-missing latitude/
# longitude values, the median of 'coord_precisao', and the counts of 'coord_fonte' being "GPS",
# "MAPA", or "WEB". The script also assesses the spatial distribution of the observations, computes
# the number of observations and the density of observations per 1000 km² for each state, and
# prepares a figure with the number of observations and the density of observations per 1000 km²
# for each state. Finally, the script transforms the 'observacao_data' column to a year format,
# summarizes the 'observacao' data frame by counting the total observations and those within
# specific year ranges, and then calculates the percentage of total observations for each year
# range. The script also counts the number of duplicated observations and prepares a figure with the
# number of observations for each year. The goal is to assess the soil observation data for further
# analysis.

# Exploratory data analysis
# Read the 'observacao' table from the Brazilian Soil Data Repository
observacao <- read.table("../data/febr-observacao.txt", sep = ";", dec = ",", header = TRUE)

# Evaluate the result using some randomly selected records
mess <-
  function (x) {
    n <- nrow(x)
    x[sample(x = seq(n), size = n), ]
  }
mess(observacao)

# Summarize the 'observacao' data frame, calculating the total count, the count of non-missing
# latitude/longitude values, the median of 'coord_precisao', and the counts of 'coord_fonte' being
# "GPS", "MAPA", or "WEB".
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

# The FEBR repository has a total of 15,158 soil observations. Among these, 11,970 have spatial
# coordinates, which corresponds to 78.9682% (11,970 / 15,158 * 100). The median precision of the
# spatial coordinates is 100 meters. The source of a significant portion of the coordinates is
# unknown, with GPS being the most common source (5,309). Many coordinates were estimated using
# online map services (693), or alternatively, were estimated using, for example, base maps (693).

# Assess the spatial distribution of the observations
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

# Compute the number of observations and the density of observations per 1000 km² for each state
tmp2 <- 
  data.frame(
    n_max = max(tmp),
    n_min = min(tmp),
    d_max = max(dens),
    d_min = min(dens)
  ) %T>% 
  print()

# The spatial distribution of soil observations is highly heterogeneous, with numerous clusters of
# observations appearing in various parts of the national territory. Consequently, large observation
# gaps are evident. The state of Rondônia has the highest number of observations. Meanwhile, the
# lowest number of observations appears in the state of Tocantins. Due to its relatively large
# territory, Tocantins also has the lowest observation density. The highest observation density is
# in the Federal District. In general, the states in the Central-West, North, and Northeast regions
# have the lowest observation densities, reflecting the historical occupation of the Brazilian
# territory and the investments made in education and research through universities and agronomic
# research centers.

# Prepare figure with the number of observations and the density of observations per 1000 km² for
# each state
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

# Of the total soil observations without spatial coordinates, the majority are located in the states
# of Bahia, Minas Gerais, Paraná, and Pará. Three datasets account for a significant portion of
# these observations (ctb0657, ctb0831, and ctb0775), each with over 100 observations lacking
# spatial coordinates. Another 14 datasets have between 50 and 100 observations without spatial
# coordinates.

# Prepare figure with the number of observations without spatial coordinates for each state
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

# Transform the 'observacao_data' column to a year format, summarize the 'observacao' data frame by
# counting the total observations and those within specific year ranges, and then calculates the
# percentage of total observations for each year range.
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

# Only about 66.54572% of soil observations have recorded observation dates, which translates to
# approximately 10,087 out of 15,158 observations. Among these, half (49.26%) were obtained between
# 1990 and 2010. This is attributed to a peak in the year 1997, the year in which soil observations
# from the largest dataset available in FEBR were obtained. This dataset is the Agroecological
# Zoning of the State of Rondônia, containing over 2,000 soil observations. Regarding observations
# without observation dates, it is believed that the majority were produced in the 1970s, a period
# of extensive soil surveys in Brazil.
# The distribution of soil observations over time reveals a gap between the 1980s and 1990s. This
# period was marked by the conclusion of major large-scale soil mapping projects/programs in Brazil.
# However, due to a significant portion of soil observations lacking information on the observation
# date, it is not possible to precisely identify the reason for this gap.

# Count the number of duplicated observations
observacao[!is.na(observacao$coord_x), c("coord_x", "coord_y", "observacao_data")] %>% 
  duplicated() %>% 
  sum()

# Prepare figure with the number of observations for each year
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

# The majority of soil observations with unknown observation dates belong to datasets covering the
# states of Amazonas, Santa Catarina, Bahia, Rio Grande do Sul, Pará, Minas Gerais, and Paraná.
# Among these, three datasets stand out due to a large number of observations without dates. They
# are: ctb0572, ctb0770, and ctb0657.

# Prepare figure with the number of observations without dates for each state
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

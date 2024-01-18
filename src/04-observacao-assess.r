# title: "Brazilian Soil Dataset 2023"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2024-01-18"

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
# range. The script also counts the number of duplicated observations and prepares a figure with
# the number of observations for each year. The goal is to assess the soil observation data for
# further analysis.

# Install and load required packages
if (!require("rnaturalearth")) {
  install.packages("rnaturalearth", dependencies = TRUE)
}
southamerica <- rnaturalearth::ne_countries(continent = c("south america", "europe"),
  returnclass = "sf", scale = "medium")
southamerica <- southamerica[, "iso_a2"]

# Exploratory data analysis
# Read the 'observacao' table from the Brazilian Soil Data Repository
observacao <- read.table("data/observacao.txt", sep = ";", dec = ",", header = TRUE)

# Evaluate the result using some randomly selected records
mess <-
  function (x) {
    n <- nrow(x)
    x[sample(x = seq(n), size = n), ]
  }
mess(observacao)

# Count the number of duplicated observations using the geographic and temporal coordinates
idx_has_coordinates <-
  !is.na(observacao$coord_x) & !is.na(observacao$coord_y) & !is.na(observacao$observacao_data)
idx_duplicated <- duplicated(
  observacao[idx_has_coordinates, c("coord_x", "coord_y", "observacao_data")]
)
sum(idx_duplicated)
# There are 277 duplicated observations in the 'observacao' table. This occurs when certain soil
# observations are reused across different datasets. It's important to note that only observations
# containing both geographic and temporal coordinates are considered in this count. Consequently,
# there might be additional duplicated observations that cannot currently be identified due to the
# absence of geographic and/or temporal coordinates. While we retain these duplicated observations
# in the dataset, it's ultimately the user's decision whether to keep or remove them during data
# analysis. However, for the analysis conducted here, they are excluded to ensure accuracy.

# Summarize the 'observacao' data frame, calculating the total count, the count of non-missing
# latitude/longitude values, the median of 'coord_precisao', and the counts of 'coord_fonte' being
# "GPS", "MAPA", or "WEB". Consider only the unique observations.
tmp <-
  observacao[!idx_duplicated, ] %>% 
  dplyr::summarise(
    Total = n(),
    `Lat/Long` = sum(!is.na(coord_x)),
    Precisão = median(coord_precisao, na.rm = TRUE),
    GPS = sum(coord_fonte == "GPS", na.rm = TRUE),
    MAPA = sum(coord_fonte == "MAPA", na.rm = TRUE),
    WEB = sum(coord_fonte == "WEB", na.rm = TRUE)
  ) %T>% 
  print()

round(tmp["Lat/Long"] / tmp["Total"] * 100)
# The FEBR repository has a total of 14,786 unique soil observations. Among these, 11,664 have
# spatial coordinates, which corresponds to 79%. The median precision of the spatial coordinates is
# 100 meters. The source of a significant portion of the coordinates is unknown, with GPS being the
# most common source (5,254). Many coordinates were estimated using web map services (602), or
# alternatively, were estimated using, for example, base maps (722).

# Assess the spatial distribution of the observations
br <- sf::read_sf("data/vector/br.shp")
tmp <- 
  observacao[!idx_duplicated, ]$estado_id %>% 
  table()
idx <- match(br$SigUF, names(tmp))
dens <- tmp[idx] / br$area
dens <- dens[order(dens)]

# Prepare figure with the spatial distribution of the observations in the Brazilian territory
png("res/fig/observacao-espaco.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(br[1],
  reset = FALSE, main = "", col = "transparent",
  axes = TRUE, graticule = TRUE, lwd = 0.01
)
plot(southamerica,
  reset = FALSE,
  col = "gray96",
  add = TRUE, lwd = 0.5
)
plot(br[1], reset = FALSE,
  main = "",
  axes = TRUE, col = "#eeece1", lwd = 0.5,
  border = "gray69",
  key.pos = NULL, graticule = TRUE, add = TRUE)
observacao[!idx_duplicated, ] %>%
  dplyr::filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  sf::st_as_sf(coords = c("coord_x", "coord_y"), crs = 4326) %>%
  plot(cex = 0.5, col = "firebrick", add = TRUE)
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
png("res/fig/observacao-uf.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
tmp %>% 
  sort() %>% 
  barplot(
    col = "gray", border = NA,
    ylab = "Federative unit", xlab = "Absolute frequency",
    horiz = TRUE, ann = TRUE)
abline(v = seq(0, 3000, 500), col = "gray", lty = 2)
dens %>%
  barplot(
    col = "gray", border = NA,
    ylab = "Federative unit", xlab = "Density (per 1000 km²)",
    horiz = TRUE, ann = TRUE
  )
abline(v = seq(0, 25, 5), col = "gray", lty = 2)
dev.off()

# Of the total soil observations without spatial coordinates, the majority are located in the states
# of Bahia, Minas Gerais, Paraná, and Pará. Three datasets account for a significant portion of
# these observations (ctb0657, ctb0831, and ctb0775), each with over 100 observations lacking
# spatial coordinates. Another 14 datasets have between 50 and 100 observations without spatial
# coordinates.

# Prepare figure with the number of observations without spatial coordinates for each state
png("res/fig/observacao-sem-coordenadas.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
observacao[!idx_duplicated, ] %>%
  dplyr::filter(is.na(coord_x)) %>%
  dplyr::select(estado_id) %>%
  dplyr::mutate(estado_id = as.factor(estado_id)) %>%
  table() %>%
  sort() %>%
  barplot(
    col = "gray", border = NA,
    ylab = "Federative unit", xlab = "Absolute frequency",
    horiz = TRUE
  )
abline(v = seq(0, 400, 100), col = "gray", lty = 2)
observacao[!idx_duplicated, ] %>%
  dplyr::filter(is.na(coord_x)) %>%
  dplyr::select(dataset_id) %>%
  dplyr::mutate(dataset_id = as.factor(dataset_id)) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(27L) %>%
  sort() %>%
  barplot(
    col = "gray", border = NA,
    names.arg = gsub("ctb0", "", names(.)),
    ylab = "Dataset ID", xlab = "Absolute frequency",
    horiz = TRUE
  )
abline(v = seq(0, 200, 50), col = "gray", lty = 2)
dev.off()

# Transform the 'observacao_data' column to a year format, summarize the 'observacao' data frame by
# counting the total observations and those within specific year ranges, and then calculates the
# percentage of total observations for each year range.
tmp <-
  observacao[!idx_duplicated, ] %>%
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
  ) %>%
  print()

round(tmp["Data"] / tmp["Total"] * 100)
# Only about 67% of soil observations have recorded observation dates, which translates to
# approximately 9,890 out of 14,786 unique soil observations. Among these, half (49.47%) were
# obtained between 1990 and 2010. This is attributed to a peak in the year 1997, the year in which
# soil observations from the largest dataset available in FEBR were obtained. This dataset is the
# Agroecological Zoning of the State of Rondônia, containing over 2,000 soil observations.
# Regarding observations without observation dates, it is believed that the majority were produced
# in the 1970s, a period of extensive soil surveys in Brazil.

# The distribution of soil observations over time reveals a gap between the 1980s and 1990s. This
# period was marked by the conclusion of major large-scale soil mapping projects/programs in Brazil.
# However, due to a significant portion of soil observations lacking information on the observation
# date, it is not possible to precisely identify the reason for this gap.

# Prepare figure with the number of observations for each year
png("res/fig/observacao-tempo.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(oma = c(0, 0, 0, 0), las = 1, mar = c(4, 4, 2, 1))
tmp <- 
  observacao[!idx_duplicated, ] %>% 
  select(observacao_data) %>% 
  mutate(
    observacao_data = as.Date(observacao_data),
    observacao_data = format(observacao_data, "%Y")) %>% 
  table()
tmp %>% 
  barplot(
    col = "gray", border = NA,
    xlab = "Year", ylab = "Absolute frequency",
    main = "")
abline(h = seq(0, 2500, 500), col = "gray", lty = 2)
dev.off()

# The majority of soil observations with unknown observation dates belong to datasets covering the
# states of Amazonas, Santa Catarina, Bahia, Rio Grande do Sul, Pará, Minas Gerais, and Paraná.
# Among these, three datasets stand out due to a large number of observations without dates. They
# are: ctb0572, ctb0770, and ctb0657.
# Prepare figure with the number of observations without dates for each state
png("res/fig/observacao-sem-data.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
layout(matrix(1:2, nrow = 1), widths = c(2, 2), heights = 1, respect = FALSE)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 4, 1, 1))
observacao[!idx_duplicated, ] %>% 
  dplyr::filter(is.na(observacao_data)) %>% 
  dplyr::select(estado_id) %>% 
  dplyr::mutate(estado_id = as.factor(estado_id)) %>% 
  table() %>% 
  sort() %>% 
  barplot(
    col = "gray", border = NA,
    ylab = "Federative unit", xlab = "Absolute frequency",
    horiz = TRUE)
abline(v = seq(0, 500, 100), col = "gray", lty = 2)
observacao[!idx_duplicated, ] %>% 
  dplyr::filter(is.na(observacao_data)) %>% 
  dplyr::select(dataset_id) %>% 
  dplyr::mutate(dataset_id = as.factor(dataset_id)) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(27L) %>% 
  sort() %>% 
  barplot(
    col = "gray", border = NA,
    names.arg = gsub("ctb0", "", names(.)),
    ylab = "Dataset ID", xlab = "Absolute frequency",
    horiz = TRUE)
abline(v = seq(0, 250, 50), col = "gray", lty = 2)
dev.off()

# Observations with soil classification information
observacao[!idx_duplicated, ] %>%
  dplyr::filter(!is.na(taxon_sibcs)) %>%
  nrow()
# There are 2411 observations with soil classification information according to the Brazilian Soil
# Classification System (SIBCS). This represents 16% of the total number of unique soil observations
# in the 'observacao' table.

# Change taxon_sibcs to lower case, keeping only the first letter of each word capitalized
observacao$taxon_sibcs <- 
  observacao$taxon_sibcs %>% 
  tolower() %>% 
  stringr::str_to_title()

# Prepare figure (barplot) with the distribution os observations per order of the Brazilian System of Soil Classification.
png("res/fig/observacao-sibcs.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(oma = c(0, 0, 1, 0), las = 1, mar = c(4, 13, 1, 1))
observacao[!idx_duplicated, ] %>%
  dplyr::filter(!is.na(taxon_sibcs)) %>%
  dplyr::select(taxon_sibcs) %>%
  dplyr::mutate(taxon_sibcs = as.factor(taxon_sibcs)) %>%
  table() %>%
  sort() %>%
  barplot(
    col = "gray", border = NA,
    ylab = "", xlab = "Absolute frequency",
    horiz = TRUE
  )
abline(v = seq(0, 350, 50), col = "gray", lty = 2)
dev.off()

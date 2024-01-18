# title: "Brazilian Soil Dataset 2019"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

# Read the processed data from the 'camada' table
camada <- read.table("data/camada.txt", sep = ";", dec = ",", header = TRUE)

# Read the processed data from the 'observacao' table. Identify duplicated observations, returning
# the dataset ID and observation ID of the duplicated observations.
observacao <- read.table("data/observacao.txt", sep = ";", dec = ",", header = TRUE)
idx_has_coordinates <-
  !is.na(observacao$coord_x) & !is.na(observacao$coord_y) & !is.na(observacao$observacao_data)
idx_duplicated_obs <- duplicated(
  observacao[idx_has_coordinates, c("coord_x", "coord_y", "observacao_data")]
)
duplicated_observation <- observacao[idx_has_coordinates, c("dataset_id", "observacao_id")][
  idx_duplicated_obs,
]
# Identify the records in 'camada' matching the dataset_id and observacao_id of the duplicated observations. Then, remove the duplicated records from the 'camada' table.
idx_duplicated_cam <- camada$dataset_id %in% duplicated_observation$dataset_id &
  camada$observacao_id %in% duplicated_observation$observacao_id
sum(idx_duplicated_cam)
camada <- camada[!idx_duplicated_cam, ]
nrow(camada)
# There are 1838 layers in the 'camada' table that pertain to duplicated observations. After removing these layers, there are 47 934 layers remaining in the 'camada' table.

# Plot the empirical distribution of the data, looking for inconsistencies in the data
png("res/fig/camada-matriz.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
# x11()
par(oma = c(0, 0, 1, 0), las = 1)
camada %>%
  dplyr::mutate(fragmentos = 1000 - terrafina) %>%
  dplyr::mutate(profund = mean(profund_inf, profund_sup)) %>%
  # dplyr::rename(profund = profund_inf) %>%
  # dplyr::select(
  #   'profund\n(cm)' = profund,
  #   'carbono\n(g/kg)' = carbono,
  #   'argila\n(g/kg)' = argila,
  #   'areia\n(g/kg)' = areia,
  #   'silte\n(g/kg)' = silte,
  #   'terrafina\n(g/kg)' = terrafina,
  #   'dsi\n(kg/dm³)' = dsi,
  #   'ctc\n(cmolc/kg)' = ctc,
  #   'ph\n(-)' = ph,
  #   'ce\n(mS/cm)' = ce) %>%
  dplyr::select(
    "Depth\n(cm)" = profund,
    "Carbon\n(g/kg)" = carbono,
    "Clay\n(g/kg)" = argila,
    "Sand\n(g/kg)" = areia,
    "Silt\n(g/kg)" = silte,
    "Coarse\n(g/kg)" = fragmentos,
    "BD\n(kg/dm³)" = dsi,
    "CEC\n(cmolc/kg)" = ctc,
    "pH\n(-)" = ph,
    "EC\n(mS/cm)" = ce
  ) %>%
  plot(
    cex = 0.5, col = "firebrick",
    # main = "Distribuição empírica dos dados",
    main = "Empirical data distribution",
    # sub = glue::glue("{Sys.Date()}-febr-camada")
  )
dev.off()

png("../res/fig/camada-200cm.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>% 
  dplyr::mutate(fragmentos = 1000 - terrafina) %>% 
  dplyr::rename(profund = profund_inf) %>%
  dplyr::filter(profund <= 200) %>% 
  dplyr::select(
    'profund\n(cm)' = profund,
    'carbono\n(g/kg)' = carbono,
    'argila\n(g/kg)' = argila,
    'areia\n(g/kg)' = areia,
    'silte\n(g/kg)' = silte,
    'terrafina\n(g/kg)' = terrafina,
    'dsi\n(kg/dm³)' = dsi,
    'ctc\n(cmolc/kg)' = ctc,
    'ph\n(-)' = ph,
    'ce\n(mS/cm)' = ce) %>%
  # dplyr::select(
  #   'Depth\n(cm)' = profund, 
  #   'Carbon\n(g/kg)' = carbono,
  #   'Clay\n(g/kg)' = argila, 
  #   'Sand\n(g/kg)' = areia, 
  #   'Silt\n(g/kg)' = silte, 
  #   'Coarse\n(g/kg)' = fragmentos, 
  #   'BD\n(kg/dm³)' = dsi, 
  #   'CEC\n(cmolc/kg)' = ctc,
  #   'pH\n(-)' = ph,
  #   'EC\n(mS/cm)' = ce) %>%
  plot(
    cex = 0.5, col = "firebrick1",
    main = "Distribuição empírica dos dados (< 200 cm)",
    # main = "Empirical data distribution (< 200 cm)",
    sub = glue::glue("{Sys.Date()}-camada"))
dev.off()

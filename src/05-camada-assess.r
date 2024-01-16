# title: "Brazilian Soil Dataset 2020"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

### Análise exploratória dos dados
camada <- read.table(glue::glue("../data/febr-camada.txt"),
  sep = ";", dec = ",",
  header = TRUE
)

# Verificar a distribuição empírica dos dados, procurando por inconsistências nos dados.
png("../res/fig/febr-camada.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>% 
  dplyr::mutate(fragmentos = 1000 - terrafina) %>% 
  dplyr::rename(profund = profund_inf) %>%
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
    main = "Distribuição empírica dos dados",
    # main = "Empirical data distribution",
    sub = glue::glue("{Sys.Date()}-febr-camada"))
dev.off()

png("../res/fig/febr-camada-200cm.png", width = 480 * 2, height = 480 * 2, res = 72 * 2)
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
    sub = glue::glue("{Sys.Date()}-febr-camada"))
dev.off()

# title: "Brazilian Soil Dataset 2019"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

# Set soil variables of interest
vars <- c(
  "carbono", "argila", "areia", "areiagrossa2", "areiafina2", "silte", "terrafina",
  "cascalho", "calhau", "dsi", "ctc", "ph", "ce"
)

# Read data from 'camada' table
camada <- febr::layer(
  dataset = "all",
  variable = glue::glue("{vars}_"),
  stack = TRUE,
  harmonization = list(harmonize = TRUE, level = 2),
  standardization = list(
    plus.sign = "add",
    plus.depth = 20,
    lessthan.sign = "remove",
    repetition = "combine",
    combine.fun = "mean",
    transition = "smooth",
    smoothing.fun = "mean",
    units = TRUE, round = TRUE
  )
)
# camada %>%
#   lapply(function (x) {
#     classe <-
#       x %>%
#       dplyr::select(dplyr::starts_with("terrafina")) %>%
#       unlist() %>%
#       class()
#     c(dataset = x$dataset_id[1], classe = classe)
#   }) %>%
#   do.call(rbind, .)

# Data processing
# As colunas dos dados de cada variável de solo prioritária que foram determinadas usando métodos parcialmente distintos são fundidas usando usando 'dplyr::coalesce()'. A ordem de prioridade de cada variável é a seguinte:
# 1. carbono: digestão úmida com cromo; método não especificado (geralmente digestão úmida como cromo); oxidação seca em forno de altíssima temperatura;
# 2. argila: dispersão com NaOH (determinação pelo método da pipeta ou do densímetro -- geralmente pipeta); método não especificado (geralmente dispersão com NaOH e determinação pelo método da pipeta ou do densímetro -- geralmente pipeta);
# 3. silte: dispersão com NaOH (determinação por diferença); método não especificado (geralmente dispersão com NaOH e determinação por diferença);
# 4. areia: dispersão com NaOH (determinação por peneiramento úmido); método não especificado (geralmente dispersão com NaOH e determinação por peneiramento úmido); soma das frações areia grossa e areia fina (com combinações como para a areia);
# 5. ctc: soma das bases trocáveis (Ca e Mg determinados por acetato de amônio ou KCl; K e Na determinados por HCl, HCl + H2SO4 ou acetato de amônio e quantificação por absorção atômica ou volumetria) e da acidez potencial (H + Al determinado por acetato de cálcio ou KCl e quantificação por volumetria); método não especificado;
# 6. dsi: determinação por cilindro; método não especificado (geralmente determinação por cilindro);
# 7. ph: determinação em água (razão solo:água variável e quantificação por potenciometria);
# 8. terrafina: determinação por peneiramento; método não especificado (geralmente determinação por peneiramento);
# 9. ce: determinação por pasta saturada;
# 10. cascalho: determinação por peneiramento; determinação visual (olho); método não especificado (geralmente determinação por peneiramento);
# 11. calhau: determinação por peneiramento; método não especificado (geralmente determinação por peneiramento).
camada %<>%
  dplyr::mutate(
    carbono = dplyr::coalesce(carbono_cromo, carbono_xxx, carbono_forno),
    argila = dplyr::coalesce(argila_naoh, argila_xxx),
    silte = dplyr::coalesce(silte_naoh, silte_xxx),
    areia = dplyr::coalesce(areia_naoh, areia_xxx),
    areiagrossa = dplyr::coalesce(areiagrossa2_naoh, areiagrossa2_xxx),
    areiafina = dplyr::coalesce(areiafina2_naoh, areiafina2_xxx),
    ctc = dplyr::coalesce(ctc_soma, ctc_xxx),
    dsi = dplyr::coalesce(dsi_cilindro, dsi_xxx),
    ph = dplyr::coalesce(ph_h2o),
    terrafina = dplyr::coalesce(terrafina_peneira, terrafina_xxx),
    ce = dplyr::coalesce(ce_pastasat),
    cascalho = dplyr::coalesce(cascalho_peneira, cascalho_olho, cascalho_xxx),
    calhau = dplyr::coalesce(calhau_peneira, calhau_xxx)
  ) %T>%
  print()

# Após a fusão das colunas com dados da mesma variável, faz-se ajustes nos dados das frações
# granulométricas. Primeiro, na falta de uma das duas frações granulométricas finas, 'argila' ou
# 'silte', calcula-se seu valor como sendo a diferença entre 1000 g/kg e a soma das duas outras
# frações granulométricas. Na falta dos dados do conteúdo de areia total ('areia'), primeiro se usa
# (quando disponível) a soma dos dados do conteúdo de areia grossa ('areiagrossa') e areia fina
# ('areiafina'). Caso não estejam disponíveis, então se usa a estratégia anterior. A seguir,
# verifica-se se a soma dos valores inteiros das três frações é igual a 1000 g/kg. Caso não seja
# (a diferença geralmente é de 1 g/kg para menos ou para mais), então altera-se os dados do conteúdo
# de silte total de maneira que a soma seja igual a 1000 g/kg. Finalmente, faz-se o processamento
# dos dados das frações granulométricas grossas calhau ('calhau'), cascalho ('cascalho') e terra
# fina ('terrafina'). Na ausência da última, utiliza-se as demais para sua estimativa. Caso todas as
# frações estejam faltando, então assume-se que o conteúdo da fração terra fina seja igual a
# 1000 g/kg. Isso porque é comum os trabalhos omitirem o conteúdo de terra fina quando a mesma
# corresponde a totalidade do solo.

correct_depth <-
  function(profund.sup, profund.inf) {
    res <- as.matrix(data.frame(profund.sup, profund.inf))
    if (any(profund.inf < profund.sup, na.rm = TRUE)) {
      id <- which(profund.inf < profund.sup)
      plus_depth <- max(res[id, 1])
      res[id,  1:2] <- abs(res[id,  1:2] - plus_depth)
      res[-id, 1:2] <- abs(res[-id, 1:2] + plus_depth)
    }
    as.data.frame(res)
  }
camada %<>%
  dplyr::mutate(
    argila_in = !is.na(argila),
    silte_in = !is.na(silte),
    areia_in = !is.na(areia),
    dtp_in = argila_in + silte_in + areia_in,
    argila = ifelse(dtp_in == 2 & !argila_in, 1000 - areia - silte, argila),
    silte = ifelse(dtp_in == 2 & !silte_in, 1000 - argila - areia, silte),
    areia = ifelse(!areia_in, areiagrossa + areiafina, areia),
    areia_in = !is.na(areia),
    areia = ifelse(dtp_in == 2 & !areia_in, 1000 - argila - silte, areia),
    dtp = areia + argila + silte,
    argila = round((argila / dtp) * 1000),
    silte = round((silte / dtp) * 1000),
    areia = round((areia / dtp) * 1000),
    dtp = areia + argila + silte,
    silte = ifelse(dtp != 1000, 1000 - areia - argila, silte),
    terrafina_in = !is.na(terrafina),
    cascalho_in = !is.na(cascalho),
    calhau_in = !is.na(calhau),
    terrafina = ifelse(!terrafina_in & cascalho_in & calhau_in, 1000 - cascalho - calhau, terrafina),
    terrafina = ifelse(!terrafina_in & cascalho_in & !calhau_in, 1000 - cascalho, terrafina),
    terrafina = ifelse(!terrafina_in & !cascalho_in & calhau_in, 1000 - calhau, terrafina),
    terrafina = ifelse(!terrafina_in & !cascalho_in & !calhau_in, 1000, terrafina) %>% round(),
    ph = ifelse(ph > 14, NA_real_, ph),
    carbono = round(carbono, 2),
    dsi = round(dsi, 2),
    profund_sup = ifelse(is.na(profund_inf), NA_real_, abs(profund_sup)),
    profund_inf = ifelse(is.na(profund_sup), NA_real_, abs(profund_inf))
  ) %>% 
  dplyr::group_by(dataset_id, observacao_id) %>%
  dplyr::mutate(
    sup = correct_depth(profund.sup = profund_sup, profund.inf = profund_inf)$profund.sup,
    inf = correct_depth(profund.sup = profund_sup, profund.inf = profund_inf)$profund.inf,
    profund_sup = sup,
    profund_inf = inf
  ) %>% 
  dplyr::ungroup()

# A última etapa de processamento lida com os dados da profundidade do solo, especificamente, com as
# camadas compostas por material orgânico. Em geral, o registro dessas camadas é realizado fixando o
# limite inferior como sendo igual a zero. Isso gera uma sequência invertida de valores de
# profundidade. Por exemplo, uma camada com profundidade de 3--0 cm representa uma camada orgânica
# de três centímetros de espessura acima das camadas de material mineral do solo. Em alguns casos a
# profundidade superior pode ser negativa, por exemplo, -3--0 cm. O processamento desses dados
# consiste em ajustar a profundidade superior do solo a profundidade superior da camada de material
# orgânico do solo, definida assim como sendo igual a zero centímetros de profundidade.

# Selecionar as colunas de interesse.
camada %<>%
  dplyr::select(
    dataset_id, observacao_id, camada_id, amostra_id, camada_nome, profund_sup, profund_inf,
    terrafina, argila, silte, areia, carbono, ctc, ph, dsi, ce
  ) %T>%
  print()

# Save data in TXT format
write.table(camada, file = "data/camada.txt", sep = ";", dec = ",", row.names = FALSE)

# title: "Brazilian Soil Dataset 2019"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2020-01-17"

# Summary
# This R script manages the 'layer' table extracted from datasets available in the Brazilian Soil
# Data Repository (FEBR). It selectively retrieves specific columns, including 'dataset_id,'
# 'observacao_id,' 'camada_id,' 'amostra_id,' 'camada_nome,' 'profund_sup,' 'profund_inf,'
# 'terrafina,' 'argila,' 'silte,' 'areia,' 'carbono,' 'ctc,' 'ph,' 'dsi,' and 'ce.' Additionally,
# it harmonizes the data of the continuous variables, 'terrafina,' 'argila,' 'silte,' 'areia,'
# 'carbono,' 'ctc,' 'ph,' 'dsi,' and 'ce,' and corrects the depth of the soil layers in the
# presence of organic layers at the top of the soil observation. The goal is to streamline and
# standardize the dataset for subsequent analysis.

# Set soil variables of interest
vars <- c(
  "carbono", "argila", "areia", "areiagrossa2", "areiafina2", "silte", "terrafina",
  "cascalho", "calhau", "dsi", "ctc", "ph", "ce"
)

# Read data from 'camada' table
# In the presence of the plus sign (+) alongside the lower limit of the bottom layer in a soil
# profile, add 20 cm to that layer.
# In the presence of the less-than sign (<), indicating that the value of a variable is below the
# lower limit of detection, simply remove the less-than sign, disregarding the limitation.
# When encountering repeated measurements of layers in an observation, combine the data from those
# repeated layers using the mean.
# In the presence of a wavy or irregular transition between subsequent layers in an observation,
# smooth them using the mean.
# The measurement unit(s) of the continuous variable(s) are converted to the standard measurement
# units and rounded to the standard number of decimal places.
# Data harmonization is performed at the second level, considering only the variable names and
# extraction/digestion/dispersion/separation method for harmonization.
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
# The columns for the data of each selected soil variable (carbon, clay, sand, coarse sand, fine
# sand, silt, fine earth, gravel, pebble, dsi, ctc, pH, and ce), determined using partially
# distinct methods, are merged using 'dplyr::coalesce()'. The priority order for each variable is
# as follows:
# 1. carbon: wet digestion with chromium; unspecified method (usually wet digestion like chromium);
# high-temperature dry oxidation;
# 2. clay: dispersion with NaOH (determination by the pipette or hydrometer method—usually
# pipette); unspecified method (usually dispersion with NaOH and determination by the pipette or
# hydrometer method—usually pipette);
# 3. silt: dispersion with NaOH (determination by difference); unspecified method (usually
# dispersion with NaOH and determination by difference);
# 4. sand: dispersion with NaOH (determination by wet sieving); unspecified method (usually
# dispersion with NaOH and determination by wet sieving); sum of the coarse and fine sand fractions
# (with combinations as for sand);
# 5. cation exchange capacity (ctc): sum of exchangeable bases (Ca and Mg determined by ammonium
# acetate or KCl; K and Na determined by HCl, HCl + H2SO4, or ammonium acetate and quantification
# by atomic absorption or volumetry) and potential acidity (H + Al determined by calcium acetate or
# KCl and quantified by volumetry); unspecified method;
# 6. dsi: determination by cylinder; unspecified method (usually determination by cylinder);
# 7. pH: determination in water (variable soil:water ratio and quantification by potentiometry);
# 8. fine earth: determination by sieving; unspecified method (usually determination by sieving);
# 9. electrical conductivity (ce): determination by saturated paste;
# 10. gravel: determination by sieving; visual determination (eye); unspecified method (usually
# determination by sieving);
# 11. pebble: determination by sieving; unspecified method (usually determination by sieving).
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

# After merging the columns with data for the same variable, adjustments are made to the data of
# the granulometric fractions. Firstly, in the absence of one of the two fine granulometric
# fractions, 'argila' or 'silte', its value is calculated as the difference between 1000 g/kg and
# the sum of the other two granulometric fractions. In the absence of total sand content ('areia')
# data, the sum of the data for coarse sand content ('areiagrossa') and fine sand content
# ('areiafina') is used (if available). If not available, the previous strategy is employed. Next,
# it is verified whether the sum of the integer values of the three fractions is equal to 1000
# g/kg. If not (usually a difference of 1 g/kg more or less), then the data for total silt content
# is adjusted so that the sum equals 1000 g/kg. Finally, the data for the coarse granulometric 
# fractions, 'pebble' ('calhau'), gravel ('cascalho'), and fine earth ('terrafina'), are processed.
# In the absence of the latter, the former two are used for estimation. If all fractions are
# missing, it is assumed that the content of the fine earth fraction is equal to 1000 g/kg. This is
# because it is common for studies to omit the fine earth content when it represents the entirety
# of the soil.

# Correct the depth of the soil layers in the presence of organic layers at the top of the soil
# observation
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

# The final processing stage deals with soil depth data, specifically the layers composed of
# organic material. Generally, recording for these layers is done by setting the lower limit as
# equal to zero. This results in a reversed sequence of depth values. For instance, a layer with a
# depth of 3–0 cm represents an organic layer three centimeters thick above the mineral soil
# layers. In some cases, the upper depth may be negative, for example, -3–0 cm. The processing of
# these data involves adjusting the upper soil depth to the upper depth of the organic soil layer,
# defined as zero centimeters depth.

# Select and order columns
camada %<>%
  dplyr::select(
    dataset_id, observacao_id, camada_id, amostra_id, camada_nome, profund_sup, profund_inf,
    terrafina, argila, silte, areia, carbono, ctc, ph, dsi, ce
  ) %T>%
  print()

# Save data in TXT format
write.table(camada, file = "data/camada.txt", sep = ";", dec = ",", row.names = FALSE)

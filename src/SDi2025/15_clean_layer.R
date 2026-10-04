# title: SoilData Integration
# subtitle: Clean layers
# author: Alessandro Rosa
# date: 2026
# licence: MIT
# description: This script cleans individual soil layers in the dataset-cleaned
# integrated Brazilian Soil Dataset produced by script 14. It standardizes layer
# names and depth intervals, removes repeated or invalid layers, normalizes soil
# property values, applies known layer corrections, plots the spatial
# distribution, and exports the layer-cleaned dataset to data/15_soildata.txt.
rm(list = ls())

# Source helper functions
source("src/SDi2025/00_helper_functions.R")

# Load required packages
library(data.table)
library(sf)
library(geobr)

# Read Brazilian state boundaries
brazil <- read_brazil_states()

# Read the dataset-cleaned SoilData from the previous script
soildata <- data.table::fread("data/14_soildata.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 63146
# Events: 20251
# Georeference: 16710 (yes) / 3541 (no)
# Date: 20094 (yes) / 157 (no)
# Datasets: 271

# Order layers by event (id) and layer depth (profund_sup and profund_inf)
soildata <- soildata[order(id, profund_sup, profund_inf)]

# Correct a few layer names
# Here we only correct a few known cases. These corrections need to be done in 
# the source data in the future. Further corrections are performed below.
soildata[camada_nome == "", camada_nome := NA_character_]
soildata[id == "ctb0770-100" & camada_nome == "B21H", camada_nome := "B21h"]
soildata[id == "ctb0636-Perfil-03" & camada_nome == "Ao", camada_nome := "A1"]

# Nonexistent layers ###########################################################

# ctb0809-Exame-8. This layer was erroneously entered in the source spreadsheet.
# We remove it. This was already corrected in the source spreadsheet.
soildata <- soildata[!(id == "ctb0809-Exame-8" & profund_sup == profund_inf)]
# There is a second nonexistent layer in ctb0809-Exame-8: profund_sup == 50 and
# profund_inf == 80. This layer was erroneously entered in the source
# spreadsheet. We remove it. This was already corrected in the source
# spreadsheet.
soildata <- soildata[
  !(id == "ctb0809-Exame-8" & profund_sup == 50 & profund_inf == 80)
]

# Duplicated layers ############################################################

# Some layers are repeated in the same event (id). These layers have equal
# values for camada_nome, profund_sup, profund_inf, carbono, and argila.

# Sort each event (id) by layer depth (profund_sup and profund_inf)
# Update the columns camada_id
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# We create a new variable called repeated to identify these layers. Then we 
# create a new variable called any_repeated to identify events (id) with 
# repeated layers.
soildata[
  ,
  repeated := duplicated(camada_nome) & duplicated(profund_sup) & duplicated(profund_inf) & duplicated(carbono) & duplicated(argila),
  by = id
]
nrow(soildata[repeated == TRUE, ])
# 570 layers
soildata[, any_repeated := any(repeated == TRUE), by = id]
if (FALSE) {
  View(soildata[
    any_repeated == TRUE,
    .(id, camada_nome, profund_sup, profund_inf, carbono, argila, repeated)
  ])
}
# Filter out layers with repeated == TRUE. These layers need to be checked in 
# the source data in the future.
soildata <- soildata[repeated == FALSE, ]
soildata[, repeated := NULL]
soildata[, any_repeated := NULL]
summary_soildata(soildata)
# Layers: 62574
# Events: 20251
# Georeference: 16710 (yes) / 3541 (no)
# Date: 20094 (yes) / 157 (no)
# Datasets: 271

# Update layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Wrong depth limits ###########################################################

# ctb0014-Perfil_5. Bw3: 114 -> 144 (not corrected in the source)
soildata[
  id == "ctb0014-Perfil_5" & camada_nome == "Bw3",
  profund_inf := ifelse(profund_inf == 114, 144, profund_inf)
]
# ctb0594-COMPLEMENTAR-92
# If profund_sup == 0 and profund_inf == 20, set observacao_id ==
# "COMPLEMENTAR-92-Neo" and id == "ctb0594-COMPLEMENTAR-92-Neo". Else, set
# observacao_id == "COMPLEMENTAR-92-Cambi" and id ==
# "ctb0594-COMPLEMENTAR-92-Cambi". This was already corrected in the source.
soildata[
  id == "ctb0594-COMPLEMENTAR-92" & profund_sup == 0 & profund_inf == 20,
  `:=`(
    observacao_id = "COMPLEMENTAR-92-Neo",
    id = "ctb0594-COMPLEMENTAR-92-Neo"
  )
]
soildata[
  id == "ctb0594-COMPLEMENTAR-92",
  `:=`(
    observacao_id = "COMPLEMENTAR-92-Cambi",
    id = "ctb0594-COMPLEMENTAR-92-Cambi"
  )
]
# ctb0599-AC-13: if camada_nome == 3ªCAM, profund_sup == 50 and profund_inf ==
# 70. This was already corrected in the source spreadsheet.
soildata[
  id == "ctb0599-AC-13" & camada_nome == "3ªCAM", `:=`(
    profund_sup = 50,
    profund_inf = 70
  )
]
# ctb0605-P-10. When camada_nome == "BC", set profund_sup == 40 and 
# profund_inf == 64
soildata[
  id == "ctb0605-P-10" & camada_nome == "BC", `:=`(
    profund_sup = 40,
    profund_inf = 64
  )
]
# ctb0667-A-E-41. The source spreadsheet had three additional layers that
# possibly are from another soil profile. We drop them here. This has also been
# corrected in the source spreadsheet.
# Drop camada_nome == "B1t" and "IIB2tp1".
soildata <- soildata[
  !(id == "ctb0667-A-E-41" & camada_nome %in% c("B1t", "IIB2tp1"))
]
# Drop camada_nome == A & amostra_id == 25377
soildata <- soildata[
  !(id == "ctb0667-A-E-41" & camada_nome == "A" & amostra_id == 25377)
]
# ctb0673-11. When camada_nome == A12, set profund_sup == 8 and 
# profund_inf == 16. This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0673-11" & camada_nome == "A12", `:=`(
    profund_sup = 8,
    profund_inf = 16
  )
]
# ctb0673-2-EXTRA. When camada_nome == "B1", set profund_sup == 30 and
# profund_inf == 50. This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0673-2-EXTRA" & camada_nome == "B1", `:=`(
    profund_sup = 30,
    profund_inf = 50
  )
]
# ctb0678-46. When camada_nome == "A3", set profund_sup == 10 and
# profund_inf == 28. This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0678-46" & camada_nome == "A3", `:=`(
    profund_sup = 10,
    profund_inf = 28
  )
]
# ctb0683-1. When camada_nome == A or C, set observacao_id == 1-extra
# and id == ctb0683-1-extra. This has also been corrected in the source
# spreadsheet.
soildata[
  id == "ctb0683-1" & camada_nome %in% c("A", "C"), `:=`(
    observacao_id = "1-extra",
    id = "ctb0683-1-extra"
  )
]
# ctb0683-15. When camada_nome == "A" & profund_inf == 35, set observacao_id ==
# "15-extra" and id == "ctb0683-15-extra". This has also been corrected in the
# source spreadsheet.
soildata[
  id == "ctb0683-15" & camada_nome == "A" & profund_inf == 35, `:=`(
    observacao_id = "15-extra",
    id = "ctb0683-15-extra"
  )
]
# When camada_nome == "Bt", set observacao_id == "15-extra" and id == 
# "ctb0683-15-extra". This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0683-15" & camada_nome == "Bt", `:=`(
    observacao_id = "15-extra",
    id = "ctb0683-15-extra"
  )
]
# ctb0683-3. When camada_nome == "A" or "C", set observacao_id == "3-extra" and
#  id == "ctb0683-3-extra". This has also been corrected in the source
# spreadsheet.
soildata[
  id == "ctb0683-3" & camada_nome %in% c("A", "C"), `:=`(
    observacao_id = "3-extra",
    id = "ctb0683-3-extra"
  )
]
# ctb0683-3. When camada_nome == "A", set observacao_id == "3-extra" and id == 
# "ctb0683-3-extra". This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0683-3" & camada_nome == "A", `:=`(
    observacao_id = "3-extra",
    id = "ctb0683-3-extra"
  )
]
# ctb0683-3. When profund = 20	40, set observacao_id == "3-extra" and id ==
# "ctb0683-3-extra". This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0683-3" & profund_sup == 20 & profund_inf == 40, `:=`(
    observacao_id = "3-extra",
    id = "ctb0683-3-extra"
  )
]
# ctb0683-3. When profund = 60	80, set observacao_id == "3-extra" and id == 
# "ctb0683-3-extra". This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0683-3" & profund_sup == 60 & profund_inf == 80, `:=`(
    observacao_id = "3-extra",
    id = "ctb0683-3-extra"
  )
]
# ctb0683-41. When camada_nome == A1, A2, or Bt, set observacao_id == "41-extra"
# and id == "ctb0683-41-extra". This has also been corrected in the source 
# spreadsheet.
soildata[
  id == "ctb0683-41" & camada_nome %in% c("A1", "A2", "Bt"), `:=`(
    observacao_id = "41-extra",
    id = "ctb0683-41-extra"
  )
]
# ctb0683-5. When profund = 0	20, 20	40, and 60	80, set observacao_id ==
# "5-extra" and id == "ctb0683-5-extra". This has also been corrected in the
# source spreadsheet.
soildata[
  id == "ctb0683-5" & profund_sup %in% c(0, 20, 60) & profund_inf %in% c(20, 40, 80), `:=`(
    observacao_id = "5-extra",
    id = "ctb0683-5-extra"
  )
]
# ctb0686-RL-37-EXTRA. Drop layers where amostra_id == 30186 and 30187.
soildata <- soildata[!(id == "ctb0686-RL-37" & amostra_id %in% c(30186, 30187))]
# ctb0686-RL-9. When amostra_id == 30090 and 30091, set observacao_id ==
# "9-extra" and id == "ctb0686-RL-9-extra". This has also been corrected in the
# source spreadsheet.
soildata[
  id == "ctb0686-RL-9" & amostra_id %in% c(30090, 30091), `:=`(
    observacao_id = "9-extra",
    id = "ctb0686-RL-9-extra"
  )
]
# ctb0691-10. Drop layer where camada_nome == O1. Rename camada_name O2 to
# "O1 e O2" and set profund = -3-0. This has also been corrected in the source
# spreadsheet.
soildata <- soildata[!(id == "ctb0691-10" & camada_nome == "O1")]
soildata[
  id == "ctb0691-10" & camada_nome == "O2", `:=`(
    camada_nome = "O1 e O2",
    profund_sup = -3,
    profund_inf = 0
  )
]
# ctb0753-101. When camada_nome == "O2", set profund_sup == -5 and profund_inf
# == 0. This has also been corrected in the source spreadsheet.
soildata[
  id == "ctb0753-101" & camada_nome == "O2", `:=`(
    profund_sup = -5,
    profund_inf = 0
  )
]
# ctb0788-12-EXTRA. We do not have access to the source document. The data in
# the source spreadsheet is inconsistent. We drop this observation.
soildata <- soildata[!(id == "ctb0788-12-EXTRA")]
# ctb0811-8. The source spreadsheet contains eight layers, but the source
# document contains only five. The source document does not contain all soil
# profiles and layers. It appears that these layers are from another soil
# profile. When amostra_id == 44258, 44259, and 44260, set observacao_id ==
# "8-extra" and id == "ctb0811-8-extra". This has also been corrected in the
# source spreadsheet.
soildata[
  id == "ctb0811-8" & amostra_id %in% c(44258, 44259, 44260), `:=`(
    observacao_id = "8-extra",
    id = "ctb0811-8-extra"
  )
]
# ctb0811-80. The source document does not contain data for this observation.
# The source spreadsheet contains layers Azn and Ezn sharing the same depth
# interval (0-25). Layer Ezn contains data only for morphological description,
# but no data for soil properties. The third layer (2Btzn) extends from 25 to 50
# cm. We suspect that the Azn horizon extends from 0 to only 15 cm, and the Ezn
# horizon extends from 15 to 25 cm. A ~10-cm thick E horizon is usual in the 
# region as reported in the source spreadsheet for other soil profiles. This
# was already corrected in the source spreadsheet. We set profund_inf of Azn to 
# 15 and profund_sup of Ezn to 15.
soildata[
  id == "ctb0811-80" & camada_nome == "Azn", profund_inf := 15
]
soildata[
  id == "ctb0811-80" & camada_nome == "Ezn", profund_sup := 15
]
# ctb0815-E13. The source document is incomplete and does not contain data for
# all observations in the source spreadsheet. However, there apears to be an
# extra layer in the source spreadsheet. We drop the row with amostra_id ==
# 44693.
soildata <- soildata[!(id == "ctb0815-E13" & amostra_id == 44693)]
# ctb0815-E14. After analysis of the source document, we conclude that this
# observation contains an extra layer from E15. When amostra_id == 44706, set
# observacao_id == "E15" and id == "ctb0815-E15". This has also been corrected 
# in the source spreadsheet.
soildata[
  id == "ctb0815-E14" & amostra_id == 44706, `:=`(
    observacao_id = "E15",
    id = "ctb0815-E15"
  )
]
# ctb0815-E14. Data attributed to E14 in the source spreadsheet consists of six
# layers, which seems a lot for an extra sample. Besides, the data reflects a
# Latossolo, while the source document reports that E14 is a Areia Quartzoza.
# Here we will drop this observation entirely.
soildata <- soildata[!(id == "ctb0815-E14")]
# ctb0815-E16. The source spreadsheet reports a layer A	from 0-40. This layer is
# not present in the source document. We drop this layer: amostra_id == 44712.
soildata <- soildata[!(id == "ctb0815-E16" & amostra_id == 44712)]
# ctb0815-E17. These observation (Terra Roxa Estruturada) has seven layers in 
# the source spreadsheet, but the source document reports only three. The other
# layers seem to be from a Latossolo. Because the source document is incomplete,
# we will drop the four layers that are not present in the source document: 
# amostra_id == 44715, 44717, 44719, 44721.
soildata <- soildata[
  !(id == "ctb0815-E17" & amostra_id %in% c(44715, 44717, 44719, 44721))
]
# ctb0815-E18. This observation has six layers in the source spreadsheet, but 
# the source document reports only two. Because the source document is 
# incomplete, we will drop the four layers that are not present in the source 
# document: amostra_id == 44722, 44724, 44725, 44726.
soildata <- soildata[
  !(id == "ctb0815-E18" & amostra_id %in% c(44722, 44724, 44725, 44726))
]
# ctb0815-E19. This observation has six layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the four layers that are not present in the source
# document: amostra_id == 44728, 44730, 44731, 44733.
soildata <- soildata[
  !(id == "ctb0815-E19" & amostra_id %in% c(44728, 44730, 44731, 44733))
]
# ctb0815-E35. This observation has five layers in the source spreadsheet, but 
# the source document reports only two. Because the source document is 
# incomplete, we will drop the three layers that are not present in the source 
# document: amostra_id == 44781, 44783, 44783.
soildata <- soildata[
  !(id == "ctb0815-E35" & amostra_id %in% c(44781, 44783, 44785))
]
# ctb0815-E36. This observation has five layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the three layers that are not present in the source
# document: amostra_id == 44786, 44786, 44790.
soildata <- soildata[
  !(id == "ctb0815-E36" & amostra_id %in% c(44786, 44788, 44790))
]
# ctb0815-E38. This observation has four layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 44794 and 44796.
soildata <- soildata[
  !(id == "ctb0815-E38" & amostra_id %in% c(44794, 44796))
]
# ctb0815-E39. This observation has four layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 45248 and 45250.
soildata <- soildata[
  !(id == "ctb0815-E39" & amostra_id %in% c(45248, 45250))
]
# ctb0815-E40. This observation has four layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 45252 and 45254.
soildata <- soildata[
  !(id == "ctb0815-E40" & amostra_id %in% c(45252, 45254))
]
# ctb0815-E41. This observation has four layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 45256 and 45258.
soildata <- soildata[
  !(id == "ctb0815-E41" & amostra_id %in% c(45256, 45258))
]
# ctb0815-E43. This observation has four layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 45262 and 45265.
soildata <- soildata[
  !(id == "ctb0815-E43" & amostra_id %in% c(45262, 45265))
]
# ctb0815-E45. This observation has four layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 45505 and 45507.
soildata <- soildata[
  !(id == "ctb0815-E45" & amostra_id %in% c(45505, 45507))
]
# ctb0815-E46. This observation has three layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the layer that is not present in the source document:
# amostra_id == 45509.
soildata <- soildata[
  !(id == "ctb0815-E46" & amostra_id == 45509)
]
# ctb0815-E70. This observation has five layers in the source spreadsheet, but 
# the source document reports only two. Because the source document is 
# incomplete, we will drop the three layers that are not present in the source
# document: amostra_id == 45522, 45524, 45525.
soildata <- soildata[
  !(id == "ctb0815-E70" & amostra_id %in% c(45522, 45524, 45525))
]
# ctb0815-E71. This observation has four layers in the source spreadsheet, but 
# the source document reports only two. Because the source document is
# incomplete, we will drop the two layers that are not present in the source
# document: amostra_id == 45585 and 45586.
soildata <- soildata[
  !(id == "ctb0815-E71" & amostra_id %in% c(45585, 45586))
]
# ctb0815-E72. This observation has three layers in the source spreadsheet, but
# the source document reports only two. Because the source document is
# incomplete, we will drop the layer that is not present in the source document:
# amostra_id == 45588.
soildata <- soildata[
  !(id == "ctb0815-E72" & amostra_id == 45588)
]
# ctb0815-E68. Drop amostra_id 45579.
soildata <- soildata[
  !(id == "ctb0815-E68" & amostra_id == 45579)
]
# ctb0815-E67. Drop amostra_id 45576.
soildata <- soildata[
  !(id == "ctb0815-E67" & amostra_id == 45576)
]
# ctb0815-E64. Drop amostra_id 45570.
soildata <- soildata[
  !(id == "ctb0815-E64" & amostra_id == 45570)
]
# ctb0815-E63. Drop amostra_id 45566 and 45567.
soildata <- soildata[
  !(id == "ctb0815-E63" & amostra_id %in% c(45566, 45567))
]
# ctb0815-E62. Drop amostra_id 45563.
soildata <- soildata[
  !(id == "ctb0815-E62" & amostra_id == 45563)
]
# ctb0815-E61. Drop amostra_id 45560.
soildata <- soildata[
  !(id == "ctb0815-E61" & amostra_id == 45560)
]
# ctb0815-E60. Drop amostra_id 45556 and 45557.
soildata <- soildata[
  !(id == "ctb0815-E60" & amostra_id %in% c(45556, 45557))
]
# ctb0815-E59. Drop amostra_id 45553.
soildata <- soildata[
  !(id == "ctb0815-E59" & amostra_id == 45553)
]
# ctb0815-E57. Drop amostra_id 45549 and 45550. Also drop amostra_id 45551 as
# it has erroneous data.
soildata <- soildata[
  !(id == "ctb0815-E57" & amostra_id %in% c(45549, 45550, 45551))
]
# ctb0815-E56. Drop amostra_id 45547.
soildata <- soildata[
  !(id == "ctb0815-E56" & amostra_id == 45547)
]
# ctb0815-E55. Drop amostra_id 45543.
soildata <- soildata[
  !(id == "ctb0815-E55" & amostra_id == 45543)
]
# ctb0815-E54. Drop amostra_id 45540.
soildata <- soildata[
  !(id == "ctb0815-E54" & amostra_id == 45540)
]
# ctb0815-E53. Drop amostra_id 45537.
soildata <- soildata[
  !(id == "ctb0815-E53" & amostra_id == 45537)
]
# ctb0815-E52. Drop amostra_id 45535.
soildata <- soildata[
  !(id == "ctb0815-E52" & amostra_id == 45535)
]
# ctb0815-E50. Drop amostra_id 45528 and 45529.
soildata <- soildata[
  !(id == "ctb0815-E50" & amostra_id %in% c(45528, 45529))
]
# ctb0815-E49. Drop amostra_id 45520.
soildata <- soildata[
  !(id == "ctb0815-E49" & amostra_id == 45520)
]
# ctb0815-E48. Drop amostra_id 45515 and 45517.
soildata <- soildata[
  !(id == "ctb0815-E48" & amostra_id %in% c(45515, 45517))
]
# ctb0815-E44. Drop amostra_id 45504.
soildata <- soildata[
  !(id == "ctb0815-E44" & amostra_id == 45504)
]
# ctb0815-E37. Drop amostra_id 44792.
soildata <- soildata[
  !(id == "ctb0815-E37" & amostra_id == 44792)
]
# ctb0815-E30. Drop amostra_id 44771.
soildata <- soildata[
  !(id == "ctb0815-E30" & amostra_id == 44771)
]
# ctb0815-7. Drop amostra_id 44665. 
soildata <- soildata[
  !(id == "ctb0815-7" & amostra_id == 44665)
]
# ctb0820-E-16. When amostra_id == 44991, set depths 18-40. We do not have
# access to the source document, but we suspect that the depth limits were
# entered incorrectly in the source spreadsheet. This was already corrected in
# the source spreadsheet.
soildata[
  id == "ctb0820-E-16" & amostra_id == 44991, `:=`(
    profund_sup = 18,
    profund_inf = 40
  )
]
# When amostra_id == 44993, set profund_inf 90.
soildata[
  id == "ctb0820-E-16" & amostra_id == 44993, profund_inf := 90
]
# Also, when amostra_id 44992, set profund_inf == 110.
soildata[
  id == "ctb0820-E-16" & amostra_id == 44992, profund_inf := 110
]
# ctb0821-P21. The source spreadsheet contains six incomplete layers, but the 
# source document contains only one layer. We drop the five layers that are not
# present in the source document: 45281 45280 45282 45283 45284.
soildata <- soildata[
  !(id == "ctb0821-P21" & amostra_id %in% c(45281, 45280, 45282, 45283, 45284))
]
# ctb0821-P24. The source spreadsheet contains five incomplete layers, but the 
# source document contains only two layers. We drop the three layers that are not present in the source document: 45290 45292 45293.
soildata <- soildata[
  !(id == "ctb0821-P24" & amostra_id %in% c(45290, 45292, 45293))
]
# ctb0821-P25. The source spreadsheet contains six incomplete layers, but the 
# source document contains only two layers. We drop the four layers that are not
# present in the source document: 45295 45296 45297 45299.
soildata <- soildata[
  !(id == "ctb0821-P25" & amostra_id %in% c(45295, 45296, 45297, 45299))
]
# ctb0821-P27. The source spreadsheet contains six incomplete layers, but the 
# source document contains only two layers. We drop the four layers that are not 
# present in the source document: 45304 45306 45308 45309.
soildata <- soildata[
  !(id == "ctb0821-P27" & amostra_id %in% c(45304, 45306, 45308, 45309))
]
# ctb0821-P28. The source spreadsheet contains four incomplete layers, but the 
# source document contains only two layers. We drop the two layers that are not
# present in the source document: 45311 45313.
soildata <- soildata[
  !(id == "ctb0821-P28" & amostra_id %in% c(45311, 45313))
]
# ctb0821-P29. The source spreadsheet contains four incomplete layers, but the
# source document contains only two layers. We drop the two layers that are not
# present in the source document: 45315 45316.
soildata <- soildata[
  !(id == "ctb0821-P29" & amostra_id %in% c(45315, 45316))
]
# ctb0821-P30. The source spreadsheet contains five incomplete layers, but the 
# source document contains only two layers. We drop the three layers that are 
# not present in the source document: 45482 45483 45485.
soildata <- soildata[
  !(id == "ctb0821-P30" & amostra_id %in% c(45482, 45483, 45485))
]
# ctb0821-P31. The source spreadsheet contains five incomplete layers, but the
# source document contains only one layer. We drop the four layers that are not 
# present in the source document: 45320 45321 45323 45324.
soildata <- soildata[
  !(id == "ctb0821-P31" & amostra_id %in% c(45320, 45321, 45323, 45324))
]
# ctb0821-P31. This observation has no layers. We drop this observation entirely.
soildata <- soildata[!(id == "ctb0821-P31")]
# ctb0821-P33. The source spreadsheet contains five incomplete layers, but the 
# source document contains only three layers. We drop the two layers that are 
# not present in the source document: 45330 45333.
soildata <- soildata[
  !(id == "ctb0821-P33" & amostra_id %in% c(45330, 45333))
]
# ctb0821-P34. The source spreadsheet contains six incomplete layers, but the 
# source document contains only one layer. We drop the five layers that are not 
# present in the source document: 45335 45336 45337 45338 45339.
soildata <- soildata[
  !(id == "ctb0821-P34" & amostra_id %in% c(45335, 45336, 45337, 45338, 45339))
]
# ctb0821-P35. The source spreadsheet contains seven incomplete layers, but the 
# source document contains only two layers. We drop the five layers that are not present in the source document: 45342 45343 45344 45346 45345.
soildata <- soildata[
  !(id == "ctb0821-P35" & amostra_id %in% c(45342, 45343, 45344, 45346, 45345))
]






















































# profund_sup > profund_inf ####################################################

# Check layers with incorrect depth limits (profund_sup > profund_inf). These
# layers need to be corrected manually. We print the layers with incorrect depth
# limits and then correct them. Here we simply reverse the depth limits. The
# corrections need to be checked in the source data in the future.
nrow(soildata[profund_sup > profund_inf])
# 0 layers with incorrect depth limits
# The following layers were already corrected in a previous script:
# soildata[id == "ctb0033-RO1154" & profund_sup == 80, `:=` (
#   profund_sup = 70,
#   profund_inf = 80
# )]
# soildata[id == "ctb0033-RO2463" & profund_sup == 80, `:=` (
#   profund_sup = 70,
#   profund_inf = 80
# )]
# soildata[id == "ctb0033-RO2826" & profund_sup == 140, `:=` (
#   profund_sup = 140,
#   profund_inf = 160
# )]
# soildata[id == "ctb0033-RO3542" & profund_sup == 110, `:=`(
#   profund_sup = 110,
#   profund_inf = 120
# )]

# Negative depth ##############################################################

# WE WILL KEEEP NEGATIVE DEPTHS TO IDENTIFY LITTER LAYERS
# # Correct negative (profund_sup < 0) depth limit of topsoil layers
# # Check each soil profile (id) for negative depth limits. Store the result in a
# # new column "negative_depth" (TRUE/FALSE). If a profile has negative depth
# # limits, add the absolute value of the negative depth limit to the depth limits
# # (profund_sup and profund_inf) of all layers of that profile. This means that
# # we standardize the topsoil layer to start at 0 cm depth. We still need to
# # think about the best way to handle negative depth limits (organic layers).
# negative_depths <- soildata[, .(min_depth = min(profund_sup)), by = id][min_depth < 0]
# print(negative_depths)
# if (nrow(negative_depths) > 0) {
#   soildata[negative_depths, on = "id", `:=` (
#     profund_sup = profund_sup + abs(i.min_depth),
#     profund_inf = profund_inf + abs(i.min_depth)
#     )
#   ]
# }
# rm(negative_depths)

# ctb0671-13-ATM
soildata[id == "ctb0671-13-ATM" & camada_nome == "O2", `:=`(
  profund_sup = -2,
  profund_inf = 0
)]
# ctb0678-88. When camada_nome == "O1", set profund_sup == -2 and
# profund_inf == 0. This was not changed in the source spreadsheet, but we keep
# it here to identify litter layers.
soildata[
  id == "ctb0678-88" & camada_nome == "O1", `:=`(
    profund_sup = -2,
    profund_inf = 0
  )
]

# profund_sup == profund_inf ###################################################

# Some layers have equal values for profund_sup and profund_inf. This may occur
# when the soil profile sampling and description ended at the top of the layer,
# producing a censoring effect. 
# It can also occurr due to errros in the 
# source data. We will check these cases and correct them manually.
nrow(soildata[profund_sup == profund_inf])
# 200 layers
soildata[, equal_depth := any(profund_sup == profund_inf), by = id]
if (FALSE) {
  View(soildata[
    equal_depth == TRUE,
    .(id, camada_nome, profund_sup, profund_inf, carbono)
  ])
}
# Manual correction for a few cases:
# ctb0606-Perfil-02
# This has already been corrected in the source spreadsheet.
soildata[id == "ctb0606-Perfil-02" & camada_nome == "CR3", `:=`(
  profund_sup = 120,
  profund_inf = 150
)]
soildata[id == "ctb0606-Perfil-02" & camada_nome == "CR", `:=`(
  profund_sup = 150,
  profund_inf = 170
)]
# ctb0821-P43
# For the C layer, depths are profund_sup == 130 and profund_inf == 130+. We set
# profund_inf of the C layer to 150.
soildata[id == "ctb0821-P43" & camada_nome == "C", profund_inf := 150]
# ctb0775-9. For the B21 layer should be profund_sup == 100. This was already
#  corrected in the source spreadsheet.
soildata[id == "ctb0775-9" & camada_nome == "B21", profund_sup := 100]
# profund_sup == profund_inf == 0
# Some profiles have both profund_sup and profund_inf equal to zero and/or NA
# for all layers. Identify these cases.
soildata[
  equal_depth == TRUE & profund_sup == 0 & profund_inf == 0,
  .(id, camada_nome, profund_sup, profund_inf)
]
# ctb0617-Extra-28: Depths were not recorded in the source document, but 
# erroneously recorded as 0 in the source spreadsheet. This was corrected in the
# source spreadsheet. We set the depth limits to NA. This was already corrected 
# in the source spreadsheet.
soildata[id == "ctb0617-Extra-28", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0627-Pinheiro-Pr-25. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits 
# to NA.
soildata[id == "ctb0627-Pinheiro-Pr-25", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-130. Depths were not recorded in the source document, but 
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits 
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-130", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-30. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-30", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-31. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-31", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-32. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-32", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-68. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-68", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-72. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-72", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-73. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-73", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-80. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-80", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-81. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-81", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-82. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-82", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-83. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-83", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0631-PC-84. Depths were not recorded in the source document, but
# erroneously recorded as 0 in the source spreadsheet. We set the depth limits
# to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0631-PC-84", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0642-Perfil-3. profund_inf of A1 should be 10 cm. This was already 
# corrected in the source spreadsheet.
soildata[id == "ctb0642-Perfil-3" & camada_nome == "A1", profund_inf := 10]
# ctb0645-Perfil-2. profund_inf of A1 should be 60 cm. This was already 
# corrected in the source spreadsheet.
soildata[id == "ctb0645-Perfil-2" & camada_nome == "A1", profund_inf := 60]
# ctb0809-Exame-20. profund_sup of B should be 40 and profund_inf of B should be
# 40+, thus we set profund_inf of B to 60. This was already corrected in the
# source spreadsheet.
soildata[id == "ctb0809-Exame-20" & camada_nome == "B", profund_sup := 40]
soildata[id == "ctb0809-Exame-20" & camada_nome == "B", profund_inf := 60]
# ctb0810-Exame-4. Depths were not recorded in the source document for layer A, 
# but erroneously recorded as 0 in the source spreadsheet. We set the depth 
# limits to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0810-Exame-4" & camada_nome == "A", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0810-Exame-6. Depths were not recorded in the source document for both 
# layers, but erroneously recorded as 0 in the source spreadsheet. We set the 
# depth limits to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0810-Exame-6", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0821-P35. Depths were not recorded in the source document for both layers,
# but erroneously recorded as 0 in the source spreadsheet. We set the depth 
# limits to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0821-P35", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0821-P39. Depths were not recorded in the source document for both layers, 
# but erroneously recorded as 0 in the source spreadsheet. We set the depth 
# limits to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0821-P39", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]
# ctb0821-P43. Depths were not recorded in the source document for the A layer,
# but erroneously recorded as 0 in the source spreadsheet. We set the depth
# limits to NA. This was already corrected in the source spreadsheet.
soildata[id == "ctb0821-P43" & camada_nome == "A", `:=`(
  profund_sup = NA_real_,
  profund_inf = NA_real_
)]

# Lowermost layer?
# Check if the layer with profund_sup == profund_inf is the lowermost layer of
# the profile. If so, we add a fixed depth (plus_depth) to the lowermost layer.
plus_depth <- 20
soildata[, max_profund_inf := if (all(is.na(profund_inf))) {
  NA_real_
} else {
  max(profund_inf, na.rm = TRUE)
}, by = id]
soildata[
  profund_sup == profund_inf & profund_inf == max_profund_inf,
  profund_inf := profund_inf + plus_depth
]
soildata[, max_profund_inf := NULL]
nrow(soildata[profund_sup == profund_inf])
# 0 layers
summary_soildata(soildata)
# Layers: 62574
# Events: 20251
# Georeference: 16710 (yes) / 3541 (no)
# Date: 20094 (yes) / 157 (no)
# Datasets: 271

# Layers from two different profiles ###########################################
# Some profiles (events) have layers from two different profiles. These layers 
# have the same id, but different values for profund_sup and profund_inf. For 
# example:
# soildata[id == "ctb0717-38", .(id, camada_nome, profund_sup, profund_inf)]
#            id camada_nome profund_sup profund_inf
#        <char>      <char>       <num>       <num>
# 1: ctb0717-38         A11           0          12
# 2: ctb0717-38          A1           0          40
# 3: ctb0717-38         A12          12          35
# 4: ctb0717-38         A21          35         100
# 5: ctb0717-38         A12          40          55
# 6: ctb0717-38        C1ca          55          70
# 7: ctb0717-38        C2ca          70          85
# 8: ctb0717-38         A22         100         190
# 9: ctb0717-38          Bh         190         210
# We identify these cases and create a new id for the second profile. These 
# layers need to be checked in the source data in the future.
# Start by identifying events (id) with two layers where profund_sup == 0.
soildata[, n_surface_layers := sum(profund_sup == 0), by = id]
View(soildata[n_surface_layers >= 2, .(id, camada_nome, profund_sup, profund_inf)])


# Partition valid depth intervals into the minimum number of non-overlapping
# profile sequences. Layers with missing or invalid depths remain in lane 0.
assign_profile_lane <- function(profund_sup, profund_inf) {
  lane <- integer(length(profund_sup))
  valid <- which(is.finite(profund_sup) & is.finite(profund_inf) & profund_sup < profund_inf)
  if (length(valid) == 0L) return(lane)

  ordered <- valid[order(profund_sup[valid], profund_inf[valid], valid)]
  lane_end <- numeric()
  for (row in ordered) {
    available <- which(lane_end <= profund_sup[row])
    lane_id <- if (length(available) > 0L) available[1L] else length(lane_end) + 1L
    lane_end[lane_id] <- profund_inf[row]
    lane[row] <- lane_id
  }
  lane
}
# We assign a profile lane to each layer. Layers with missing or invalid depths
# remain in lane 0.
soildata[,
  profile_lane := assign_profile_lane(profund_sup, profund_inf),
  by = id
]
soildata[, has_multiple_profiles := any(profile_lane > 1L), by = id]
if (FALSE) {
  View(soildata[
    has_multiple_profiles == TRUE,
    .(id, camada_nome, profund_sup, profund_inf, profile_lane)
  ])
}

profile_candidates <- soildata[
  profund_sup == 0 & has_multiple_profiles == TRUE,
  .(n_surface_layers = .N),
  by = id
][n_surface_layers >= 2L]
profile_stats <- soildata[profile_lane > 0L, .(
  n_layers = .N
), by = .(id, profile_lane)]
profile_candidates <- profile_stats[
  id %in% profile_candidates$id,
  .(n_profiles = .N),
  by = id
]

# Keep id as the source event identifier and assign a separate profile_id.
soildata[, profile_id := id]
profile_assignments <- profile_stats[
  id %in% profile_candidates$id & profile_lane > 1L,
  .(id, profile_lane, profile_id = paste0(id, "-profile-", profile_lane))
]
if (nrow(profile_assignments) > 0L) {
  soildata[
    profile_assignments,
    on = .(id, profile_lane),
    profile_id := i.profile_id
  ]
}
cat("Events split into multiple profiles:", nrow(profile_candidates), "\n")
print(profile_candidates[, .(id, n_profiles)])
soildata[, profile_lane := NULL]

# Recheck zero-thickness bottom layers after separating profiles. The earlier
# event-level pass cannot identify the bottom of a shallower second profile.
soildata[, max_profund_inf := if (all(is.na(profund_inf))) {
  NA_real_
} else {
  max(profund_inf, na.rm = TRUE)
}, by = profile_id]
soildata[
  profund_sup == profund_inf & profund_inf == max_profund_inf,
  profund_inf := profund_inf + plus_depth
]
soildata[, max_profund_inf := NULL]















# Overlapping layers ###########################################################











# Fine earth
# R layers are consolidated rock layers. These layers should have terrafina == NA_real.
# Correct samples with terrafina == 0 g/kg
# When terrafina == 0, we set the fine earth content to 1000 g/kg.
soildata[terrafina == 0, .N]
# 23 samples with terrafina == 0
cols <- c("id", "camada_nome", "profund_sup", "profund_inf", "terrafina", "argila", "taxon_sibcs")
print(soildata[terrafina == 0, ..cols])
# If camada_nome != "R", set terrafina to 1000 g/kg
soildata[camada_nome == "R", terrafina := NA_real_]
soildata[camada_nome == "2R", terrafina := NA_real_]
soildata[camada_nome == "IIR", terrafina := NA_real_]
soildata[terrafina == 0, terrafina := 1000]

# Fine earth content
soildata[, esqueleto := 1000 - terrafina]
# Check samples with esqueleto > 800
print(soildata[esqueleto > 800, .N])
# 133 sample with esqueleto > 800
# Correct soil skeleton and fine earth content
soildata[id == "ctb0565-Perfil-08" & camada_nome == "BC1", `:=`(
  esqueleto = 0,
  terrafina = 1000
)]
# These datasets have been checked
ctb_ok <- c(
  "ctb0006", "ctb0011", "ctb0017", "ctb0025", "ctb0033", "ctb0038", "ctb0044", "ctb0562",
  "ctb0600", "ctb0605", "ctb0606"
)
cols <- c("id", "camada_nome", "profund_sup", "profund_inf", "esqueleto", "terrafina")
# View(soildata[!(dataset_id %in% ctb_ok) & esqueleto > 800, ..cols])
# Filter out samples with skeleton > 1000.
# Some layers have esqueleto > 1000. This is not possible.
# We filter out these layers. These layers need to be checked in the source data in the
# future.
soildata <- soildata[is.na(esqueleto) | esqueleto < 1000]
summary_soildata(soildata)
# Layers: 57326
# Events: 16868
# Georeferenced events: 14387
# Datasets: 255

# Clean camada_nome
soildata[, camada_nome := as.character(camada_nome)]
# ignore
# soildata[is.na(camada_nome) | camada_nome == "" & profund_sup == 0, camada_nome := "A"]
# ignore
# soildata[is.na(camada_nome) | camada_nome == "" & profund_sup != 0, camada_nome := NA_character_]
soildata[, camada_nome := gsub("p1", "pl", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("Çg", "Cg", camada_nome, ignore.case = FALSE)]
# ignore
# soildata[, camada_nome := gsub("0", "O", camada_nome, ignore.case = FALSE)]
soildata[grepl(",OOE+O", camada_nome, fixed = TRUE), camada_nome := NA_character_]
# Convert starting "ll" and "ii" to Roman letters in layer names (camada_nome)
soildata[, camada_nome := sub("^ll", "II", camada_nome)]
soildata[, camada_nome := sub("^ii", "II", camada_nome)]
# bw -> Bw
soildata[, camada_nome := sub("^bw", "Bw", camada_nome)]
# O-2O -> 0-20
soildata[, camada_nome := sub("^O-2O", "0-20", camada_nome)]
# 3O-5O -> 30-50
soildata[, camada_nome := sub("^3O-5O", "30-50", camada_nome)]
# 3O-2O -> 30-20
soildata[, camada_nome := sub("^3O-2O", "30-20", camada_nome)]
# B21CN -> B21cn
soildata[, camada_nome := sub("^B21CN", "B21cn", camada_nome)]
# Bcn21 -> B21cn
soildata[, camada_nome := sub("^Bcn21", "B21cn", camada_nome)]
# B2TPL -> B2tpl
soildata[, camada_nome := sub("^B2TPL", "B2tpl", camada_nome)]
# B31PL -> B31pl
soildata[, camada_nome := sub("^B31PL", "B31pl", camada_nome)]
# B3PL -> B3pl
soildata[, camada_nome := sub("^B3PL", "B3pl", camada_nome)]
# 0-20cm
soildata[, camada_nome := gsub("0-20cm", "0-20", camada_nome, ignore.case = FALSE)]
# 20-40cm
soildata[, camada_nome := gsub("20-40cm", "20-40", camada_nome, ignore.case = FALSE)]
# 40-60cm
soildata[, camada_nome := gsub("40-60cm", "40-60", camada_nome, ignore.case = FALSE)]
# o -> O
soildata[, camada_nome := gsub("^o$", "O", camada_nome, ignore.case = FALSE)]
# bHS -> Bhs
soildata[, camada_nome := gsub("^bHS", "bhs", camada_nome, ignore.case = FALSE)]
# T -> t
soildata[, camada_nome := gsub("T", "t", camada_nome, ignore.case = FALSE)]
# NULL -> NA
soildata[camada_nome == "NULL", camada_nome := NA_character_]
# C2G -> C2g
soildata[, camada_nome := gsub("C2G", "C2g", camada_nome, ignore.case = FALSE)]
# ^g$ -> G
soildata[, camada_nome := gsub("^g$", "G", camada_nome, ignore.case = FALSE)]
# Print unique layer names
sort(unique(soildata[, camada_nome]))

# Particle size distribution
# Check if the sum of the three fractions is 1000 g/kg
soildata[, argila := round(argila)]
soildata[, silte := round(silte)]
soildata[, areia := round(areia)]
soildata[, psd := argila + silte + areia]
# Correct the particle size fractions
# Some layers have incorrect particle size fractions. We correct these layers based on visual
# inspection of the source documents. These corrections need to be implemented in the source data
# in the future.
soildata[
  id == "ctb0591-P-13-Sao-Mateus-do-Sul" & camada_nome == "BW1" & argila == 597, `:=` (
    argila = 1000 - 160 - 90,
    silte = 160,
    areia = 90
  )
]
soildata[
  id == "ctb0591-P-13-Sao-Mateus-do-Sul" & camada_nome == "BW2" & argila == 0, `:=` (
    argila = 1000 - 140 - 100,
    silte = 140,
    areia = 100
  )
]
soildata[
  id == "ctb0591-P-13-Sao-Mateus-do-Sul" & camada_nome == "BW3" & argila == 148, `:=`(
    argila = 1000 - 130 - 100,
    silte = 130,
    areia = 100
  )
]
soildata[
  id == "ctb0620-Á-de-Chapecó-3" & camada_nome == "Ap" & argila == 0, `:=`(
    argila = 1000 - 360 - 20,
    silte = 360,
    areia = 20
  )
]
soildata[
  id == "ctb0646-PERFIL-20" & camada_nome == "C2" & argila == 0, `:=`(
    argila = NA_real_,
    silte = NA_real_,
    areia = NA_real_
  )
]
# Check if there is any size fraction equal to 0
# clay
ctb_zero_clay <- c(
  "ctb0607", "ctb0656", "ctb0666", "ctb0679", "ctb0020", "ctb0691", "ctb0695", "ctb0698",
  "ctb0705"
)
soildata[argila == 0 & !dataset_id %in% ctb_zero_clay, .N]
# 12 layers (they need to be checked in the source data in the future)
# Print the layers with clay == 0
cols <- c("id", "camada_nome", "argila", "silte", "areia")
soildata[argila == 0 & !dataset_id %in% ctb_zero_clay, ..cols]
# silt
soildata[silte == 0, .N]
# 57 layers (they need to be checked in the source data in the future)
# Print the layers with silt == 0
soildata[silte == 0, ..cols]
# sand
soildata[areia == 0, .N]
# 102 layers (they need to be checked in the source data in the future)
# Print the layers with sand == 0
soildata[areia == 0, ..cols]

# Check if the sum of the three fractions is 1000 g/kg
# We also check for values close to 100% (90-110%), which may be due to rounding errors.
soildata[psd != 1000, .N]
# 188 layers
psd_lims <- 900:1100
soildata[!is.na(psd) & !(psd %in% psd_lims), .N]
# 2 layers, both from ctb0025-Perfil-38. We drop these layers. They need to be checked in the
# source data in the future.
soildata <- soildata[!(id == "ctb0025-Perfil-38" & camada_nome == "Bt2")]
soildata <- soildata[!(id == "ctb0025-Perfil-38" & camada_nome == "BC")]
cols <- c("id", "camada_nome", "argila", "silte", "areia", "psd")
soildata[!is.na(psd) & !(psd %in% psd_lims), ..cols]
# If the sum of the three fractions is different from 1000 g/kg, adjust their values, adding the
# difference to the silt fraction. We only consider layers with psd between 900 and 1100 g/kg.
soildata[psd != 1000, argila := round(argila / psd * 1000)]
soildata[psd != 1000, areia := round(areia / psd * 1000)]
soildata[psd != 1000, silte := 1000 - argila - areia]
soildata[, psd := round(argila + silte + areia)]
soildata[psd != 1000, psd]
soildata[, psd := NULL]

# Correct bulk density values
# Some layers have incorrect bulk density values. We correct these layers based on inspection of 
# the source documents. These corrections need to be implemented in the source data
# in the future.
soildata[id == "ctb0562-Perfil-13" & camada_id == 2, dsi := ifelse(dsi == 2.6, 0.86, dsi)]
soildata[id == "ctb0562-Perfil-14" & camada_id == 1, dsi := ifelse(dsi == 2.53, 1.09, dsi)]
soildata[id == "ctb0562-Perfil-14" & camada_id == 2, dsi := ifelse(dsi == 2.6, 0.9, dsi)]
soildata[id == "ctb0608-15-V-RCC" & camada_id == 3, dsi := ifelse(dsi == 0.42, 1.94, dsi)]
soildata[id == "ctb0631-Perfil-17" & camada_id == 3, dsi := ifelse(dsi == 0.14, 1.1, dsi)]
soildata[id == "ctb0700-15" & camada_id == 1, dsi := ifelse(dsi == 2.53, 1.6, dsi)]
soildata[id == "ctb0700-15" & camada_id == 2, dsi := ifelse(dsi == 2.56, 1.49, dsi)]
soildata[id == "ctb0771-26" & camada_id == 1, dsi := ifelse(dsi == 2.59, 1.32, dsi)]
soildata[id == "ctb0771-26" & camada_id == 2, dsi := ifelse(dsi == 2.56, 1.37, dsi)]
soildata[id == "ctb0777-1" & camada_id == 1, dsi := ifelse(dsi == 2.65, 1.35, dsi)]
soildata[id == "ctb0787-1" & camada_id == 2, dsi := ifelse(dsi == 2.58, 1.35, dsi)]
soildata[id == "ctb0787-4" & camada_id == 1, dsi := ifelse(dsi == 2.35, 1.35, dsi)]
soildata[id == "ctb0787-4" & camada_id == 2, dsi := ifelse(dsi == 1.3, 1.27, dsi)]
soildata[id == "ctb0811-2" & camada_id == 3, dsi := ifelse(dsi == 0.34, 1.64, dsi)]
soildata[id == "ctb0702-P-46" & camada_id == 1, dsi := ifelse(dsi == 2.08, 1.08, dsi)] # check document
soildata[id == "ctb0572-Perfil-063" & camada_id == 2, dsi := ifelse(dsi == 0.34, 1.84, dsi)]
soildata[id == "ctb0605-P-06" & camada_id == 2, dsi := ifelse(dsi == 0.31, 1.32, dsi)]
summary_soildata(soildata)
# Layers: 57324
# Events: 16868
# Georeferenced events: 14387
# Datasets: 255



# THIS HAS ALREADY BEEN CORRECTED IN THE ORIGINAL DATASET. WE KEEP IT HERE FOR REFERENCE.
soildata[
  dataset_id == "ctb0607" & observacao_id == "PERFIL-92",
  carbono := ifelse(carbono == 413, 41.3, carbono)
]

# THIS HAS ALREADY BEEN CORRECTED IN THE ORIGINAL DATASET. WE KEEP IT HERE FOR REFERENCE.
# ctb0718-51. carbon is recorded as 145 g/kg. It is corrected to 14.5 g/kg.
soildata[
  dataset_id == "ctb0718" & observacao_id == "51",
  carbono := ifelse(carbono == 145, 14.5, carbono)
]

# FIGURE 15.1
# Check spatial distribution after cleaning layers
soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
# Plot spatial distribution
file_path <- fig_path("151_spatial_distribution_after_cleaning_layers.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["code_state"],
  col = "gray95", lwd = 0.5, reset = FALSE,
  main = "Spatial distribution of SoilData after cleaning layers"
)
plot(soildata_sf["estado_id"], cex = 0.3, add = TRUE, pch = 20)
dev.off()

# Clean soil bulk density data
# Delete possible inconsistent values
soildata[dsi > 2.5, dsi := NA_real_]

# Correct inconsistent soil bulk density values
soildata[id == "ctb0058-RN_20", dsi := ifelse(dsi == 2.11, 1.11, dsi)]

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255
data.table::fwrite(soildata, "data/15_soildata.txt", sep = "\t")

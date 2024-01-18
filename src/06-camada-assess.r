# title: "Brazilian Soil Dataset 2023"
# subtitle: A Comprehensive Dataset of Soil Properties for Brazil
# author: "Alessandro Samuel-Rosa"
# date: "2024-01-18"

# Summary
# This R script reads processed data from the 'camada' and 'observacao' tables of the Brazilian
# Soil Dataset. It identifies duplicated observations based on coordinates and observation date,
# and extracts the dataset ID and observation ID of these duplicates. The script then identifies
# and removes these duplicated records from the 'camada' table. The goal is to ensure data
# integrity by eliminating duplicate entries in the dataset. Then the script creates two new
# variables in the 'camada' table, 'fragmentos' and 'profund,' which are the difference between
# 1000 and the variable 'terrafina' and the average of the variables 'profund_sup' and 
# 'profund_inf,' respectively. Finally, the script plots the empirical distribution of the data 
# and the boxplot of each of the ten continuous variables side by side. The goal is to assess the 
# distribution of the data and identify potential outliers.

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

# Create a new variable named 'fragmentos' in the 'camada' table, which is the difference between
# 1000 and the variable 'terrafina' in the 'camada' table. Create a new variable named 'profund'
# in the 'camada' table, which is the average of the variables 'profund_sup' and 'profund_inf' in 
# the 'camada' table.
camada <-
  camada %>%
  dplyr::mutate(
    fragmentos = 1000 - terrafina,
    profund = (profund_sup + profund_inf) / 2
  )

# Plot the empirical distribution of the data
png("res/fig/camada-matriz.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(oma = c(0, 0, 1, 0), las = 1)
camada %>%
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
  plot(cex = 0.3, col = "firebrick", main = "")
dev.off()

# Prepare figure with the boxplot of each of the ten continuous variable side by side. Use function
# 'layout' to create the layout. Use function 'par' to set the outer margins of the figure. Use function 'boxplot' from package 'graphics' to plot the boxplots. Compute the number of non-na records of the plotted variable and append it to the ylab. 
png("res/fig/camada-boxplot.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(oma = c(0, 0, 0, 0), las = 1, mar = c(1, 4, 1, 1))
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), nrow = 2, ncol = 5, byrow = TRUE))
boxplot(camada$profund, main = "", ylab = paste("Depth (cm), n =", sum(!is.na(camada$profund))))
boxplot(camada$carbono, main = "", ylab = paste("Carbon (g/kg), n =", sum(!is.na(camada$carbono))))
boxplot(camada$argila, main = "", ylab = paste("Clay (g/kg), n =", sum(!is.na(camada$argila))))
boxplot(camada$areia, main = "", ylab = paste("Sand (g/kg), n =", sum(!is.na(camada$areia))))
boxplot(camada$silte, main = "", ylab = paste("Silt (g/kg), n =", sum(!is.na(camada$silte))))
par(mar = c(1, 4, 1, 1))
boxplot(camada$fragmentos,
  main = "", ylab = paste("Coarse fragments (g/kg), n =", sum(!is.na(camada$fragmentos)))
)
boxplot(camada$dsi, main = "", ylab = paste("BD (kg/dm³), n =", sum(!is.na(camada$dsi))))
boxplot(camada$ctc, main = "", ylab = paste("CEC (cmolc/kg), n =", sum(!is.na(camada$ctc))))
boxplot(camada$ph, main = "", ylab = paste("pH (-), n =", sum(!is.na(camada$ph))))
boxplot(camada$ce, main = "", ylab = paste("EC (mS/cm), n =", sum(!is.na(camada$ce))))
dev.off()

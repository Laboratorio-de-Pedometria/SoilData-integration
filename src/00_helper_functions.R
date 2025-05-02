# title: SoilData Integration
# subtitle: Helper functions
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
# licence: MIT

# Describe soil data ###############################################################################
# Create function to describe a data.frame. Use an argument na.rm = TRUE.
summary_soildata <- function(x, na.rm = TRUE) {
  cat("Column names:")
  cat("\n", paste(names(x)), collapse = " ")
  cat("\nLayers:", nrow(x))
  cat("\nEvents:", nrow(unique(x[, "id"])))
  cat("\nGeoreferenced events:", nrow(unique(x[!is.na(coord_x) & !is.na(coord_y), "id"])))
  cat("\n")
}

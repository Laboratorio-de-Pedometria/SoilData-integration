# title: SoilData Integration
# subtitle: Helper functions
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2026
# licence: MIT

# Set version ##################################################################
sdi <- "sdi2025"

# Create directories for results if they do not exist ##########################
if (!dir.exists("res/tab")) { 
  dir.create("res/tab", recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists("res/fig")) { 
  dir.create("res/fig", recursive = TRUE, showWarnings = FALSE)
}

# General functions to construct file paths ####################################
# Append the version to the filename
fig_path <- function(filename) {
  file.path("res", "fig", paste0(sdi, "_", filename))
}
tab_path <- function(filename) {
  file.path("res", "tab", paste0(sdi, "_", filename))
}

# Describe soil data ###########################################################
# Create function to describe a data.frame. Use an argument na.rm = TRUE.
summary_soildata <- function(x, na.rm = TRUE) {
  # If 'id' is missing, generate temporary 'id' column by concatenating 'dataset_id' and 'observacao_id'
  if (!"id" %in% names(x) & all(c("dataset_id", "observacao_id") %in% names(x))) {
    x[, id := paste0(dataset_id, "_", observacao_id)]
    temp_id <- TRUE
  } else {
    temp_id <- FALSE
  }
  cat("Column names:")
  cat("\n", paste(sort(names(x))), collapse = " ")
  cat("\nLayers:", nrow(x))
  n_events <- nrow(unique(x[, "id"]))
  cat("\nEvents:", n_events)
  n_geo <- nrow(unique(x[!is.na(coord_x) & !is.na(coord_y), "id"]))
  cat("\nGeoreference:", n_geo, "(yes) /", n_events - n_geo, "(no)")
  n_year <- nrow(unique(x[!is.na(data_ano), "id"]))
  cat("\nDate:", n_year, "(yes) /", n_events - n_year, "(no)")
  n_dataset <- nrow(unique(x[, "dataset_id"]))
  cat("\nDatasets:", n_dataset)
  cat("\n")
  if (temp_id) {
    x[, id := NULL] # Remove temporary 'id' column
  }
}

# Summarize dates in soil data #################################################
summary_date <- function(x) {
  # If 'id' is missing, generate temporary 'id' column by concatenating 'dataset_id' and 'observacao_id'
  if (!"id" %in% names(x) & all(c("dataset_id", "observacao_id") %in% names(x))) {
    x[, id := paste0(dataset_id, "_", observacao_id)]
    temp_id <- TRUE
  } else {
    temp_id <- FALSE
  }

  n_events <- nrow(unique(x[, "id"]))
  n_date <- nrow(unique(x[!is.na(data_ano), "id"]))
  cat("Date:", n_date, "(yes) /", n_events - n_date, "(no)\n")

  if (temp_id) {
    invisible(x[, id := NULL])
  }
}

# Query SoilData API by otherIdValue (ctb) #########################################################
# Function to query SoilData API by otherIdValue (ctb)
# If doi = TRUE, return only the DOI (global_id), else return the full search_result
# Query SoilData API by otherIdValue (ctb) with per_page control
ctb_query <- function (ctb, doi = TRUE, per_page = 1000) {
  
  # --- Input Validation ---
  # Ensure per_page does not exceed the API's maximum limit
  if (per_page > 1000) {
    per_page <- 1000
    warning("per_page was capped at the maximum value of 1000.")
  }
  
  # Check if we are searching for one or multiple ctb values
  if (length(ctb) > 1) {
    # Build the ("value1" OR "value2") part of the query
    query_part <- paste0("\"", ctb, "\"", collapse = " OR ")
    query_string <- sprintf("otherIdValue:(%s)", query_part)
  } else {
    # Use the simpler query string for a single value
    query_string <- sprintf("otherIdValue:\"%s\"", ctb)
  }
  
  # URL-encode the query string to handle special characters
  encoded_query <- URLencode(query_string, reserved = TRUE)
  
  # Construct the final API URL, now including the per_page parameter
  api_url <- sprintf(
    "https://soildata.mapbiomas.org/api/search?q=%s&type=dataset&per_page=%d",
    encoded_query,
    per_page
  )
  
  # Make the API call
  search_result <- jsonlite::fromJSON(api_url)
  
  # If doi is TRUE, return only the DOI (global_id), else return the full search result
  if (doi && search_result$data$total_count > 0) {
    search_result <- search_result$data$items$global_id
  }
  
  return(search_result)
}

# Query SoilData API endpoint by global_id (DOI) ###################################################
# Function to query SoilData API by global_id (DOI)
# doi_query <- function(doi) {
#   details_url <- sprintf(
#     "https://soildata.mapbiomas.org/api/datasets/:persistentId/?persistentId=%s",
#     doi
#   )
#   details_result <- jsonlite::fromJSON(details_url)
#   return(details_result)
# }


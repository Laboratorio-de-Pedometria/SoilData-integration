# title: SoilData Integration
# subtitle: Helper functions
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2026
# licence: MIT

# Set version ##################################################################
sdi <- "sdi2025"

# Create directories if they do not exist ######################################
if (!dir.exists("res/tab")) { 
  dir.create(path = "res/tab", recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists("res/fig")) {
  dir.create(path = "res/fig", recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists("tmp")) { 
  dir.create(path = "tmp", recursive = TRUE, showWarnings = FALSE)
}

# General functions to construct file paths ####################################
# Append the version to the filename
fig_path <- function(filename) {
  file.path("res", "fig", paste0(sdi, "_", filename))
}
tab_path <- function(filename) {
  file.path("res", "tab", paste0(sdi, "_", filename))
}

# Read Brazilian state boundaries ##############################################
read_brazil_states <- function(file_path = "data/brazil_states.geojson") {
  if (!file.exists(file_path)) {
    brazil <- geobr::read_state(simplified = FALSE)
    sf::st_write(brazil, file_path, quiet = TRUE)
  } else {
    brazil <- sf::st_read(file_path, quiet = TRUE)
  }
  brazil
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

# Outlier detection ############################################################

# Robust outlier detection via MAD with zero-spread safety
flag_mad_dt <- function(x, c_val = 3.0) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1.4826, na.rm = TRUE)
  if (is.na(mad_val) || mad_val == 0) return(rep(FALSE, length(x)))
  outlier <- abs(x - med) > (c_val * mad_val)
  outlier[is.na(outlier)] <- FALSE
  return(outlier)
}

# Main curation function
curate_soil_data_dt <- function(dt, 
                                col_id = "point_id", 
                                col_layer = "layer", 
                                col_soc = "soc", 
                                col_clay = "clay", 
                                col_silt = "silt", 
                                col_sand = "sand", 
                                col_bd = "bd",
                                texture_tol_pct = 0.05,
                                weight_texture = 2L,
                                weight_inversion = 2L,
                                weight_ptf = 2L,
                                weight_mad = 1L,
                                cutoff_inconsistent = 3L) {
  
  # Work on a shallow/deep copy to prevent unintended side effects on input
  dt_proc <- data.table::copy(data.table::as.data.table(dt))
  
  # --- Test 1: Particle Size Sum Constraint (5% tolerance) ---
  cols_texture <- c(col_clay, col_silt, col_sand)
  if (all(cols_texture %in% names(dt_proc))) {
    dt_proc[, texture_sum := rowSums(.SD, na.rm = FALSE), .SDcols = cols_texture]
    
    # Auto-detect baseline: 100% or 1000 g/kg
    base_ref <- ifelse(median(dt_proc$texture_sum, na.rm = TRUE) > 500, 1000, 100)
    tol <- texture_tol_pct * base_ref
    
    dt_proc[
      ,
      flag_invalid_texture := !is.na(texture_sum) & abs(texture_sum - base_ref) > tol
    ]
    dt_proc[, texture_sum := NULL]
  } else {
    dt_proc[, flag_invalid_texture := FALSE]
  }

  # --- Test 2: Vertical SOC Gradient Inversion (0-20 vs 30-50 cm) ---
  dt_top <- dt_proc[get(col_layer) %in% c("0-20", "0_20", 1),
    .(soc_top = get(col_soc)[1]),
    by = col_id
  ]
  dt_sub <- dt_proc[get(col_layer) %in% c("30-50", "30_50", 2),
    .(soc_sub = get(col_soc)[1]),
    by = col_id
  ]
  
  dt_paired <- merge(dt_top, dt_sub, by = col_id, all = FALSE)
  inverted_ids <- dt_paired[
    !is.na(soc_top) & !is.na(soc_sub) & soc_sub > soc_top, get(col_id)
  ]
  
  dt_proc[, flag_soc_inversion := get(col_id) %in% inverted_ids]

  # --- Test 3: Univariate Outliers via MAD (stratified by layer) ---
  dt_proc[, `:=`(
    flag_mad_soc  = flag_mad_dt(get(col_soc), c_val = 3.0),
    flag_mad_bd   = if (col_bd %in% names(dt_proc)) { 
      flag_mad_dt(get(col_bd), c_val = 3.0) } else { FALSE },
    flag_mad_clay = if (col_clay %in% names(dt_proc)) { 
      flag_mad_dt(get(col_clay), c_val = 3.0) } else { FALSE }
  ), by = col_layer]

  # --- Test 4: Multivariate Pedotransfer Residual Check (rlm by layer) ---
  dt_proc[, `:=`(flag_ptf_bd = FALSE, std_ptf_res = NA_real_)]
  
  model_cols <- c(col_bd, col_soc, col_clay)
  if (all(model_cols %in% names(dt_proc))) {
    layers <- unique(dt_proc[[col_layer]])
    
    for (lay in layers) {
      idx <- which(
        dt_proc[[col_layer]] == lay & complete.cases(dt_proc[, ..model_cols])
      )
      
      if (length(idx) >= 15L) {
        formula_ptf <- as.formula(
          paste0(col_bd, " ~ log(", col_soc, " + 0.1) + ", col_clay)
        )
        fit <- tryCatch(MASS::rlm(
          formula_ptf,
          data = dt_proc[idx], method = "M"
        ), error = function(e) NULL)
        
        if (!is.null(fit)) {
          res <- residuals(fit)
          mad_res <- mad(res, na.rm = TRUE)
          if (!is.na(mad_res) && mad_res > 0) {
            std_res <- res / mad_res
            data.table::set(dt_proc,
              i = idx,
              j = "std_ptf_res", value = std_res
            )
            data.table::set(dt_proc,
              i = idx,
              j = "flag_ptf_bd", value = abs(std_res) > 3.0
            )
          }
        }
      }
    }
  }

  # --- Cross-Validation Score and Categorization ---
  dt_proc[
    ,
    inconsistency_score := (as.integer(flag_invalid_texture) * weight_texture) +
     (as.integer(flag_soc_inversion) * weight_inversion) +
      (as.integer(flag_ptf_bd) * weight_ptf) + 
      (as.integer(flag_mad_soc) * weight_mad) + 
      (as.integer(flag_mad_bd) * weight_mad) + 
      (as.integer(flag_mad_clay) * weight_mad)
  ]

  dt_proc[, quality_flag := data.table::fcase(
    inconsistency_score == 0L, "Consistent",
    inconsistency_score < cutoff_inconsistent, "Suspect (Inspect)",
    default = "Inconsistent (Discard/Audit)"
  )]

  return(dt_proc)
}

# Query SoilData API by otherIdValue (ctb) #####################################
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


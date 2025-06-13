# global.R

# ============================================================================
# LIBRARIES
# ============================================================================
library(shiny)
library(redcapAPI)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)
library(bslib)
library(httr)
library(stringr)
library(xml2)
library(fontawesome)
library(tidyr)
library(reactable)
library(htmltools)
library(data.table)
library(purrr)
library(jsonlite)

# ============================================================================
# ENVIRONMENT SETUP
# ============================================================================

# 1) Identify whether we are hosted
is_hosted <- Sys.getenv("RDM_TOKEN") != ""

# 2) Load tokens from environment or config
if (is_hosted) {
  rdm_token  <- Sys.getenv("RDM_TOKEN")
  fac_token  <- Sys.getenv("FAC_TOKEN")
  
  # Disable SSL verification in the hosted environment (NOT recommended for production)
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
} else {
  conf <- config::get(file = "config.yml")
  rdm_token  <- conf$rdm_token
  fac_token  <- conf$fac_token
}

# The RedCap URL
url <- "https://redcapsurvey.slu.edu/api/"

# Source helper functions
source("R/helpers.R")

# ============================================================================
# FUNCTIONS
# ============================================================================

# Simple API call for just the two forms we need
get_rdm_data <- function() {
  tryCatch({
    cat("Pulling resident_data and faculty_evaluation forms...\n")
    
    # API call using labels instead of raw codes
    formData <- list(
      "token" = rdm_token,
      content = 'record',
      action = 'export',
      format = 'json',
      type = 'flat',
      csvDelimiter = '',
      'forms[0]' = 'resident_data',
      'forms[1]' = 'faculty_evaluation',
      rawOrLabel = 'label',        # Using labels instead of raw codes
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'false',
      exportDataAccessGroups = 'false',
      returnFormat = 'json'
    )
    
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (httr::status_code(response) != 200) {
      stop("REDCap API call failed with status: ", httr::status_code(response))
    }
    
    # Get the response as text first, then parse JSON
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    rdm_data <- jsonlite::fromJSON(response_text)
    
    cat("Data loaded. Total rows:", nrow(rdm_data), "\n")
    cat("Columns:", ncol(rdm_data), "\n")
    
    # Check repeat instruments
    if ("redcap_repeat_instrument" %in% names(rdm_data)) {
      repeat_counts <- table(rdm_data$redcap_repeat_instrument, useNA = "ifany")
      cat("Repeat instrument counts:\n")
      print(repeat_counts)
    }
    
    return(rdm_data)
    
  }, error = function(e) {
    cat("Error in RDM API pull:", e$message, "\n")
    return(NULL)
  })
}

# Get resident data (non-repeating)
get_resident_data <- function(rdm_data) {
  if (is.null(rdm_data)) return(NULL)
  
  resident_data <- rdm_data %>%
    filter(is.na(redcap_repeat_instrument) | redcap_repeat_instrument == "")
  
  cat("Resident data extracted. Total rows:", nrow(resident_data), "\n")
  
  # Filter out archived residents
  if ("res_archive" %in% names(resident_data)) {
    resident_data <- resident_data %>%
      filter(is.na(res_archive) | res_archive != "Yes")
    
    cat("Active (non-archived) residents:", nrow(resident_data), "\n")
  } else {
    cat("No res_archive column found - using all residents\n")
  }
  
  return(resident_data)
}

# Get faculty evaluation data (repeating)
get_faculty_eval_data <- function(rdm_data) {
  if (is.null(rdm_data)) return(NULL)
  
  # Use the correct string from your output: "Faculty Evaluation" (with space and capitals)
  fac_eval_data <- rdm_data %>%
    filter(redcap_repeat_instrument == "Faculty Evaluation")
  
  cat("Faculty evaluation data extracted. Rows:", nrow(fac_eval_data), "\n")
  
  if (nrow(fac_eval_data) > 0) {
    cat("Sample record_ids:", paste(head(unique(fac_eval_data$record_id)), collapse = ", "), "\n")
  }
  
  return(fac_eval_data)
}

# Function to get faculty data
get_faculty_data <- function() {
  tryCatch({
    fac_dat <- full_api_pull(fac_token, url)
    cat("Faculty data loaded. Rows:", nrow(fac_dat), "\n")
    return(fac_dat)
  }, error = function(e) {
    cat("Error in faculty API pull:", e$message, "\n")
    return(NULL)
  })
}

# Function to calculate resident level
calculate_resident_level <- function(coach_data) {
  
  if (!inherits(coach_data, "data.frame")) {
    warning("Input to calculate_resident_level must be a data frame or tibble. Returning as-is.")
    return(coach_data)
  }
  
  # Debug: Check what we have in type and grad_yr
  cat("Sample type values:", paste(head(unique(coach_data$type)), collapse = ", "), "\n")
  cat("Sample grad_yr values:", paste(head(unique(coach_data$grad_yr)), collapse = ", "), "\n")
  
  # Check if required columns exist
  has_type <- "type" %in% names(coach_data)
  has_grad_yr <- "grad_yr" %in% names(coach_data)
  
  if (!has_type || !has_grad_yr) {
    cat("Missing required columns - defaulting all to Intern\n")
    coach_data$Level <- "Intern"
    return(coach_data)
  }
  
  # Get current academic year (July 1 to June 30)
  current_date <- Sys.Date()
  current_academic_year <- ifelse(format(current_date, "%m") >= "07", 
                                  as.numeric(format(current_date, "%Y")), 
                                  as.numeric(format(current_date, "%Y")) - 1)
  
  cat("Current academic year (July-June):", current_academic_year, "\n")
  
  # Ensure grad_yr is numeric
  coach_data <- coach_data %>%
    mutate(
      grad_yr = suppressWarnings(as.numeric(grad_yr))
    )
  
  # Debug: Check grad_yr after conversion
  cat("Sample grad_yr after conversion:", paste(head(unique(coach_data$grad_yr)), collapse = ", "), "\n")
  
  # Calculate Level based on type and grad_yr
  coach_data <- coach_data %>%
    mutate(
      Level = case_when(
        # Prelim residents are always Interns (one year only)
        tolower(type) == "preliminary" ~ "Intern",
        
        # Categorical residents based on graduation year
        tolower(type) == "categorical" & grad_yr == current_academic_year + 3 ~ "Intern",  # PGY1 - graduates in 3 years
        tolower(type) == "categorical" & grad_yr == current_academic_year + 2 ~ "PGY2",    # PGY2 - graduates in 2 years  
        tolower(type) == "categorical" & grad_yr == current_academic_year + 1 ~ "PGY3",    # PGY3 - graduates in 1 year
        
        # Default for any unmatched cases
        TRUE ~ "Intern"
      )
    )
  
  level_counts <- table(coach_data$Level)
  cat("Level counts - Intern:", ifelse("Intern" %in% names(level_counts), level_counts[["Intern"]], 0), 
      "PGY2:", ifelse("PGY2" %in% names(level_counts), level_counts[["PGY2"]], 0), 
      "PGY3:", ifelse("PGY3" %in% names(level_counts), level_counts[["PGY3"]], 0), "\n")
  
  return(coach_data)
}

# Function to count faculty evaluations for a specific resident by record_id
count_resident_faculty_evals <- function(fac_eval_data, resident_data, resident_name) {
  if (is.null(fac_eval_data) || is.null(resident_data) || is.null(resident_name)) {
    return(0)
  }
  
  # Get the record_id for this resident
  resident_record <- resident_data %>%
    filter(name == resident_name) %>%
    slice(1)
  
  if (nrow(resident_record) == 0) {
    return(0)
  }
  
  resident_id <- resident_record$record_id
  
  # Count evaluations for this specific record_id
  resident_evals <- fac_eval_data %>%
    filter(!is.na(record_id) & record_id == !!resident_id)
  
  eval_count <- nrow(resident_evals)
  
  return(eval_count)
}

# Function to count faculty evaluations by year/level for a specific resident
count_resident_evals_by_level <- function(fac_eval_data, resident_data, resident_name) {
  if (is.null(fac_eval_data) || is.null(resident_data) || is.null(resident_name)) {
    return(data.frame(Level = character(0), Count = numeric(0), Goal = numeric(0)))
  }
  
  # Get the resident's info
  resident_record <- resident_data %>%
    filter(name == resident_name) %>%
    slice(1)
  
  if (nrow(resident_record) == 0) {
    return(data.frame(Level = character(0), Count = numeric(0), Goal = numeric(0)))
  }
  
  resident_id <- resident_record$record_id
  current_level <- resident_record$Level
  
  # Get all evaluations for this resident
  resident_evals <- fac_eval_data %>%
    filter(!is.na(record_id) & record_id == !!resident_id)
  
  total_count <- nrow(resident_evals)
  
  # Since we can't determine historical levels by date, show current level progress
  # and indicate if they're on track for their current level
  current_goal <- get_eval_goal_by_level(current_level)
  
  # Create summary showing current level and total evaluations
  level_summary <- data.frame(
    Level = c("Total Evaluations"),
    Count = c(total_count),
    Goal = c(current_goal)
  )
  
  cat("Resident:", resident_name, "Level:", current_level, "Evaluations:", total_count, "Goal:", current_goal, "\n")
  
  return(level_summary)
}

# Function to count faculty evaluations by level  
count_evals_by_level <- function(fac_eval_data, resident_data) {
  if (is.null(fac_eval_data) || is.null(resident_data)) {
    return(data.frame(Level = character(0), Count = numeric(0), Goal = numeric(0)))
  }
  
  # Get evaluation counts for each resident
  resident_eval_counts <- resident_data %>%
    rowwise() %>%
    mutate(
      eval_count = count_resident_faculty_evals(fac_eval_data, resident_data, name)
    ) %>%
    ungroup()
  
  # Summarize by level
  level_summary <- resident_eval_counts %>%
    group_by(Level) %>%
    summarise(
      Count = sum(eval_count, na.rm = TRUE),
      Residents = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      Goal = case_when(
        Level == "Intern" ~ 15 * Residents,
        Level == "PGY2" ~ 20 * Residents,
        Level == "PGY3" ~ 25 * Residents,
        TRUE ~ 20 * Residents
      )
    )
  
  return(level_summary)
}

# Function to get evaluation goals by level
get_eval_goal_by_level <- function(level) {
  goals <- list(
    "Intern" = 15,
    "PGY2" = 20,
    "PGY3" = 25
  )
  
  return(goals[[level]] %||% 20)  # Default to 20 if level not found
}

# Function to create modern rating questions
create_rating_question <- function(input_id, question_text, left_label, right_label) {
  div(style = "margin-bottom: 25px;",
      h5(question_text, class = "question-text"),
      div(class = "rating-scale",
          div(style = "flex: 1; text-align: left; font-size: 12px; color: #6c757d;", left_label),
          div(style = "display: flex; gap: 10px;",
              lapply(1:5, function(i) {
                actionButton(paste0(input_id, "_", i), as.character(i),
                             class = "rating-option",
                             onclick = paste0("selectRating('", input_id, "', ", i, ")"))
              })
          ),
          div(style = "flex: 1; text-align: right; font-size: 12px; color: #6c757d;", right_label)
      ),
      # Hidden input to store the selected value
      tags$input(type = "hidden", id = input_id, value = "")
  )
}

# JavaScript for rating interactions
rating_js <- "
function selectRating(questionId, value) {
  // Remove selected class from all options for this question
  document.querySelectorAll('[id^=\"' + questionId + '_\"]').forEach(function(btn) {
    btn.classList.remove('selected');
  });
  
  // Add selected class to clicked option
  document.getElementById(questionId + '_' + value).classList.add('selected');
  
  // Update hidden input value
  document.getElementById(questionId).value = value;
  
  // Trigger Shiny input update
  Shiny.setInputValue(questionId, value);
}
"

# ============================================================================
# DATA LOADING
# ============================================================================

# Load data using the API call with labels
rdm_all_data <- get_rdm_data()
resident_data <- get_resident_data(rdm_all_data)
faculty_eval_data <- get_faculty_eval_data(rdm_all_data)
faculty_data <- get_faculty_data()

# Add level calculation to resident data
if (!is.null(resident_data)) {
  resident_data <- calculate_resident_level(resident_data)
}
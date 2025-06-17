
submit_evaluation_to_redcap <- function(eval_data, token, url) {
  # Add the required fields that were missing
  
  # 1. Add fac_fell_name from selected faculty
  if (exists("values") && !is.null(values$selected_faculty)) {
    eval_data$fac_fell_name <- values$selected_faculty$fac_name
  }
  
  # 2. Add current system date in Y-M-D format (REDCap requirement)
  eval_data$fac_eval_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Convert evaluation data to REDCap format
  redcap_data <- data.frame(eval_data, stringsAsFactors = FALSE)
  
  cat("Final REDCap submission data:\n")
  print(redcap_data)
  
  # Submit to REDCap
  result <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      data = jsonlite::toJSON(redcap_data, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  
  response_text <- httr::content(result, "text")
  cat("REDCap response status:", httr::status_code(result), "\n")
  cat("REDCap response:", response_text, "\n")
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit evaluation to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
  }
  
  return(response_text)
}

# Final clean version - using fieldNames parameter that works
get_next_faculty_eval_instance <- function(resident_id, token, url) {
  tryCatch({
    cat("=== GETTING NEXT INSTANCE FOR RESIDENT ID:", resident_id, "===\n")
    
    # Query for specific record with only needed fields
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = resident_id,
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    cat("REDCap query response status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) == 200) {
      response_text <- httr::content(response, "text", encoding = "UTF-8")
      all_data <- jsonlite::fromJSON(response_text)
      
      if (is.data.frame(all_data) && nrow(all_data) > 0) {
        # Filter for faculty_evaluation instances
        faculty_evals <- all_data[
          !is.na(all_data$redcap_repeat_instrument) & 
            all_data$redcap_repeat_instrument == "faculty_evaluation", 
        ]
        
        cat("Faculty evaluation records found:", nrow(faculty_evals), "\n")
        
        if (nrow(faculty_evals) > 0) {
          # Get all instance numbers
          instances <- as.numeric(faculty_evals$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          
          if (length(instances) > 0) {
            instances <- sort(instances)
            max_instance <- max(instances)
            next_instance <- max_instance + 1
            
            cat("✅ Found existing instances:", paste(instances, collapse = ", "), "\n")
            cat("✅ Max instance:", max_instance, "Next instance:", next_instance, "\n")
            
            return(next_instance)
          }
        }
      }
    } else {
      cat("❌ REDCap query failed with status:", httr::status_code(response), "\n")
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      cat("Error details:", error_text, "\n")
    }
    
    # Fallback to instance 1 if no existing instances found
    cat("⚠️ No existing faculty evaluation instances found, starting with instance 1\n")
    return(1)
    
  }, error = function(e) {
    cat("❌ Error in get_next_faculty_eval_instance:", e$message, "\n")
    return(1)
  })
}

# Function to submit pending faculty to REDCap faculty database as repeating instrument
submit_pending_faculty_to_redcap <- function(pending_data, token, url) {
  # Get the next available instance number for record_id = 1, pending_queue instrument
  next_instance <- tryCatch({
    result <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        records = "1",
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    if (httr::status_code(result) == 200) {
      records <- jsonlite::fromJSON(httr::content(result, "text"))
      if (is.data.frame(records) && nrow(records) > 0) {
        # Find max instance number for pending_queue
        existing_instances <- records[!is.na(records$redcap_repeat_instrument) & 
                                        records$redcap_repeat_instrument == "pending_queue", ]
        if (nrow(existing_instances) > 0) {
          instances <- as.numeric(existing_instances$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          if (length(instances) > 0) {
            max_instance <- max(instances)
            cat("Found existing pending_queue instances:", paste(sort(instances), collapse = ", "), "\n")
            cat("Next pending_queue instance:", max_instance + 1, "\n")
            max_instance + 1  # ✅ Return value to next_instance variable, don't exit function
          } else {
            1
          }
        } else {
          1
        }
      } else {
        1
      }
    } else {
      cat("No existing pending_queue instances found, starting with instance 1\n")
      1  # ✅ Return value to next_instance variable, don't exit function
    }
  }, error = function(e) {
    cat("Warning: Could not get existing pending instances, using instance = 1\n")
    1  # ✅ Return value to next_instance variable, don't exit function
  })
  
  redcap_data <- data.frame(
    record_id = "1",
    redcap_repeat_instrument = "pending_queue",
    redcap_repeat_instance = as.character(next_instance),
    pend_name = pending_data$pend_name,
    pend_fac_fell = pending_data$namepend_fac_fell,
    pend_rot = pending_data$pend_rot,
    pending_queue_complete = 2,
    stringsAsFactors = FALSE
  )
  
  cat("Submitting pending faculty data as repeating instrument instance", next_instance, ":\n")
  print(redcap_data)
  
  # Submit to REDCap faculty database
  result <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      data = jsonlite::toJSON(redcap_data, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  
  response_text <- httr::content(result, "text")
  cat("REDCap pending queue response status:", httr::status_code(result), "\n")
  cat("REDCap pending queue response:", response_text, "\n")
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit pending faculty to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
  }
  
  return(response_text)
}


count_monthly_faculty_evals <- function(faculty_eval_data, resident_data, resident_name) {
  tryCatch({
    # Get resident record
    resident_record <- resident_data %>%
      filter(name == resident_name) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      return(0)
    }
    
    # Calculate date 30 days ago
    thirty_days_ago <- Sys.Date() - 30
    
    # Filter evaluations for this resident in the last 30 days
    if (!is.null(faculty_eval_data) && nrow(faculty_eval_data) > 0) {
      monthly_evals <- faculty_eval_data %>%
        filter(
          record_id == resident_record$record_id,
          !is.na(fac_eval_date),
          as.Date(fac_eval_date) >= thirty_days_ago
        )
      
      return(nrow(monthly_evals))
    }
    
    return(0)
  }, error = function(e) {
    cat("Error counting monthly evaluations:", e$message, "\n")
    return(0)
  })
}

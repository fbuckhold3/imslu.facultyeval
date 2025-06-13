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

# Test both fields and field_names to see which works
get_next_faculty_eval_instance <- function(resident_id, token, url) {
  tryCatch({
    cat("=== GETTING NEXT INSTANCE FOR RESIDENT ID:", resident_id, "===\n")
    
    # Try with 'fields' first
    cat("Trying with 'fields' parameter...\n")
    response1 <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = resident_id,
        fields = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    cat("Response with 'fields':", httr::status_code(response1), "\n")
    
    if (httr::status_code(response1) != 200) {
      # Try with 'fieldNames' 
      cat("Trying with 'fieldNames' parameter...\n")
      response2 <- httr::POST(
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
      
      cat("Response with 'fieldNames':", httr::status_code(response2), "\n")
      
      if (httr::status_code(response2) != 200) {
        # If both fail, get all data without field restriction
        cat("Both failed, getting all data for record...\n")
        response_final <- httr::POST(
          url = url,
          body = list(
            token = token,
            content = "record",
            action = "export",
            format = "json",
            type = "flat",
            records = resident_id,
            rawOrLabel = "raw",
            rawOrLabelHeaders = "raw",
            exportCheckboxLabel = "false",
            exportSurveyFields = "false",
            exportDataAccessGroups = "false",
            returnFormat = "json"
          ),
          encode = "form"
        )
        response <- response_final
      } else {
        response <- response2
      }
    } else {
      response <- response1
    }
    
    cat("Final REDCap query response status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) == 200) {
      response_text <- httr::content(response, "text", encoding = "UTF-8")
      all_data <- jsonlite::fromJSON(response_text)
      
      if (is.data.frame(all_data) && nrow(all_data) > 0) {
        cat("Total records returned:", nrow(all_data), "\n")
        
        # Check if we have the repeat instrument columns
        if ("redcap_repeat_instrument" %in% names(all_data) && "redcap_repeat_instance" %in% names(all_data)) {
          
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
          } else {
            cat("No faculty_evaluation instances found in returned data\n")
            
            # Debug: show what repeat instruments we do have
            if (sum(!is.na(all_data$redcap_repeat_instrument)) > 0) {
              unique_instruments <- unique(all_data$redcap_repeat_instrument[!is.na(all_data$redcap_repeat_instrument)])
              cat("Available repeat instruments:", paste(unique_instruments, collapse = ", "), "\n")
            }
          }
          
        } else {
          cat("Missing repeat instrument columns in returned data\n")
          cat("Available columns:", paste(names(all_data), collapse = ", "), "\n")
        }
        
      } else {
        cat("No data returned or data is not a data frame\n")
      }
    } else {
      cat("REDCap query failed with status:", httr::status_code(response), "\n")
      # Get the error message
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      cat("Error details:", error_text, "\n")
    }
    
    # If we get here, no existing instances found for this record_id
    cat("⚠️ No existing faculty evaluation instances found for record_id", resident_id, ", starting with instance 1\n")
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
        format = "json",
        records = "1",
        forms = "pending_queue"
      ),
      encode = "form"
    )
    
    if (httr::status_code(result) == 200) {
      records <- jsonlite::fromJSON(httr::content(result, "text"))
      if (length(records) > 0 && nrow(records) > 0) {
        # Find max instance number for pending_queue
        existing_instances <- records[records$redcap_repeat_instrument == "pending_queue", ]
        if (nrow(existing_instances) > 0) {
          max(as.numeric(existing_instances$redcap_repeat_instance), na.rm = TRUE) + 1
        } else {
          1
        }
      } else {
        1
      }
    } else {
      1
    }
  }, error = function(e) {
    cat("Warning: Could not get existing instances, using instance = 1\n")
    1
  })
  
  redcap_data <- data.frame(
    record_id = "1",
    redcap_repeat_instrument = "pending_queue",
    redcap_repeat_instance = as.character(next_instance),
    pend_name = pending_data$pend_name,
    pend_fac_fell = ifelse(pending_data$namepend_fac_fell == "Attending", "1", "2"),
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
  cat("REDCap response status:", httr::status_code(result), "\n")
  cat("REDCap response:", response_text, "\n")
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit pending faculty to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
  }
  
  return(response_text)
}

# Function to submit new faculty to REDCap
submit_new_faculty_to_redcap <- function(faculty_data, token, url) {
  # Convert faculty data to REDCap format
  redcap_data <- data.frame(
    record_id = paste0("fac_", Sys.time()),
    name = faculty_data$name,
    department = faculty_data$department,
    title = faculty_data$title,
    faculty_roster_complete = 2  # Mark as complete
  )
  
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
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit new faculty to REDCap")
  }
  
  return(httr::content(result, "text"))
}
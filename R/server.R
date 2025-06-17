server <- function(input, output, session) {
  
  # Reactive values to track app state
  values <- reactiveValues(
    current_step = 1,
    resident_info = NULL,
    selected_faculty = NULL,
    eval_step = 1,  # Track sub-steps within evaluation
    eval_type = NULL,  # Track if evaluating attending or fellow
    auto_detected = FALSE,  # Track if faculty type was auto-detected
    current_faculty_results = NULL  # Store current search results
  )
  
  # 🔹 Determine Access Code
  access_code <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$code)) {
      return(trimws(query$code))
    } else {
      return(trimws(input$access_code_input))
    }
  })
  
  # 🔹 Extract Resident Name Using Access Code
  resident_info <- reactive({
    req(access_code())
    if (is.null(resident_data)) return(NULL)
    
    filtered_res <- resident_data %>% 
      filter(access_code == access_code())
    
    if (nrow(filtered_res) == 0) {
      return(NULL)
    }
    
    filtered_res$name[1]
  })
  
  
  
  # Update your faculty_progress_text output in server.R to include monthly count:
  # Replace the existing output$faculty_progress_text with this:
  
  output$faculty_progress_text <- renderUI({
    req(resident_info())
    
    # Get resident level and total count
    resident_record <- resident_data %>%
      filter(name == resident_info()) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      return(div("Resident information not found."))
    }
    
    resident_level <- resident_record$Level
    eval_goal <- get_eval_goal_by_level(resident_level)
    total_fac_evals <- count_resident_faculty_evals(faculty_eval_data, resident_data, resident_info())
    monthly_evals <- count_monthly_faculty_evals(faculty_eval_data, resident_data, resident_info())
    
    div(
      style = "text-align: center; font-size: 16px; font-weight: bold;",
      
      # Total evaluations
      div(
        paste("You have completed", total_fac_evals, "total evaluations"),
        style = "color: var(--ssm-primary-blue); margin-bottom: 0.5rem;"
      ),
      
      # Monthly evaluations  
      div(
        paste("Evaluations in last 30 days:", monthly_evals),
        style = "color: var(--ssm-secondary-blue); font-size: 14px; margin-bottom: 1rem;"
      ),
      
      # Goal information
      div(
        paste("Goal for", resident_level, "level:", eval_goal),
        style = "margin-bottom: 1rem;"
      ),
      
      if (total_fac_evals >= eval_goal) {
        div(style = "color: var(--ssm-success-green); font-weight: bold; margin-top: 10px;", "🎉 Goal Achieved!")
      } else {
        div(style = "margin-top: 10px;", paste("Remaining:", eval_goal - total_fac_evals))
      }
    )
  })
  
  submit_fellow_evaluation <- function() {
    # Get the resident's record_id
    resident_record <- resident_data %>%
      filter(name == values$resident_info) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      stop("Could not find resident record")
    }
    
    resident_id <- resident_record$record_id
    
    # Get next instance number for this resident's faculty evaluations
    next_instance <- get_next_faculty_eval_instance(resident_id, rdm_token, url)
    
    eval_data <- list(
      record_id = resident_id,
      redcap_repeat_instrument = "faculty_evaluation",
      redcap_repeat_instance = as.character(next_instance),
      fac_fell_name = values$selected_faculty$fac_name,
      fac_eval_date = format(Sys.Date(), "%Y-%m-%d"),
      att_or_fell = "2", # Fellow
      fell_eval = input$fell_eval,
      att_rot = input$fell_eval,
      plus = input$plus,
      delta = input$delta,
      faculty_evaluation_complete = "2"
    )
    
    cat("Submitting fellow evaluation:\n")
    print(eval_data)
    
    tryCatch({
      # Submit the evaluation
      submit_evaluation_to_redcap(eval_data, rdm_token, url)
      
      # ALSO submit to pending queue if this is a pending faculty member
      cat("Checking if pending faculty member...\n")
      cat("selected_faculty$source:", if(is.null(values$selected_faculty$source)) "NULL" else values$selected_faculty$source, "\n")
      
      if (!is.null(values$selected_faculty$source) && values$selected_faculty$source == "pending") {
        cat("✅ This is a pending faculty member - submitting to pending queue...\n")
        pending_data <- list(
          pend_name = values$selected_faculty$fac_name,
          namepend_fac_fell = "2",  # Use "2" for Fellow, not "Fellow" text
          pend_rot = input$fell_eval
        )
        
        cat("Pending data to submit:\n")
        print(pending_data)
        
        submit_pending_faculty_to_redcap(pending_data, fac_token, url)
        cat("✅ Pending queue submission completed\n")
      }
      
      values$current_step <- 4
      showNotification("Fellow evaluation submitted successfully!", type = "default")
    }, error = function(e) {
      cat("❌ Error in submit_fellow_evaluation:", e$message, "\n")
      showNotification(paste("Error submitting evaluation:", e$message), type = "error")
    })
  }
  
  output$show_rotation_question <- reactive({
    result <- !is.null(values$eval_type) && values$eval_type == "attending"
    cat("show_rotation_question:", result, "- eval_type:", values$eval_type, "\n")
    return(result)
  })
  outputOptions(output, "show_rotation_question", suspendWhenHidden = FALSE)
  
  # Also add this for showing eval type selection (if not already there):
  output$show_eval_type_selection <- reactive({
    result <- is.null(values$eval_type) || !values$auto_detected
    return(result)
  })
  outputOptions(output, "show_eval_type_selection", suspendWhenHidden = FALSE)
  
  # Step visibility logic
  output$show_step1 <- reactive({ values$current_step == 1 })
  output$show_step2 <- reactive({ values$current_step == 2 })
  output$show_step3 <- reactive({ values$current_step == 3 })
  output$show_step4 <- reactive({ values$current_step == 4 })
  outputOptions(output, "show_step1", suspendWhenHidden = FALSE)
  outputOptions(output, "show_step2", suspendWhenHidden = FALSE)
  outputOptions(output, "show_step3", suspendWhenHidden = FALSE)
  outputOptions(output, "show_step4", suspendWhenHidden = FALSE)
  
  # Evaluation sub-step visibility logic
  output$show_eval_step1 <- reactive({ values$current_step == 3 && values$eval_step == 1 })
  output$show_eval_step2 <- reactive({ values$current_step == 3 && values$eval_step == 2 })
  output$show_eval_step3 <- reactive({ values$current_step == 3 && values$eval_step == 3 })
  outputOptions(output, "show_eval_step1", suspendWhenHidden = FALSE)
  outputOptions(output, "show_eval_step2", suspendWhenHidden = FALSE)
  outputOptions(output, "show_eval_step3", suspendWhenHidden = FALSE)
  
  # Fellow selection visibility - should show when evaluating a fellow (auto-detected or manually selected)
  output$fellow_selected <- reactive({
    result <- !is.null(values$eval_type) && values$eval_type == "fellow"
    cat("fellow_selected:", result, "- eval_type:", values$eval_type, "\n")
    return(result)
  })
  outputOptions(output, "fellow_selected", suspendWhenHidden = FALSE)
  
  # 🔹 Display Resident Name
  output$resident_name <- renderText({
    req(resident_info())
    paste("Welcome:", resident_info())
  })
  
  # Display faculty name in evaluation header
  output$eval_faculty_name <- renderText({
    req(values$selected_faculty)
    paste("Evaluating:", values$selected_faculty$fac_name)
  })
  
  # Handle access code submission
  observeEvent(input$submit_code, {
    if (!is.null(resident_info())) {
      values$current_step <- 2
      values$resident_info <- resident_info()
    } else {
      showNotification("Invalid access code. Please try again.", type = "error")
    }
  })
  
  # 🔹 Faculty Evaluation Progress Text
  output$faculty_progress_text <- renderUI({
    req(resident_info())
    
    # Get resident level and total count
    resident_record <- resident_data %>%
      filter(name == resident_info()) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      return(div("Resident information not found."))
    }
    
    resident_level <- resident_record$Level
    eval_goal <- get_eval_goal_by_level(resident_level)
    total_fac_evals <- count_resident_faculty_evals(faculty_eval_data, resident_data, resident_info())
    monthly_evals <- count_monthly_faculty_evals(faculty_eval_data, resident_data, resident_info())
    
    div(
      style = "text-align: center; font-size: 16px; font-weight: bold;",
      
      # Total evaluations
      div(
        paste("You have completed", total_fac_evals, "total evaluations"),
        style = "color: var(--ssm-primary-blue); margin-bottom: 0.5rem;"
      ),
      
      # Monthly evaluations counter
      div(
        paste("📅 Evaluations in last 30 days:", monthly_evals),
        style = "color: var(--ssm-secondary-blue); font-size: 14px; margin-bottom: 1rem; padding: 0.5rem; background: rgba(0, 102, 161, 0.1); border-radius: 8px;"
      ),
      
      # Goal information
      div(
        paste("🎯 Goal for", resident_level, "level:", eval_goal),
        style = "margin-bottom: 1rem;"
      ),
      
      if (total_fac_evals >= eval_goal) {
        div(style = "color: var(--ssm-success-green); font-weight: bold; margin-top: 10px;", "🎉 Goal Achieved!")
      } else {
        div(style = "margin-top: 10px;", paste("📊 Remaining:", eval_goal - total_fac_evals))
      }
    )
  })
  
  # 🔹 Faculty Evaluation Progress Plot (using your preferred style)
  output$fac_eval_progress <- renderPlot({
    req(resident_info())
    
    # Get resident level and goal
    resident_record <- resident_data %>%
      filter(name == resident_info()) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      return(ggplot() + labs(title = "Resident information not found"))
    }
    
    resident_level <- resident_record$Level
    eval_goal <- get_eval_goal_by_level(resident_level)
    eval_count <- count_resident_faculty_evals(faculty_eval_data, resident_data, resident_info())
    
    # Create data in the format you used (with rotation-like categories)
    progress_data <- data.frame(
      Category = c("Completed", "Goal"),
      Count = c(eval_count, eval_goal)
    )
    
    ggplot(progress_data, aes(x = reorder(Category, -Count), y = Count, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette = "Set3") +  # Set3 allows more than 8 colors
      theme_minimal() +
      labs(title = "Faculty Evaluations Progress", x = "", y = "Count") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none")
  })
  
  # Faculty search functionality
  filtered_faculty <- reactive({
    req(input$faculty_search)
    req(faculty_data)
    
    search_term <- tolower(trimws(input$faculty_search))
    if (nchar(search_term) < 2) return(NULL)
    
    # Filter based on fac_name column
    filtered <- faculty_data %>%
      filter(!is.na(fac_name) & grepl(search_term, tolower(fac_name), fixed = TRUE)) %>%
      head(10)  # Limit results
    
    cat("Faculty search for '", search_term, "' returned ", nrow(filtered), " results\n")
    if (nrow(filtered) > 0) {
      cat("First result columns:", paste(names(filtered), collapse = ", "), "\n")
      cat("First result fac_name:", filtered$fac_name[1], "\n")
    }
    
    return(filtered)
  })
  
  # Render faculty search results
  output$faculty_search_results <- renderUI({
    faculty_results <- filtered_faculty()
    
    if (is.null(faculty_results) || nrow(faculty_results) == 0) {
      if (!is.null(input$faculty_search) && nchar(trimws(input$faculty_search)) >= 2) {
        return(div(style = "padding: 10px; color: #666;", 
                   "No faculty found matching your search. Please check the spelling or add them to the pending queue below."))
      } else {
        return(div(style = "padding: 10px; color: #666;", 
                   "Start typing to search for faculty members..."))
      }
    }
    
    # Store the search results for later use
    values$current_faculty_results <- faculty_results
    
    result_list <- lapply(1:nrow(faculty_results), function(i) {
      faculty <- faculty_results[i, ]
      
      # Create a unique button ID for each faculty member
      button_id <- paste0("select_faculty_", i)
      
      # Build the display text
      faculty_text <- faculty$fac_name
      if (!is.na(faculty$fac_div)) {
        faculty_text <- paste0(faculty_text, " - ", faculty$fac_div)
      }
      if (!is.na(faculty$fac_fell)) {
        type_text <- if(faculty$fac_fell == 1) "Faculty/Attending" else if(faculty$fac_fell == 2) "Fellow" else paste("Type:", faculty$fac_fell)
        faculty_text <- paste0(faculty_text, " (", type_text, ")")
      }
      
      div(class = "search-result", style = "padding: 10px; border-bottom: 1px solid #eee; margin: 5px 0;",
          actionButton(button_id, 
                       label = faculty_text,
                       style = "background: #f8f9fa; border: 1px solid #ddd; text-align: left; width: 100%; padding: 10px;",
                       class = "btn btn-light")
      )
    })
    
    do.call(tagList, result_list)
  })
  
  # Handle individual faculty button clicks
  observe({
    faculty_results <- values$current_faculty_results
    if (!is.null(faculty_results)) {
      for (i in 1:nrow(faculty_results)) {
        local({
          faculty_index <- i
          button_id <- paste0("select_faculty_", faculty_index)
          
          observeEvent(input[[button_id]], {
            cat("=== FACULTY BUTTON CLICKED ===\n")
            cat("Button ID:", button_id, "Index:", faculty_index, "\n")
            
            # Trigger the same logic as the original select_faculty event
            if (!is.null(faculty_results) && faculty_index <= nrow(faculty_results)) {
              values$selected_faculty <- faculty_results[faculty_index, ]
              values$current_step <- 3
              values$eval_step <- 1  # Start with first evaluation step
              
              # Debug: Show ALL faculty data for this selection
              cat("=== FACULTY SELECTION DEBUG ===\n")
              cat("Selected faculty name:", values$selected_faculty$fac_name, "\n")
              cat("Number of columns:", ncol(values$selected_faculty), "\n")
              cat("Available faculty columns:\n")
              for(col_name in names(values$selected_faculty)) {
                col_value <- values$selected_faculty[[col_name]]
                cat("  ", col_name, ":", if(is.na(col_value)) "NA" else col_value, "\n")
              }
              cat("================================\n")
              
              # Auto-detect faculty vs fellow from faculty database
              faculty_type_detected <- FALSE
              
              # Check for fac_fell field
              if ("fac_fell" %in% names(values$selected_faculty)) {
                fac_fell_value <- values$selected_faculty$fac_fell
                cat("fac_fell value found:", fac_fell_value, "\n")
                
                if (!is.na(fac_fell_value)) {
                  # Handle both numeric codes (1, 2) and text values ("Faculty", "Fellow", "Attending")
                  fac_fell_lower <- tolower(as.character(fac_fell_value))
                  
                  if (fac_fell_value == 1 || fac_fell_lower %in% c("faculty", "attending")) {
                    values$eval_type <- "attending"
                    values$auto_detected <- TRUE
                    faculty_type_detected <- TRUE
                    cat("Auto-detected from fac_fell: Attending (value was:", fac_fell_value, ")\n")
                  } else if (fac_fell_value == 2 || fac_fell_lower == "fellow") {
                    values$eval_type <- "fellow"
                    values$auto_detected <- TRUE
                    faculty_type_detected <- TRUE
                    cat("Auto-detected from fac_fell: Fellow (value was:", fac_fell_value, ")\n")
                  } else {
                    cat("fac_fell value '", fac_fell_value, "' not recognized as Faculty/Attending (1) or Fellow (2)\n")
                  }
                }
              } else {
                cat("fac_fell field NOT FOUND\n")
              }
              
              # If no auto-detection possible, user will need to select manually
              if (!faculty_type_detected) {
                values$eval_type <- NULL
                values$auto_detected <- FALSE
                cat("No faculty type auto-detection possible - user must select manually\n")
              }
              
              cat("Final result - eval_type:", values$eval_type, "auto_detected:", values$auto_detected, "\n")
            }
          })
        })
      }
    }
  })
  
  # Handle faculty selection
  observeEvent(input$select_faculty, {
    faculty_results <- filtered_faculty()
    if (!is.null(faculty_results) && input$select_faculty <= nrow(faculty_results)) {
      values$selected_faculty <- faculty_results[input$select_faculty, ]
      values$current_step <- 3
      values$eval_step <- 1  # Start with first evaluation step
      values$eval_type <- NULL  # Reset evaluation type
    }
  })
  
  # Display selected faculty info
  output$selected_faculty_info <- renderUI({
    if (!is.null(values$selected_faculty)) {
      faculty <- values$selected_faculty
      div(
        strong(faculty$fac_name),
        br(),
        if (!is.na(faculty$fac_div)) {
          paste("Division:", faculty$fac_div)
        } else {
          "Faculty member"
        },
        br(), br(),
        actionButton("proceed_to_eval", "Proceed to Evaluation", class = "btn-primary")
      )
    } else {
      div("Select a faculty member from the search results to proceed with the evaluation.")
    }
  })
  
  # Handle attending/fellow selection
  observeEvent(input$select_attending, {
    values$eval_type <- "attending"
  })
  
  observeEvent(input$select_fellow, {
    values$eval_type <- "fellow"
  })
  
  # Display evaluation type selection
  output$eval_type_selected <- renderText({
    if (!is.null(values$eval_type)) {
      paste("Selected:", if(values$eval_type == "attending") "Attending" else "Fellow")
    }
  })
  
  # Handle continue to detailed questions
  observeEvent(input$continue_eval, {
    # Debug: Show current state when continue button is clicked
    cat("=== CONTINUE BUTTON CLICKED ===\n")
    cat("eval_type:", values$eval_type, "\n")
    cat("auto_detected:", values$auto_detected, "\n")
    cat("att_rot:", input$att_rot, "\n")  # FIXED: Changed from rotation to att_rot
    cat("plus length:", if(is.null(input$plus)) 0 else nchar(trimws(input$plus)), "\n")
    cat("delta length:", if(is.null(input$delta)) 0 else nchar(trimws(input$delta)), "\n")
    
    # Validate that evaluation type is set (either auto-detected or manually selected)
    if (is.null(values$eval_type)) {
      cat("ERROR: eval_type is null\n")
      showNotification("Please select whether you're evaluating an attending or fellow.", type = "warning")
      return()
    }
    
    # ONLY validate rotation for attendings, NOT for fellows
    if (values$eval_type == "attending") {
      cat("Validating rotation for attending...\n")
      if (is.null(input$att_rot) || input$att_rot == "") {
        cat("ERROR: rotation not selected for attending\n")
        showNotification("Please select the rotation on which you worked with this person.", type = "warning")
        return()
      }
    } else {
      cat("Skipping rotation validation for fellow\n")
    }
    
    if (is.null(input$plus) || trimws(input$plus) == "") {
      cat("ERROR: plus feedback empty\n")
      showNotification("Please provide feedback on what went well.", type = "warning")
      return()
    }
    
    if (is.null(input$delta) || trimws(input$delta) == "") {
      cat("ERROR: delta feedback empty\n")
      showNotification("Please provide feedback on what could be improved.", type = "warning")
      return()
    }
    
    # If evaluating fellow, check that fellow type is selected
    if (values$eval_type == "fellow" && (is.null(input$fell_eval) || input$fell_eval == "")) {
      cat("ERROR: fellow type not selected\n")
      showNotification("Please select the fellow evaluation type.", type = "warning")
      return()
    }
    
    # All validation passed
    cat("All validations passed - proceeding\n")
    
    # Move to next step (only for attendings, fellows end here)
    if (values$eval_type == "attending") {
      cat("Moving to attending evaluation step 2\n")
      values$eval_step <- 2
    } else {
      cat("Submitting fellow evaluation\n")
      # For fellows, submit directly
      submit_fellow_evaluation()
    }
  })
  
  # Back to feedback step
  observeEvent(input$back_to_feedback, {
    values$eval_step <- 1
  })
  
  # Continue to page 3 for attendings
  observeEvent(input$continue_to_page3, {
    # Validate page 2 questions based on rotation type
    if (is_short_rotation()) {
      required_page2 <- c("approachability", "ques_clin_des", "feedback")
    } else {
      required_page2 <- c("approachability", "respect", "bedside_manner", "time_teaching", 
                          "ques_clin_des", "autonomy", "feedback", "organ")
    }
    
    missing_ratings <- sapply(required_page2, function(rating) {
      is.null(input[[rating]]) || input[[rating]] == ""
    })
    
    if (any(missing_ratings)) {
      missing_names <- required_page2[missing_ratings]
      showNotification(paste("Please answer all questions on this page. Missing:", paste(missing_names, collapse = ", ")), 
                       type = "warning")
      return()
    }
    
    values$eval_step <- 3
  })
  
  # Back to page 2
  observeEvent(input$back_to_page2, {
    values$eval_step <- 2
  })
  
  # Helper function to determine if this is a short rotation
  is_short_rotation <- reactive({
    req(values$selected_faculty)
    
    # Debug: Check what columns are available
    cat("Available faculty columns:", paste(names(values$selected_faculty), collapse = ", "), "\n")
    
    # att_rot is typically stored in the evaluation form, not the faculty roster
    # For now, we'll need to determine short rotations by another method
    # or add rotation type to the faculty roster
    
    # Option 1: Check if there's a rotation field in faculty data
    if ("fac_rotation" %in% names(values$selected_faculty)) {
      rotation <- values$selected_faculty$fac_rotation
      cat("fac_rotation value:", rotation, "\n")
      
      if (!is.na(rotation)) {
        result <- rotation %in% c(4, 5, 6, 11)
        cat("Is short rotation:", result, "\n")
        return(result)
      }
    }
    
    # Option 2: Check by division or department for short rotations
    # You might need to customize this based on your data
    if ("fac_div" %in% names(values$selected_faculty)) {
      division <- values$selected_faculty$fac_div
      cat("Faculty division:", division, "\n")
      
      # Add specific divisions that should use short evaluations
      # Customize these based on your needs
      short_divisions <- c("Ambulatory", "Outpatient", "Clinic", "Consultation")
      
      if (!is.na(division)) {
        result <- tolower(division) %in% tolower(short_divisions)
        cat("Is short rotation based on division:", result, "\n")
        return(result)
      }
    }
    
    # Option 3: For now, default to full evaluation for all faculty
    # This ensures the app works while we figure out the rotation logic
    cat("Defaulting to full evaluation - no rotation criteria met\n")
    return(FALSE)
  })
  
  # Generate detailed questions page 2 for attendings
  output$detailed_questions_step2 <- renderUI({
    req(values$eval_type == "attending")
    req(values$eval_step == 2)
    
    # Page 2 content depends on rotation type
    if (is_short_rotation()) {
      # Short rotation questions
      tagList(
        tags$head(
          tags$style("
            .rating-container { margin: 20px 0; }
            .rating-buttons { display: flex; gap: 10px; justify-content: center; margin: 10px 0; }
            .rating-btn { 
              padding: 8px 16px; 
              border: 2px solid #ddd; 
              background: #f8f9fa; 
              border-radius: 20px; 
              cursor: pointer;
              transition: all 0.3s;
              font-size: 16px;
            }
            .rating-btn:hover { 
              background: #007bff; 
              color: white; 
              border-color: #007bff;
            }
            .rating-btn.selected { 
              background: #28a745; 
              color: white; 
              border-color: #28a745;
            }
            .rating-scale {
              margin: 25px 0 !important;
            }
            .question-text {
              font-size: 18px !important;
              font-weight: 600;
              color: #2c3e50;
              margin-bottom: 15px;
            }
            .scale-labels {
              font-size: 14px !important;
              color: #6c757d;
            }
          "),
          tags$script(rating_js)
        ),
        
        h4("Teaching Evaluation - Part 1", style = "margin-bottom: 30px;"),
        
        create_rating_question("approachability", 
                               "How approachable is the attending to address questions or updates?",
                               "Difficult to approach/intimidating", 
                               "Easy to approach without fear"),
        
        create_rating_question("ques_clin_des", 
                               "Does the attending ask questions about your clinical decisions?",
                               "Never posed questions", 
                               "Always posed questions"),
        
        create_rating_question("feedback", 
                               "The attending provided me with feedback that allowed me to improve my performance",
                               "Never received feedback", 
                               "Received actionable feedback"),
        
        div(style = "text-align: center; margin-top: 30px;",
            actionButton("back_to_feedback", "Back", class = "btn-secondary btn-modern", style = "margin-right: 15px;"),
            actionButton("continue_to_page3", "Continue", class = "btn-primary btn-modern")
        )
      )
    } else {
      # Full rotation questions
      tagList(
        tags$head(
          tags$style("
            .rating-container { margin: 20px 0; }
            .rating-buttons { display: flex; gap: 10px; justify-content: center; margin: 10px 0; }
            .rating-btn { 
              padding: 8px 16px; 
              border: 2px solid #ddd; 
              background: #f8f9fa; 
              border-radius: 20px; 
              cursor: pointer;
              transition: all 0.3s;
              font-size: 16px;
            }
            .rating-btn:hover { 
              background: #007bff; 
              color: white; 
              border-color: #007bff;
            }
            .rating-btn.selected { 
              background: #28a745; 
              color: white; 
              border-color: #28a745;
            }
            .rating-scale {
              margin: 25px 0 !important;
            }
            .question-text {
              font-size: 18px !important;
              font-weight: 600;
              color: #2c3e50;
              margin-bottom: 15px;
            }
            .scale-labels {
              font-size: 14px !important;
              color: #6c757d;
            }
          "),
          tags$script(rating_js)
        ),
        
        h4("Teaching Evaluation - Part 1", style = "margin-bottom: 30px;"),
        
        create_rating_question("approachability", 
                               "How approachable is the attending to address questions or updates?",
                               "Difficult to approach/intimidating", 
                               "Easy to approach without fear"),
        
        create_rating_question("respect", 
                               "Is the attending respectful of all members of the healthcare team?",
                               "Did not demonstrate respect", 
                               "Consistently respectful"),
        
        create_rating_question("bedside_manner", 
                               "Does the attending role model a good bedside manner?",
                               "Does not role model", 
                               "Consistently role models"),
        
        create_rating_question("time_teaching", 
                               "Does the attending ensure time for teaching?",
                               "Never ensures time", 
                               "Always ensures time"),
        
        create_rating_question("ques_clin_des", 
                               "Does the attending ask questions about your clinical decisions?",
                               "Never posed questions", 
                               "Always posed questions"),
        
        create_rating_question("autonomy", 
                               "The attending allowed me autonomy in my clinical decision making",
                               "Never allowed decisions", 
                               "Consistently allowed decisions"),
        
        create_rating_question("feedback", 
                               "The attending provided me with feedback that allowed me to improve my performance",
                               "Never received feedback", 
                               "Received actionable feedback"),
        
        create_rating_question("organ", 
                               "The clinical session(s) I had with the attending were conducted in an organized fashion?",
                               "Consistently disorganized", 
                               "Consistently organized"),
        
        div(style = "text-align: center; margin-top: 30px;",
            actionButton("back_to_feedback", "Back", class = "btn-secondary btn-modern", style = "margin-right: 15px;"),
            actionButton("continue_to_page3", "Continue", class = "btn-primary btn-modern")
        )
      )
    }
  })
  
  # Generate detailed questions page 3 for attendings
  output$detailed_questions_step3 <- renderUI({
    req(values$eval_type == "attending")
    req(values$eval_step == 3)
    
    # Page 3 - Final questions
    tagList(
      h4("Teaching Evaluation - Part 2", style = "margin-bottom: 30px;"),
      
      if (is_short_rotation()) {
        # Short rotation final questions
        tagList(
          create_select_question("att_give_feed", 
                                 "How did the attending give you feedback outside of clinical teaching rounds?",
                                 list("Choose..." = "",
                                      "Verbal feedback only" = "1",
                                      "Written feedback only" = "2",
                                      "Both oral and written" = "3",
                                      "I did not receive any feedback" = "4")),
          
          create_select_question("eval_done", 
                                 "Do you know if the attending completed an evaluation for you?",
                                 list("Choose..." = "",
                                      "Yes" = "1",
                                      "No" = "2",
                                      "I don't know" = "3")),
          
          create_rating_question("att_overall", 
                                 "How would you rate the overall teaching of this attending?",
                                 "Needs improvement", 
                                 "Outstanding")
        )
      } else {
        # Full rotation final questions
        tagList(
          create_select_question("att_ext_tea", 
                                 "The attending gave clinical teaching or other educational effort outside of teaching rounds",
                                 list("Choose..." = "",
                                      "Strongly agree" = "1",
                                      "Agree" = "2", 
                                      "Neither agree nor disagree" = "3",
                                      "Disagree" = "4",
                                      "Strongly disagree" = "5")),
          
          create_select_question("att_give_feed", 
                                 "How did the attending give you feedback outside of clinical teaching rounds?",
                                 list("Choose..." = "",
                                      "Verbal feedback only" = "1",
                                      "Written feedback only" = "2",
                                      "Both oral and written" = "3",
                                      "I did not receive any feedback" = "4")),
          
          create_select_question("eval_done", 
                                 "Do you know if the attending completed an evaluation for you?",
                                 list("Choose..." = "",
                                      "Yes" = "1",
                                      "No" = "2",
                                      "I don't know" = "3")),
          
          create_rating_question("att_overall", 
                                 "How would you rate the overall teaching of this attending?",
                                 "Needs improvement", 
                                 "Outstanding")
        )
      },
      
      div(style = "text-align: center; margin-top: 30px;",
          actionButton("back_to_page2", "Back", class = "btn-secondary btn-modern", style = "margin-right: 15px;"),
          actionButton("submit_evaluation", "Submit Evaluation", class = "btn-success btn-modern", style = "font-size: 16px;")
      )
    )
  })
  
  # Function to create select questions with larger text
  create_select_question <- function(input_id, question_text, choices) {
    div(style = "margin-bottom: 25px;",
        h5(question_text, class = "question-text", style = "font-size: 18px !important;"),
        selectInput(input_id, "", 
                    choices = choices,
                    selected = "")
    )
  }
  

  # Handle pending faculty addition
  observeEvent(input$add_pending_faculty, {
    req(input$pend_name)
    req(input$namepend_fac_fell)
    req(input$pend_rot)
    
    # Prepare pending faculty data
    pending_data <- list(
      resident_name = values$resident_info,
      pend_name = input$pend_name,
      namepend_fac_fell = if(input$namepend_fac_fell == "Fellow") "2" else "1",  # ✅ Convert text to number
      pend_rot = input$pend_rot,
      submission_date = Sys.Date()
    )
    
    # Submit to REDCap pending queue
    tryCatch({
      submit_pending_faculty_to_redcap(pending_data, fac_token, url)
      showNotification(
        paste("Faculty member", input$pend_name, "has been added to the pending queue for review."), 
        type = "default"
      )
      
      # Create a mock faculty object for the pending person to start evaluation
      pending_faculty <- data.frame(
        fac_name = input$pend_name,
        fac_div = "Pending Review",
        fac_fell = if(input$namepend_fac_fell == "1") "Faculty" else "Fellow",
        source = "pending",
        stringsAsFactors = FALSE
      )
      
      # Set this as the selected faculty and start evaluation
      values$selected_faculty <- pending_faculty
      values$current_step <- 3
      values$eval_step <- 1
      
      # Auto-detect faculty type from pending queue selection
      cat("=== PENDING FACULTY AUTO-DETECTION DEBUG ===\n")
      cat("input$namepend_fac_fell:", '"', input$namepend_fac_fell, '"', "\n")
      cat("Class:", class(input$namepend_fac_fell), "\n")
      cat("Length:", length(input$namepend_fac_fell), "\n")
      cat("Is NA:", is.na(input$namepend_fac_fell), "\n")
      
      # Handle both text values and numeric codes
      pend_type <- as.character(input$namepend_fac_fell)
      cat("Converted to character:", '"', pend_type, '"', "\n")
      
      if (pend_type %in% c("Attending", "1")) {
        values$eval_type <- "attending"
        values$auto_detected <- TRUE
        cat("✅ Auto-detected from pending queue: Attending (value was:", pend_type, ")\n")
      } else if (pend_type %in% c("Fellow", "2")) {
        values$eval_type <- "fellow"
        values$auto_detected <- TRUE
        cat("✅ Auto-detected from pending queue: Fellow (value was:", pend_type, ")\n")
      } else {
        values$eval_type <- NULL
        values$auto_detected <- FALSE
        cat("❌ Pending queue - manual selection required. Value was:", '"', pend_type, '"', "\n")
        cat("Expected values: 'Attending', '1', 'Fellow', '2'\n")
      }
      cat("============================================\n")
      
      cat("Pending faculty evaluation started - eval_type:", values$eval_type, "auto_detected:", values$auto_detected, "\n")
      
      # Reset the form
      updateTextInput(session, "pend_name", value = "")
      updateSelectInput(session, "namepend_fac_fell", selected = "")
      updateTextInput(session, "pend_rot", value = "")
      updateCheckboxInput(session, "faculty_not_found", value = FALSE)
      
    }, error = function(e) {
      showNotification(paste("Error submitting to pending queue:", e$message), type = "error")
    })
  })
  
  # Handle attending evaluation submission
  observeEvent(input$submit_evaluation, {
    # Validate all rating questions are answered based on rotation type
    if (is_short_rotation()) {
      required_ratings <- c("approachability", "ques_clin_des", "feedback", "att_overall")
      required_selects <- c("att_give_feed", "eval_done")
    } else {
      required_ratings <- c("approachability", "respect", "bedside_manner", "time_teaching", 
                            "ques_clin_des", "autonomy", "feedback", "organ", "att_overall")
      required_selects <- c("att_ext_tea", "att_give_feed", "eval_done")
    }
    
    missing_ratings <- sapply(required_ratings, function(rating) {
      is.null(input[[rating]]) || input[[rating]] == ""
    })
    
    missing_selects <- sapply(required_selects, function(select) {
      is.null(input[[select]]) || input[[select]] == ""
    })
    
    if (any(missing_ratings) || any(missing_selects)) {
      missing_items <- c(required_ratings[missing_ratings], required_selects[missing_selects])
      showNotification(paste("Please answer all questions. Missing:", paste(missing_items, collapse = ", ")), 
                       type = "warning")
      return()
    }
    
    # Get the resident's record_id
    resident_record <- resident_data %>%
      filter(name == values$resident_info) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      showNotification("Could not find resident record", type = "error")
      return()
    }
    
    resident_id <- resident_record$record_id
    
    # Get next instance number for this resident's faculty evaluations
    # Pass the token and url parameters to the function
    next_instance <- get_next_faculty_eval_instance(resident_id, rdm_token, url)
    
    # Prepare base evaluation data
    eval_data <- list(
      record_id = resident_id,
      redcap_repeat_instrument = "faculty_evaluation",
      redcap_repeat_instance = as.character(next_instance),
      fac_fell_name = values$selected_faculty$fac_name,  # Add faculty name
      fac_eval_date = format(Sys.Date(), "%Y-%m-%d"),    # Add current date in Y-M-D format (REDCap requirement)
      att_rot = input$att_rot,  # Changed from rotation to att_rot
      att_or_fell = "1", # Attending
      plus = input$plus,
      delta = input$delta,
      approachability = input$approachability,
      ques_clin_des = input$ques_clin_des,
      feedback = input$feedback,
      att_give_feed = input$att_give_feed,
      eval_done = input$eval_done,
      att_overall = input$att_overall,
      faculty_evaluation_complete = "2" # Complete
    )
    
    # Add additional fields for full rotations
    if (!is_short_rotation()) {
      eval_data$respect <- input$respect
      eval_data$bedside_manner <- input$bedside_manner
      eval_data$time_teaching <- input$time_teaching
      eval_data$autonomy <- input$autonomy
      eval_data$organ <- input$organ
      eval_data$att_ext_tea <- input$att_ext_tea
    }
    
    cat("Submitting attending evaluation:\n")
    print(eval_data)
    
    # Submit to REDCap
    tryCatch({
      submit_evaluation_to_redcap(eval_data, rdm_token, url)
      values$current_step <- 4
      showNotification("Attending evaluation submitted successfully!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error submitting evaluation:", e$message), type = "error")
    })
  })
  
  
  # Remove the old get_next_faculty_eval_instance function from server.R since we moved it to the functions file
  
  # Add Return to Start functionality for all steps
  observeEvent(input$return_to_start_step2, {
    # Reset all reactive values to initial state
    values$current_step <- 1
    values$resident_info <- NULL
    values$selected_faculty <- NULL
    values$eval_step <- 1
    values$eval_type <- NULL
    values$auto_detected <- FALSE
    values$current_faculty_results <- NULL
    
    # Clear all form inputs
    updateTextInput(session, "access_code_input", value = "")
    updateTextInput(session, "faculty_search", value = "")
    updateTextInput(session, "pend_name", value = "")
    updateSelectInput(session, "namepend_fac_fell", selected = "")
    updateTextInput(session, "pend_rot", value = "")
    updateCheckboxInput(session, "faculty_not_found", value = FALSE)
  })
  
  observeEvent(input$return_to_start_step3, {
    # Reset all reactive values to initial state
    values$current_step <- 1
    values$resident_info <- NULL
    values$selected_faculty <- NULL
    values$eval_step <- 1
    values$eval_type <- NULL
    values$auto_detected <- FALSE
    values$current_faculty_results <- NULL
    
    # Clear all form inputs including evaluation form fields
    updateTextInput(session, "access_code_input", value = "")
    updateTextInput(session, "faculty_search", value = "")
    updateSelectInput(session, "att_rot", selected = "")
    # REMOVED: updateTextInput(session, "other_rot", value = "")  # This field no longer exists
    updateTextAreaInput(session, "plus", value = "")
    updateTextAreaInput(session, "delta", value = "")
    updateSelectInput(session, "fell_eval", selected = "")
    
    # Clear rating inputs if they exist
    rating_fields <- c("approachability", "respect", "bedside_manner", "time_teaching", 
                       "ques_clin_des", "autonomy", "feedback", "organ", "att_overall")
    for(field in rating_fields) {
      if(!is.null(input[[field]])) {
        session$sendCustomMessage("clearRating", field)
      }
    }
    
    # Clear select inputs
    select_fields <- c("att_ext_tea", "att_give_feed", "eval_done")
    for(field in select_fields) {
      updateSelectInput(session, field, selected = "")
    }
  })
  
  observeEvent(input$return_to_start_step4, {
    # Reset all reactive values to initial state
    values$current_step <- 1
    values$resident_info <- NULL
    values$selected_faculty <- NULL
    values$eval_step <- 1
    values$eval_type <- NULL
    values$auto_detected <- FALSE
    values$current_faculty_results <- NULL
    
    # Clear all form inputs
    updateTextInput(session, "access_code_input", value = "")
    updateTextInput(session, "faculty_search", value = "")
  })
  
  # Navigation buttons
  observeEvent(input$back_to_search, {
    values$current_step <- 2
    values$selected_faculty <- NULL
    values$eval_step <- 1
    values$eval_type <- NULL
  })
  
  observeEvent(input$evaluate_another, {
    values$current_step <- 2
    values$selected_faculty <- NULL
    values$eval_step <- 1
    values$eval_type <- NULL
    updateTextInput(session, "faculty_search", value = "")
  })
  
  observeEvent(input$return_to_dashboard, {
    # Redirect to main dashboard or close app
    session$reload()
  })
}
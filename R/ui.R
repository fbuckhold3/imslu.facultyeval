# ui.R
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  # Enhanced CSS for better styling
  tags$head(
    tags$style("
      .faculty-card {
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        background-color: #f8f9fa;
      }
      .step-header {
        background-color: #007bff;
        color: white;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 20px;
        position: relative;
      }
      .return-btn {
        position: absolute;
        right: 15px;
        top: 50%;
        transform: translateY(-50%);
      }
      .search-result {
        cursor: pointer;
        padding: 10px;
        border-bottom: 1px solid #eee;
      }
      .search-result:hover {
        background-color: #f0f0f0;
      }
      .progress-section {
        background-color: #e9ecef;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      .evaluation-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 30px;
        text-align: center;
        position: relative;
      }
      .eval-step-card {
        background: white;
        border-radius: 15px;
        padding: 30px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      .rating-scale {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin: 15px 0;
        padding: 20px;
        background: #f8f9fa;
        border-radius: 10px;
        border: 2px solid transparent;
        transition: all 0.3s ease;
      }
      .rating-scale:hover {
        border-color: #007bff;
        box-shadow: 0 2px 8px rgba(0,123,255,0.2);
      }
      .rating-option {
        cursor: pointer;
        padding: 12px 20px;
        border-radius: 25px;
        background: #e9ecef;
        border: 2px solid transparent;
        transition: all 0.3s ease;
        font-weight: 500;
        min-width: 80px;
        text-align: center;
      }
      .rating-option:hover {
        background: #007bff;
        color: white;
        transform: translateY(-2px);
      }
      .rating-option.selected {
        background: #28a745;
        color: white;
        border-color: #1e7e34;
      }
      .question-text {
        font-size: 16px;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 10px;
      }
      .scale-labels {
        display: flex;
        justify-content: space-between;
        font-size: 12px;
        color: #6c757d;
        margin-top: 5px;
      }
      .btn-modern {
        padding: 12px 30px;
        border-radius: 25px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        transition: all 0.3s ease;
      }
      .btn-modern:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.2);
      }
      .btn-return {
        padding: 8px 20px;
        font-size: 12px;
        border-radius: 20px;
        font-weight: 500;
      }
      textarea {
        border-radius: 10px !important;
        border: 2px solid #e9ecef !important;
        padding: 15px !important;
        font-size: 14px !important;
      }
      textarea:focus {
        border-color: #007bff !important;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25) !important;
      }
      .eval-type-selected {
        margin-top: 10px;
        font-weight: bold;
        color: #28a745;
      }
    ")
  ),
  
  titlePanel("Faculty Evaluation System"),
  
  # Step 1: Access Code Entry
  conditionalPanel(
    condition = "output.show_step1",
    div(class = "step-header",
        h3("Step 1: Enter Your Access Code")
    ),
    fluidRow(
      column(6,
             textInput("access_code_input", 
                       "Access Code:", 
                       placeholder = "Enter your unique access code"),
             actionButton("submit_code", "Submit", class = "btn-primary")
      ),
      column(6,
             div(id = "resident_info_display",
                 textOutput("resident_name")
             )
      )
    )
  ),
  
  # Step 2: Faculty Selection with Progress
  conditionalPanel(
    condition = "output.show_step2",
    div(class = "step-header",
        h3("Step 2: Select Faculty to Evaluate"),
        actionButton("return_to_start_step2", "Return to Start", 
                     class = "btn-outline-light btn-return return-btn")
    ),
    
    # Progress Section
    fluidRow(
      column(12,
             div(class = "progress-section",
                 h4("Your Faculty Evaluation Progress"),
                 uiOutput("faculty_progress_text"),
                 br(),
                 plotOutput("fac_eval_progress", height = "200px")
             )
      )
    ),
    
    fluidRow(
      column(8,
             textInput("faculty_search", 
                       "Search Faculty:", 
                       placeholder = "Start typing faculty name..."),
             div(id = "search_results",
                 uiOutput("faculty_search_results")
             ),
             br(),
             div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                 p(strong("Can't find the faculty member you're looking for?")),
                 p("If the faculty member doesn't appear in the search results above, you can add them to our pending queue for review."),
                 checkboxInput("faculty_not_found", 
                               "Add new faculty member to pending queue", 
                               value = FALSE)
             )
      ),
      column(4,
             div(class = "faculty-card",
                 h4("Selected Faculty:"),
                 uiOutput("selected_faculty_info")
             )
      )
    ),
    br(),
    conditionalPanel(
      condition = "input.faculty_not_found == true",
      fluidRow(
        column(12,
               div(class = "faculty-card",
                   h4("Add New Faculty to Pending Queue"),
                   p("Please provide the following information about the faculty member you'd like to evaluate:"),
                   textInput("pend_name", 
                             "Faculty Name:", 
                             placeholder = "Enter full name of faculty member"),
                   selectInput("namepend_fac_fell", 
                               "Faculty Type:",
                               choices = list("Choose..." = "", "Attending" = "Attending", "Fellow" = "Fellow"),
                               selected = ""),
                   textInput("pend_rot", 
                             "Where did you work with this person?", 
                             placeholder = "e.g., Bronze, Bridge, VA, Diamond, etc."),
                   br(),
                   actionButton("add_pending_faculty", "Add to Pending Queue", class = "btn-warning"),
                   p(style = "margin-top: 10px; font-size: 12px; color: #666;",
                     "Note: Faculty added to the pending queue will be reviewed and added to our system before you can complete the evaluation.")
               )
        )
      )
    )
  ),
  
  # Step 3: Evaluation Form - New Full Screen Design
  conditionalPanel(
    condition = "output.show_step3",
    div(class = "evaluation-header",
        h2(textOutput("eval_faculty_name", inline = TRUE)),
        p("Faculty Evaluation", style = "font-size: 18px; margin-bottom: 0;"),
        actionButton("return_to_start_step3", "Return to Start", 
                     class = "btn-outline-light btn-return return-btn")
    ),
    
    # Sub-steps within evaluation
    conditionalPanel(
      condition = "output.show_eval_step1", # Plus/Delta Step
      div(class = "eval-step-card",
          h3("Step 1: General Feedback", style = "color: #2c3e50; margin-bottom: 25px;"),
          
          div(style = "margin-bottom: 30px;",
              conditionalPanel(
                condition = "output.show_eval_type_selection",
                h5("Are you evaluating an attending or fellow?", class = "question-text"),
                div(style = "display: flex; gap: 20px; margin-top: 15px;",
                    actionButton("select_attending", "Attending", class = "btn-outline-primary btn-modern"),
                    actionButton("select_fellow", "Fellow", class = "btn-outline-success btn-modern")
                )
              ),
              div(class = "eval-type-selected",
                  textOutput("eval_type_selected")
              )
          ),
          
          conditionalPanel(
            condition = "output.fellow_selected",
            div(style = "margin-bottom: 25px;",
                h5("Fellow evaluation type:", class = "question-text"),
                selectInput("fell_eval", "",
                            choices = list("Choose..." = "", 
                                           "As a consultant" = "1", 
                                           "Inpatient service (cards, bronze, ICU)" = "2",
                                           "Consult service (ID, Nephro)" = "3",
                                           "Outpatient rotation" = "4"),
                            selected = "")
            )
          ),
          
          # Add rotation selection for all evaluations
          div(style = "margin-bottom: 25px;",
              h5("Please select the rotation on which you worked with this person:", class = "question-text"),
              selectInput("rotation", "",
                          choices = list("Choose..." = "",
                                         "General Floors - SLUH or VA" = "1",
                                         "Continuity Clinic" = "2", 
                                         "MICU" = "3",
                                         "ACE team" = "4",
                                         "Cardiology" = "5",
                                         "Bronze" = "6",
                                         "Hem/onc Consults" = "7",
                                         "ID Consults" = "8",
                                         "Nephrology Consults" = "9",
                                         "Palliative Care" = "10",
                                         "Endocrine" = "11",
                                         "Other (Please specify)" = "12",
                                         "VA Walk-in, ACS, or Bridge" = "13",
                                         "Other IM Consults (pulm, gi, cards, etc)" = "14"),
                          selected = "")
          ),
          
          # Conditional other rotation specification
          conditionalPanel(
            condition = "input.rotation == '12'",
            div(style = "margin-bottom: 25px;",
                h5("What rotation?", class = "question-text"),
                textInput("other_rot", "", placeholder = "Please specify the rotation...")
            )
          ),
          
          div(style = "margin-bottom: 25px;",
              h5("What went well? (Plus)", class = "question-text"),
              textAreaInput("plus", "", height = "120px",
                            placeholder = "Describe the positive aspects of working with this faculty member...")
          ),
          
          div(style = "margin-bottom: 30px;",
              h5("What could be improved? (Delta)", class = "question-text"),
              textAreaInput("delta", "", height = "120px",
                            placeholder = "Suggest areas for improvement or development...")
          ),
          
          div(style = "text-align: center;",
              actionButton("continue_eval", "Continue to Detailed Questions", 
                           class = "btn-primary btn-modern", style = "font-size: 16px;")
          )
      )
    ),
    
    # Detailed Questions Step (only for attendings)
    conditionalPanel(
      condition = "output.show_eval_step2",
      div(class = "eval-step-card",
          h3("Step 2: Detailed Evaluation", style = "color: #2c3e50; margin-bottom: 25px;"),
          uiOutput("detailed_questions_step2")
      )
    ),
    
    # Final Questions Step (only for attendings)
    conditionalPanel(
      condition = "output.show_eval_step3",
      div(class = "eval-step-card",
          h3("Step 3: Final Questions", style = "color: #2c3e50; margin-bottom: 25px;"),
          uiOutput("detailed_questions_step3")
      )
    )
  ),
  
  # Step 4: Confirmation
  conditionalPanel(
    condition = "output.show_step4",
    div(class = "evaluation-header",
        h2("Evaluation Submitted Successfully!"),
        p("Thank you for your feedback", style = "font-size: 18px;"),
        actionButton("return_to_start_step4", "Return to Start", 
                     class = "btn-outline-light btn-return return-btn")
    ),
    div(class = "eval-step-card",
        div(style = "text-align: center;",
            icon("check-circle", style = "color: #28a745; font-size: 64px; margin-bottom: 20px;"),
            h4("Your evaluation has been recorded", style = "color: #2c3e50; margin-bottom: 30px;"),
            actionButton("evaluate_another", "Evaluate Another Faculty", 
                         class = "btn-primary btn-modern", style = "margin-right: 15px; font-size: 16px;"),
            actionButton("return_to_dashboard", "Return to Dashboard", 
                         class = "btn-secondary btn-modern", style = "font-size: 16px;")
        )
    )
  )
)

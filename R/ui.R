# ui.R - SSM Health SLUCare Modern Professional Design
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  # Modern SSM Health Professional Styling
  tags$head(
    tags$style("
      /* SSM Health Brand Colors & Typography */
      :root {
        --ssm-primary-blue: #003d5c;
        --ssm-secondary-blue: #0066a1;
        --ssm-light-blue: #4a90a4;
        --ssm-accent-blue: #2196f3;
        --ssm-success-green: #00a651;
        --ssm-warning-orange: #ff8c00;
        --ssm-neutral-gray: #6c757d;
        --ssm-light-gray: #f8f9fa;
        --ssm-white: #ffffff;
        --ssm-text-primary: #2c3e50;
        --ssm-text-secondary: #546e7a;
      }
      
      body {
        font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        color: var(--ssm-text-primary);
        line-height: 1.6;
      }
      
      /* Modern Header Styling */
      .navbar-brand {
        font-weight: 700;
        font-size: 1.5rem;
        color: var(--ssm-primary-blue) !important;
        letter-spacing: -0.5px;
      }
      
      /* Professional Card System */
      .ssm-card {
        background: var(--ssm-white);
        border: none;
        border-radius: 16px;
        box-shadow: 0 8px 32px rgba(0, 61, 92, 0.08);
        padding: 2rem;
        margin-bottom: 1.5rem;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .ssm-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, var(--ssm-primary-blue), var(--ssm-secondary-blue));
      }
      
      .ssm-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 16px 48px rgba(0, 61, 92, 0.12);
      }
      
      /* Step Headers */
      .step-header {
        background: linear-gradient(135deg, var(--ssm-primary-blue) 0%, var(--ssm-secondary-blue) 100%);
        color: var(--ssm-white);
        padding: 2rem;
        border-radius: 16px 16px 0 0;
        margin-bottom: 0;
        position: relative;
        overflow: hidden;
      }
      
      .step-header::after {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 200%;
        background: linear-gradient(45deg, transparent 30%, rgba(255,255,255,0.1) 50%, transparent 70%);
        transform: rotate(45deg);
        animation: shimmer 3s infinite;
      }
      
      @keyframes shimmer {
        0% { transform: translateX(-100%) rotate(45deg); }
        100% { transform: translateX(100%) rotate(45deg); }
      }
      
      .step-header h3 {
        margin: 0;
        font-weight: 600;
        font-size: 1.75rem;
        letter-spacing: -0.5px;
      }
      
      /* Return Button Styling */
      .return-btn {
        position: absolute;
        right: 2rem;
        top: 50%;
        transform: translateY(-50%);
        padding: 0.5rem 1.25rem;
        font-size: 0.875rem;
        border-radius: 25px;
        font-weight: 500;
        background: rgba(255, 255, 255, 0.2);
        border: 1px solid rgba(255, 255, 255, 0.3);
        color: var(--ssm-white);
        transition: all 0.3s ease;
        backdrop-filter: blur(10px);
      }
      
      .return-btn:hover {
        background: rgba(255, 255, 255, 0.3);
        border-color: rgba(255, 255, 255, 0.5);
        color: var(--ssm-white);
        transform: translateY(-50%) scale(1.05);
      }
      
      /* Progress Section */
      .progress-section {
        background: linear-gradient(135deg, var(--ssm-white) 0%, var(--ssm-light-gray) 100%);
        border: 1px solid rgba(0, 102, 161, 0.1);
        border-radius: 16px;
        padding: 2rem;
        margin-bottom: 2rem;
        position: relative;
      }
      
      .progress-section::before {
        content: '📊';
        position: absolute;
        top: 1rem;
        right: 1rem;
        font-size: 1.5rem;
        opacity: 0.3;
      }
      
      /* Search Results */
      .search-result {
        background: var(--ssm-white);
        border: 1px solid rgba(0, 102, 161, 0.1);
        border-radius: 12px;
        padding: 1rem;
        margin: 0.5rem 0;
        transition: all 0.3s ease;
        cursor: pointer;
        position: relative;
      }
      
      .search-result:hover {
        border-color: var(--ssm-secondary-blue);
        box-shadow: 0 4px 16px rgba(0, 102, 161, 0.15);
        transform: translateX(4px);
      }
      
      .search-result::before {
        content: '';
        position: absolute;
        left: 0;
        top: 0;
        bottom: 0;
        width: 0;
        background: var(--ssm-secondary-blue);
        transition: width 0.3s ease;
        border-radius: 12px 0 0 12px;
      }
      
      .search-result:hover::before {
        width: 4px;
      }
      
      /* Faculty Card */
      .faculty-card {
        background: linear-gradient(135deg, var(--ssm-white) 0%, #f8f9fa 100%);
        border: 1px solid rgba(0, 102, 161, 0.15);
        border-radius: 16px;
        padding: 1.5rem;
        position: relative;
        overflow: hidden;
      }
      
      .faculty-card::before {
        content: '👩‍⚕️';
        position: absolute;
        top: 1rem;
        right: 1rem;
        font-size: 1.25rem;
        opacity: 0.4;
      }
      
      /* Evaluation Header */
      .evaluation-header {
        background: linear-gradient(135deg, var(--ssm-primary-blue) 0%, var(--ssm-light-blue) 100%);
        color: var(--ssm-white);
        padding: 2.5rem 2rem;
        border-radius: 20px;
        margin-bottom: 2rem;
        text-align: center;
        position: relative;
        overflow: hidden;
      }
      
      .evaluation-header::before {
        content: '';
        position: absolute;
        top: -50%;
        left: -50%;
        width: 200%;
        height: 200%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
        animation: pulse 4s infinite;
      }
      
      @keyframes pulse {
        0%, 100% { opacity: 0.3; transform: scale(1); }
        50% { opacity: 0.6; transform: scale(1.1); }
      }
      
      .evaluation-header h2 {
        font-size: 2.5rem;
        font-weight: 700;
        margin-bottom: 0.5rem;
        letter-spacing: -1px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* Evaluation Step Cards */
      .eval-step-card {
        background: var(--ssm-white);
        border-radius: 20px;
        padding: 2.5rem;
        box-shadow: 0 12px 40px rgba(0, 61, 92, 0.08);
        margin-bottom: 2rem;
        position: relative;
        overflow: hidden;
      }
      
      .eval-step-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 6px;
        background: linear-gradient(90deg, var(--ssm-success-green), var(--ssm-secondary-blue));
      }
      
      /* Modern Form Elements */
      .form-control, .form-select {
        border: 2px solid rgba(0, 102, 161, 0.1);
        border-radius: 12px;
        padding: 0.875rem 1.25rem;
        font-size: 1rem;
        transition: all 0.3s ease;
        background: var(--ssm-white);
      }
      
      .form-control:focus, .form-select:focus {
        border-color: var(--ssm-secondary-blue);
        box-shadow: 0 0 0 4px rgba(0, 102, 161, 0.1);
        outline: none;
      }
      
      /* Modern Rating System */
      .rating-scale {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin: 1.5rem 0;
        padding: 1.5rem;
        background: linear-gradient(135deg, var(--ssm-light-gray) 0%, var(--ssm-white) 100%);
        border-radius: 16px;
        border: 2px solid transparent;
        transition: all 0.3s ease;
        position: relative;
      }
      
      .rating-scale:hover {
        border-color: rgba(0, 102, 161, 0.2);
        box-shadow: 0 8px 24px rgba(0, 102, 161, 0.1);
      }
      
      .rating-option {
        cursor: pointer;
        padding: 0.875rem 1.5rem;
        border-radius: 50px;
        background: var(--ssm-white);
        border: 2px solid rgba(0, 102, 161, 0.2);
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        font-weight: 600;
        min-width: 60px;
        text-align: center;
        color: var(--ssm-text-primary);
        position: relative;
        overflow: hidden;
      }
      
      .rating-option::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent);
        transition: left 0.5s;
      }
      
      .rating-option:hover {
        background: var(--ssm-secondary-blue);
        color: var(--ssm-white);
        transform: translateY(-2px);
        box-shadow: 0 8px 20px rgba(0, 102, 161, 0.3);
      }
      
      .rating-option:hover::before {
        left: 100%;
      }
      
      .rating-option.selected {
        background: var(--ssm-success-green);
        color: var(--ssm-white);
        border-color: var(--ssm-success-green);
        box-shadow: 0 6px 16px rgba(0, 166, 81, 0.3);
      }
      
      /* Question Text */
      .question-text {
        font-size: 1.125rem;
        font-weight: 600;
        color: var(--ssm-text-primary);
        margin-bottom: 1rem;
        line-height: 1.5;
      }
      
      /* Scale Labels */
      .scale-labels {
        display: flex;
        justify-content: space-between;
        font-size: 0.875rem;
        color: var(--ssm-text-secondary);
        margin-top: 0.5rem;
        font-style: italic;
      }
      
      /* Modern Buttons */
      .btn-modern {
        padding: 0.875rem 2rem;
        border-radius: 50px;
        font-weight: 600;
        text-transform: none;
        letter-spacing: 0.5px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        border: none;
        position: relative;
        overflow: hidden;
      }
      
      .btn-modern::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
        transition: left 0.6s;
      }
      
      .btn-modern:hover::before {
        left: 100%;
      }
      
      .btn-primary.btn-modern {
        background: linear-gradient(135deg, var(--ssm-secondary-blue), var(--ssm-accent-blue));
        color: var(--ssm-white);
      }
      
      .btn-primary.btn-modern:hover {
        background: linear-gradient(135deg, var(--ssm-primary-blue), var(--ssm-secondary-blue));
        transform: translateY(-2px);
        box-shadow: 0 8px 24px rgba(0, 102, 161, 0.3);
      }
      
      .btn-success.btn-modern {
        background: linear-gradient(135deg, var(--ssm-success-green), #00c851);
        color: var(--ssm-white);
      }
      
      .btn-success.btn-modern:hover {
        background: linear-gradient(135deg, #00a651, var(--ssm-success-green));
        transform: translateY(-2px);
        box-shadow: 0 8px 24px rgba(0, 166, 81, 0.3);
      }
      
      .btn-warning.btn-modern {
        background: linear-gradient(135deg, var(--ssm-warning-orange), #ffa000);
        color: var(--ssm-white);
      }
      
      .btn-secondary.btn-modern {
        background: linear-gradient(135deg, var(--ssm-neutral-gray), #868e96);
        color: var(--ssm-white);
      }
      
      /* Text Areas */
      textarea.form-control {
        border-radius: 16px !important;
        border: 2px solid rgba(0, 102, 161, 0.1) !important;
        padding: 1.25rem !important;
        font-size: 1rem !important;
        resize: vertical;
        min-height: 120px;
      }
      
      textarea.form-control:focus {
        border-color: var(--ssm-secondary-blue) !important;
        box-shadow: 0 0 0 4px rgba(0, 102, 161, 0.1) !important;
      }
      
      /* Success State */
      .eval-type-selected {
        margin-top: 1rem;
        font-weight: 600;
        color: var(--ssm-success-green);
        padding: 0.75rem 1.25rem;
        background: rgba(0, 166, 81, 0.1);
        border-radius: 12px;
        border-left: 4px solid var(--ssm-success-green);
      }
      
      /* Completion Page */
      .completion-icon {
        color: var(--ssm-success-green);
        font-size: 4rem;
        margin-bottom: 1.5rem;
        filter: drop-shadow(0 4px 8px rgba(0, 166, 81, 0.3));
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .return-btn {
          position: static;
          transform: none;
          margin-top: 1rem;
          width: auto;
        }
        
        .step-header {
          text-align: center;
          padding: 1.5rem;
        }
        
        .evaluation-header h2 {
          font-size: 2rem;
        }
        
        .rating-scale {
          flex-direction: column;
          gap: 0.5rem;
        }
        
        .rating-option {
          width: 100%;
        }
      }
      
      /* Accessibility Enhancements */
      .btn:focus, .form-control:focus, .rating-option:focus {
        outline: 3px solid var(--ssm-accent-blue);
        outline-offset: 2px;
      }
      
      /* Loading States */
      .loading {
        position: relative;
        overflow: hidden;
      }
      
      .loading::after {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(0, 102, 161, 0.2), transparent);
        animation: loading 1.5s infinite;
      }
      
      @keyframes loading {
        0% { left: -100%; }
        100% { left: 100%; }
      }
    ")
  ),
  
  # SSM Health Header
  div(class = "container-fluid mb-4",
      div(class = "row",
          div(class = "col-12",
              div(class = "d-flex justify-content-between align-items-center py-3",
                  div(class = "navbar-brand", "IMSLU Attending or Fellow Evaluation"),
                  div(class = "text-muted", "Internal Medicine Residency Program")
              )
          )
      )
  ),
  
  div(class = "container",
      
      # Step 1: Access Code Entry
      conditionalPanel(
        condition = "output.show_step1",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Welcome to Attending/Fellow Evaluation"),
                p("Please enter your access code to begin", style = "margin: 0; opacity: 0.9;")
            ),
            div(style = "padding: 2rem 0;",
                fluidRow(
                  column(8,
                         div(class = "mb-3",
                             h5("Access Code", class = "question-text"),
                             textInput("access_code_input", 
                                       "", 
                                       placeholder = "Enter your unique access code")
                         ),
                         actionButton("submit_code", "Continue", 
                                      class = "btn btn-primary btn-modern")
                  ),
                  column(4,
                         div(id = "resident_info_display", class = "text-center",
                             textOutput("resident_name")
                         )
                  )
                )
            )
        )
      ),
      
      # Step 2: Faculty Selection with Progress
      conditionalPanel(
        condition = "output.show_step2",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Select Attending or Fellow to Evaluate"),
                actionButton("return_to_start_step2", "Return to Start", 
                             class = "btn return-btn")
            ),
            
            # Progress Section
            div(class = "progress-section",
                h4("Your Evaluation Progress", style = "color: var(--ssm-primary-blue);"),
                uiOutput("faculty_progress_text"),
                br(),
                plotOutput("fac_eval_progress", height = "200px")
            ),
            
            fluidRow(
              column(8,
                     div(class = "mb-4",
                         h5("Search Attending or Fellow", class = "question-text"),
                         textInput("faculty_search", 
                                   "", 
                                   placeholder = "Start typing name...")
                     ),
                     div(id = "search_results",
                         uiOutput("faculty_search_results")
                     ),
                     br(),
                     div(class = "faculty-card",
                         div(class = "form-check",
                             checkboxInput("faculty_not_found", 
                                           "Can't find the attending or fellow? Add them here.", 
                                           value = FALSE)
                         )
                     )
              ),
              column(4,
                     div(class = "faculty-card",
                         h5("Selected Attending/Fellow", class = "question-text"),
                         uiOutput("selected_faculty_info")
                     )
              )
            ),
            
            conditionalPanel(
              condition = "input.faculty_not_found == true",
              div(class = "ssm-card mt-4",
                  div(style = "border-left: 4px solid var(--ssm-secondary-blue); padding-left: 1rem;",
                      h5("Add New Attending or Fellow", class = "question-text"),
                      p("Please provide information about the attending or fellow you'd like to evaluate:")
                  ),
                  fluidRow(
                    column(6,
                           textInput("pend_name", 
                                     "Full Name", 
                                     placeholder = "Enter full name")
                    ),
                    column(6,
                           selectInput("namepend_fac_fell", 
                                       "Type",
                                       choices = list("Choose..." = "", "Attending" = "1", "Fellow" = "2"),
                                       selected = "")
                    )
                  ),
                  textInput("pend_rot", 
                            "Where did you work together?", 
                            placeholder = "e.g., Bronze, Bridge, VA, Diamond, etc."),
                  br(),
                  actionButton("add_pending_faculty", "Create Evaluation", 
                               class = "btn btn-warning btn-modern"),
                  p("This person will be added for administrative review.", 
                    style = "margin-top: 1rem; font-size: 0.875rem; color: var(--ssm-text-secondary);")
              )
            )
        )
      ),
      
      # Step 3: Evaluation Form
      conditionalPanel(
        condition = "output.show_step3",
        div(class = "evaluation-header",
            h2(textOutput("eval_faculty_name", inline = TRUE)),
            p("Attending/Fellow Evaluation", style = "font-size: 1.25rem; margin-bottom: 0; opacity: 0.9;"),
            actionButton("return_to_start_step3", "Return to Start", 
                         class = "btn return-btn")
        ),
        
        # Evaluation Steps
        conditionalPanel(
          condition = "output.show_eval_step1",
          div(class = "eval-step-card",
              h3("General Feedback", style = "color: var(--ssm-primary-blue); margin-bottom: 2rem;"),
              
              # Faculty Type Selection
              conditionalPanel(
                condition = "output.show_eval_type_selection",
                div(class = "mb-4",
                    h5("Are you evaluating an attending or fellow?", class = "question-text"),
                    div(class = "d-flex gap-3 mt-3",
                        actionButton("select_attending", "Attending", 
                                     class = "btn btn-outline-primary btn-modern"),
                        actionButton("select_fellow", "Fellow", 
                                     class = "btn btn-outline-success btn-modern")
                    )
                )
              ),
              
              div(class = "eval-type-selected",
                  textOutput("eval_type_selected")
              ),
              
              # Fellow Specific Questions
              conditionalPanel(
                condition = "output.fellow_selected",
                div(class = "mb-4",
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
              
              # Rotation Selection (Attendings only)
              conditionalPanel(
                condition = "output.show_rotation_question",
                div(class = "mb-4",
                    h5("Select the rotation where you worked together:", class = "question-text"),
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
                )
              ),
              
              # Other Rotation Specification
              conditionalPanel(
                condition = "input.rotation == '12'",
                div(class = "mb-4",
                    h5("Please specify the rotation:", class = "question-text"),
                    textInput("other_rot", "", 
                              placeholder = "Enter rotation name...")
                )
              ),
              
              # Feedback Text Areas
              div(class = "mb-4",
                  h5("What went well? (Plus)", class = "question-text"),
                  textAreaInput("plus", "", 
                                height = "120px",
                                placeholder = "Describe the positive aspects of working with this faculty member...")
              ),
              
              div(class = "mb-4",
                  h5("What could be improved? (Delta)", class = "question-text"),
                  textAreaInput("delta", "", 
                                height = "120px",
                                placeholder = "Suggest areas for improvement or development...")
              ),
              
              div(class = "text-center mt-4",
                  actionButton("continue_eval", "Continue to Detailed Questions", 
                               class = "btn btn-primary btn-modern", 
                               style = "font-size: 1.125rem; padding: 1rem 2.5rem;")
              )
          )
        ),
        
        # Detailed Questions Step (Attendings only)
        conditionalPanel(
          condition = "output.show_eval_step2",
          div(class = "eval-step-card",
              h3("Detailed Evaluation", style = "color: var(--ssm-primary-blue); margin-bottom: 2rem;"),
              uiOutput("detailed_questions_step2")
          )
        ),
        
        # Final Questions Step (Attendings only)
        conditionalPanel(
          condition = "output.show_eval_step3",
          div(class = "eval-step-card",
              h3("Final Questions", style = "color: var(--ssm-primary-blue); margin-bottom: 2rem;"),
              uiOutput("detailed_questions_step3")
          )
        )
      ),
      
      # Step 4: Completion
      conditionalPanel(
        condition = "output.show_step4",
        div(class = "evaluation-header",
            h2("Evaluation Complete!"),
            p("Thank you for your valuable feedback", style = "font-size: 1.25rem; margin-bottom: 0; opacity: 0.9;"),
            actionButton("return_to_start_step4", "Return to Start", 
                         class = "btn return-btn")
        ),
        div(class = "eval-step-card text-center",
            div(class = "completion-icon", "✅"),
            h4("Your evaluation has been successfully submitted", 
               style = "color: var(--ssm-primary-blue); margin-bottom: 2rem;"),
            div(class = "d-flex justify-content-center gap-3",
                actionButton("evaluate_another", "Evaluate Another Attending/Fellow", 
                             class = "btn btn-primary btn-modern"),
                actionButton("return_to_dashboard", "Return to Dashboard", 
                             class = "btn btn-secondary btn-modern")
            )
        )
      )
  )
)
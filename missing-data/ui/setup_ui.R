tabPanel(
  title = "Data and matching criteria",
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      h3("Age"),
      
      radioButtons("match_age_how", "Matching method:",
                   choices = list(
                     "Within margin" = "margin",
                     "By category" = "category"
                   ),
                   selected = "margin"),
      
      numericInput("match_age_margin", "Margin (years):",
                   value = 5, min = 0),
      
      shinyjs::hidden(
        numericInput("match_age_cutoff", "Cutoff (≤ versus >)",
                     value = 75, min = 0)
        ),
      
      hr(),
      
      h3("BMI"),
      
      radioButtons("match_bmi_how", "Matching method:",
                   choices = list(
                     "Within margin" = "margin",
                     "By category" = "category"
                   ),
                   selected = "margin"),
      
      numericInput("match_bmi_margin", "Margin:",
                   value = 5, min = 0),
      
      shinyjs::hidden(
        numericInput("match_bmi_cutoff", "Cutoff (≤ versus >)",
                     value = 30, min = 0)
        ),
      
      hr(),
      
      h3("Regimen"),
      
      checkboxInput("match_regimen", "Match",
                    value = TRUE),
      
      hr(),
      
      h3("Baseline"),
      
      radioButtons("match_baseline_how", "Matching method:",
                   choices = list(
                     "Within margin" = "margin",
                     "By category" = "category"
                   ),
                   selected = "margin"),
      
      numericInput("match_baseline_margin", "Margin:",
                   value = 10, min = 0),
      
      shinyjs::hidden(
        numericInput("match_baseline_cutoff", "Cutoff (≤ versus >)",
                     value = 50, min = 0)
        ),
      
      hr(),
      
      h3("Cycles 1, 2, and 3"),
      
      radioButtons("match_cycles_count", "At least",
                   choices = list(
                     "0" = 0,
                     "1" = 1,
                     "2" = 2,
                     "3" = 3
                   ),
                   selected = 0),
      
      radioButtons("match_cycles_how", "cycle outcome(s) must be matched by",
                   choices = list(
                     "distance" = "margin",
                     "category" = "category"
                   ),
                   selected = "margin"),
      
      numericInput("match_cycles_margin", "with a margin of",
                   value = 10, min = 0),
      
      shinyjs::hidden(
        numericInput("match_cycles_cutoff", "with a cutoff (≤ versus >) of",
                     value = 50, min = 0)
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(
          title = "Dataset",
          
          selectInput("show_matches_for",
                      "Show matches for subject:",
                      choices = c(
                        list("<show all>" = "none"),
                        as.list(need.matches)
                      ),
                      selected = NA),
          
          dataTableOutput("data_table")
        ),
        
        tabPanel(
          title = "Plots",
          
          textOutput("text_plot_note"),
          
          plotOutput("plot_outcome_age"),
          plotOutput("plot_outcome_bmi"),
          plotOutput("plot_outcome_baseline"),
          plotOutput("plot_outcome_treatment")
        ),
        
        tabPanel(
          title = "Match table",
          
          tableOutput("match_table")
        )
      )
      
      
      
      
    )
    
  )
)
tabPanel(
  title = "Imputation results",
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      h3("Parameters"),
      
      numericInput(inputId = "seed",
                   label = "Seed:",
                   value = round(as.numeric(Sys.time()))),
      
      numericInput(inputId = "n_iters",
                   label = "Number of iterations:",
                   min = 1,
                   max = 1000,
                   step = 1,
                   value = 30),
      
      actionButton("compute", "Compute"),
      
      hr(),
      
      h3("Results"),
      
      htmlOutput("results")
    ),
    mainPanel = mainPanel(
      
      tabsetPanel(
        tabPanel(
          title = "Table",
          # tableOutput("table_exec")
          dataTableOutput("table_exec")
        ),
        tabPanel(
          title = "Plots",
          plotOutput("plot_estimates"),
          plotOutput("plot_ses")
        )
      )
      
    )
  )
)




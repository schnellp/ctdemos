sidebarLayout(
  
  sidebarPanel = sidebarPanel(
    h3("Sample size"),
    
    numericInput("sample_size_trt", "Treatment first:",
                 value = 9, min = 1),
    
    numericInput("sample_size_ctl", "Control first:",
                 value = 8, min = 1),
    
    hr(),
    
    h3("Mean change within period"),
    
    numericInput("delta_trt", "Treatment:",
                 value = 0.1, min = 0, step = 0.1),
    
    numericInput("delta_ctl", "Control:",
                 value = 0.0, min = 0, step = 0.1),
    
    hr(),
    
    h3("Standard deviation of baseline measurements"),
    
    numericInput("stdev", "Sigma (total):",
                 value = 0.4, min = 0, step = 0.1),
    
    hr(),
    
    h3("Within-subject correlation"),
    
    numericInput("rho", "Rho:",
                 value = 0.85, min = -1, max = 1, step = 0.05),
    
    hr(),
    
    h3("Trial Design"),
    
    radioButtons("design", "Choose one:",
                 choices = list(
                   "Parallel arm difference-in-means" = "diff",
                   "Parallel arm change-from-baseline" = "change",
                   "Crossover" = "cross"
                 ),
                 selected = "diff"),
    
    hr(),
    
    h3("Significance level"),
    
    numericInput("alpha", "Alpha:",
                 value = 0.05, min = 0, max = 1, step = 0.01)
    
  ),
  
  mainPanel(
    htmlOutput("summary"),
    
    plotOutput("plot_pval"),
    
    plotOutput("plot_tstat"),
    
    plotOutput("plot_est")
    
  )
  
)
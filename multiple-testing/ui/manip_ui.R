shiny::tabPanel(
  
  title = "Edit graph",
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      textOutput("shiny_return"),
      tags$head(tags$style("#shiny_return{font-weight: bold;}")),
      
      hr(),
      
      actionButton("add_node", "Add node"),
      
      actionButton("add_edge", "Add edge"),
      
      hr(),
      
      textInput("hypname", "Hypothesis name", value = NA),
      
      numericInput("alpha", "alpha", value = NA, min = 0, max = 1, step = 0.001),
      
      numericInput("pval", "p-value", value = NA, min = 0, max = 1, step = 0.001),
      
      actionButton("delete_node", "Delete node"),
      
      hr(),
      
      numericInput("weight", "Weight", value = NA, min = 0, max = 1, step = 0.01),
      
      actionButton("delete_edge", "Delete edge"),
      
      hr(),
      
      actionButton("check_validity", "Check graph validity"),
      
      htmlOutput("warnings")
      
    ),
    mainPanel = mainPanel(
      visNetworkOutput("network",height = "600px")
    )
  )
  
  
)
tabPanel(
  title = "Execute procedure",
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      actionButton("reset", "Load/reset procedure"),
      
      hr(),
      
      actionButton("step_backward", "Step backward"),
      
      htmlOutput("test"),
      
      actionButton("step_forward", "Step forward"),
      
      hr(),
      
      actionButton("step_end", "Skip to end")
    ),
    mainPanel = mainPanel(
      visNetworkOutput("network_exec",height = "600px")
    )
  )
  )
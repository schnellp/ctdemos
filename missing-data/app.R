library(shiny)
library(shinyjs)
library(DT)

library(dplyr)
library(scales)
library(ggplot2)

source("./R/data-generation.R")

data <- generate.data()

need.matches <- data %>% filter(is.na(follow.up)) %>% pull(subject) %>% as.character()

ui <- fluidPage(theme = "table1_defaults.css",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("Missing Data"),
  
  tabsetPanel(
    source("./ui/setup_ui.R", local = TRUE)$value,
    source("./ui/exec_ui.R", local = TRUE)$value
  )
  
  
  # tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                
  
  
 
)

server <- function(input, output, session) {
  
  source("./server/setup_server.R", local = TRUE)$value
  source("./server/exec_server.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)
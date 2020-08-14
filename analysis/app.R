library(shiny)
library(shinyjs)

library(DT)
library(table1)

library(dplyr)
library(scales)
library(ggplot2)

source("./R/data-generation.R")

values <- reactiveValues()

values$data <- generate.data.2()

ui <- fluidPage(theme = "table1_defaults.css",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("Analysis and Intercurrent Events"),
  
  tabsetPanel(
    source("./ui/data_ui.R", local = TRUE)$value,
    source("./ui/result_ui.R", local = TRUE)$value
  )
)

server <- function(input, output, session) {
  
  source("./server/data_server.R", local = TRUE)$value
  source("./server/result_server.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)
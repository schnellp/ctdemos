library(shiny)
library(shinyjs)
library(DT)

library(scales)
library(ggplot2)

source("./R/data-generation.R")

# data <- generate.data()

ui <- fluidPage(theme = "table1_defaults.css",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("Power"),
  
  source("./ui/ui.R", local = TRUE)$value
)

server <- function(input, output, session) {
  source("./server/server.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)
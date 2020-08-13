load.or.install <- function(package.string) {
  if (!require(package.string, character.only = TRUE)) {
    install.packages(package.string)
    library(package.string, character.only = TRUE)
  }
}

load.or.install("shiny")
load.or.install("shinyjs")

load.or.install("dplyr")

load.or.install("ggplot2")
load.or.install("scales")

load.or.install("DT")
load.or.install("table1")

load.or.install("igraph")
load.or.install("visNetwork")

ui <- fluidPage(
  titlePanel("Success!"),
  
  source("./ui/setup_ui.R", local = TRUE)$value
)

server <- function(input, output, session) {
  source("./server/setup_server.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)

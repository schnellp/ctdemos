require(shiny)
require(shinyjs)
require(igraph)
require(visNetwork)

server <- function(input, output, session) {
  
  source("./server/visIgraphLayoutMine.R")
  
  source("./server/manip_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./server/exec_server.R", local = TRUE, encoding = "UTF-8")
}


ui <- fluidPage(
  
  useShinyjs(),
  
  title = "Multiple Testing",
  
  titlePanel("Graphical Multiple Testing"),
  
  tabsetPanel(
    source("./ui/manip_ui.R", local = TRUE)$value,
    source("./ui/exec_ui.R", local = TRUE)$value
  )
)

shinyApp(ui = ui, server = server)
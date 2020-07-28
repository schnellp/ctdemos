library(shiny)
library(shinyjs)

library(table1)

source("R/simulation.R")

ui <- fluidPage(theme = "table1_defaults.css",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                
  titlePanel("Randomization"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      numericInput(inputId = "seed",
                   label = "Seed:",
                   value = round(as.numeric(Sys.time()))),
      
      numericInput(inputId = "sample_size",
                   label = "Sample size:",
                   min = 1,
                   max = 1000,
                   step = 1,
                   value = 30),
      
      hr(),
      
      sliderInput(inputId = "n_arms",
                   label = "Number of arms:",
                   min = 2, max = 4, value = 2, step = 1),
      
      tags$b("Randomization ratio:"),
      
      selectInput(inputId = "ratio_1",
                  label = NULL,
                  choices = list(1, 2, 3, 4, 5),
                  selected = 1),
      
      uiOutput("help_ratio_1"),
      
      selectInput(inputId = "ratio_2",
                  label = NULL,
                  choices = list(1, 2, 3, 4, 5),
                  selected = 1),
      
      uiOutput("help_ratio_2"),
      
      shinyjs::hidden(
        selectInput(inputId = "ratio_3",
                    label = NULL,
                    choices = list(1, 2, 3, 4, 5),
                    selected = 1)
      ),
      
      
      uiOutput("help_ratio_3"),
      
      shinyjs::hidden(
        selectInput(inputId = "ratio_4",
                    label = NULL,
                    choices = list(1, 2, 3, 4, 5),
                    selected = 1)
      ),
      
      
      
      
      
      hr(),
      
      radioButtons(inputId = "random_type",
                   label = "Randomization type:",
                   choices = list(
                     "Simple" = "simple",
                     "Block" = "block"
                   ),
                   selected = "simple"),
      
      shinyjs::hidden(
        
        radioButtons(inputId = "block_scale",
                     label = "Block size:",
                     choices = list(
                       "2" = 1,
                       "4" = 2,
                       "6" = 3
                     ),
                     selected = 1)
      ),
      
      hr(),
      
      checkboxGroupInput(inputId = "stratify_on",
                         label = "Stratify on:",
                         choices = list(
                           "Site" = "site",
                           "Sex" = "sex",
                           "Age 65+" = "age.binary",
                           "Race" = "race",
                           "Arthritis" = "arthritis",
                           "PASI 15+" = "pasi.binary",
                           "Ham-D" = "hamd"
                         ))
      
    ),
    
    mainPanel(
      
      htmlOutput(outputId = "table_output")
      
      
    )
    
  )
)

server <- function(input, output, session) {
  
  observe({
    shinyjs::toggle(id = "block_scale", condition = input$random_type == "block")
  })
  
  observe({
    saved.block.scale <- input$block_scale
    
    n.arms <- input$n_arms
    
    if (n.arms == 2) {
      odds <- as.numeric(c(input$ratio_1, input$ratio_2))
    } else if (n.arms == 3) {
      odds <- as.numeric(c(input$ratio_1, input$ratio_2, input$ratio_3))
    } else if (n.arms == 4) {
      odds <- as.numeric(c(input$ratio_1, input$ratio_2, input$ratio_3, input$ratio_4))
    }
    
    choices <- as.list(1 : 3)
    
    names(choices) <- as.character((1 : 3) * sum(odds))
    
    shinyjs::toggle(id = "ratio_3", condition = as.numeric(input$n_arms) > 2)
    shinyjs::toggle(id = "ratio_4", condition = input$n_arms > 3)
    
    updateRadioButtons(session,
                       inputId = "block_scale",
                       choices = choices,
                       selected = saved.block.scale
    )
  })
  
  output$help_ratio_1 <- renderUI({
    if (input$n_arms > 1) {
      helpText("to")
    }
  })
  
  output$help_ratio_2 <- renderUI({
    if (input$n_arms > 2) {
      helpText("to")
    }
  })
  
  output$help_ratio_3 <- renderUI({
    if (input$n_arms > 3) {
      helpText("to")
    }
  })
  
  
  output$table_output <- renderUI({
    
    n.arms <- input$n_arms
    
    if (n.arms == 2) {
      odds <- as.numeric(c(input$ratio_1, input$ratio_2))
    } else if (n.arms == 3) {
      odds <- as.numeric(c(input$ratio_1, input$ratio_2, input$ratio_3))
    } else if (n.arms == 4) {
      odds <- as.numeric(c(input$ratio_1, input$ratio_2, input$ratio_3, input$ratio_4))
    }
    
    odds <- odds * as.numeric(input$block_scale)
    
    data <- simulate(seed = input$seed,
                     n.subs = input$sample_size,
                     n.arms = n.arms,
                     stratify.on = input$stratify_on,
                     random.type = input$random_type,
                     odds = odds)
      
    
    table1( ~ sex + age + race +
              arthritis + pasi + hamd +
              site | arm, data = data)
    
  })
}

shinyApp(ui = ui, server = server)
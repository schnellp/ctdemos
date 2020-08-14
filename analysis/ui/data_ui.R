tabPanel(
  title = "Data",
  
  h3("Analysis groups"),
  
  fluidRow(
    column(12,
           HTML(
             "<table style='border: 1px solid black' class='input_2x2'>",
             "<tbody>",
             "<tr>",
             "<td></td><td></td><td style='border-bottom: 1px solid black'>Exposure</td><td style='border-bottom: 1px solid black'></td>",
             "</tr>",
             "<tr>",
             "<td></td><td></td><td>Control</td><td>Treatment</td>",
             "</tr>",
             "<tr>",
             "<td style='border-right: 1px solid black'>Assignment</td><td>Control</td>",
             "<td>",
             toString(textInput("group_cc", label = NULL, value = "PP Control")),
             "</td>",
             "<td>",
             toString(textInput("group_ct", label = NULL, value = "PP Excluded")),
             "</td>",
             "</tr>",
             "<tr>",
             "<td style='border-right: 1px solid black'></td><td>Treatment</td>",
             "<td>",
             toString(textInput("group_tc", label = NULL, value = "PP Excluded")),
             "</td>",
             "<td>",
             toString(textInput("group_tt", label = NULL, value = "PP Treatment")),
             "</td>",
             "</tr>",
             "</tbody>",
             "</table>"
           ))
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           dataTableOutput("data_table"))
  )
  
  
)
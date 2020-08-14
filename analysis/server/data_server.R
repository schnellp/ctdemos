output$data_table <- renderDataTable({
  values$data <- values$data %>% 
    mutate(
      Analysis_Group = factor(
        case_when(
          Assignment == "Control" & Exposure == "Control" ~ input$group_cc,
          Assignment == "Control" & Exposure == "Treatment" ~ input$group_ct,
          Assignment == "Treatment" & Exposure == "Control" ~ input$group_tc,
          Assignment == "Treatment" & Exposure == "Treatment" ~ input$group_tt,
          TRUE ~ as.character(NA)
          )#,
        # levels = unique(
        #   c(
        #     input$group_cc,
        #     input$group_ct,
        #     input$group_tc,
        #     input$group_tt
        #     )
        #   )
        )
      )
  
  values$data
})

output$textout <- renderText("hello")
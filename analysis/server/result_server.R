output$table_output <- renderUI({
  table1( ~ Assignment + Exposure +
            Adverse_Event + Pain_Improvement + Therapy_Length | Analysis_Group,
          data = values$data)
})
result <- reactive({
  sapply(1 : 1000, simulate,
         n.trt = as.numeric(input$sample_size_trt),
         n.ctl = as.numeric(input$sample_size_ctl),
         delta.trt = as.numeric(input$delta_trt),
         delta.ctl = as.numeric(input$delta_ctl),
         sd = as.numeric(input$stdev),
         rho = as.numeric(input$rho),
         design = input$design,
         alpha = as.numeric(input$alpha))
})

output$plot_pval <- renderPlot({
  
  plot.data <- data.frame(t(result()))
  
  brx <- pretty(range(plot.data$pval), 
                n = nclass.Sturges(plot.data$pval),min.n = 1)
  
  ggplot(plot.data, aes(x = pval)) +
    geom_histogram(breaks = brx,
                   color = "darkgray",
                   fill = "white") +
    xlab("p-value")
})

output$plot_tstat <- renderPlot({
  
  plot.data <- data.frame(t(result()))
  
  brx <- pretty(range(plot.data$tstat), 
                n = nclass.Sturges(plot.data$tstat),min.n = 1)
  
  ggplot(plot.data, aes(x = tstat)) +
    geom_histogram(breaks = brx,
                   color = "darkgray",
                   fill = "white") +
    xlab("t-statistic")
})

output$plot_est <- renderPlot({
  
  plot.data <- data.frame(t(result()))
  
  brx <- pretty(range(plot.data$est), 
                n = nclass.Sturges(plot.data$est),min.n = 1)
  
  ggplot(plot.data, aes(x = est)) +
    geom_histogram(breaks = brx,
                   color = "darkgray",
                   fill = "white") +
    xlab("Effect estimate")
})

output$summary <- renderUI({
  plot.data <- data.frame(t(result()))
  paste0("Estimated power (", nrow(plot.data), " iterations): ",
         round(100 * mean(plot.data$pval < 0.05), 1), "%")
})
reactive.message <- reactiveVal("reactive.test")

result <- reactive({
  # input validation
  input.valid <- TRUE
  message <- "Errors:<ul>"
  
  n.trt <- as.numeric(input$sample_size_trt)
  if (is.na(n.trt)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Treatment-first sample size not a number.")
  } else if (n.trt <= 0) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Treatment-first sample size must be positive.")
  } else if (n.trt != round(n.trt)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Treatment-first sample size must be an integer")
  }
  
  n.ctl <- as.numeric(input$sample_size_ctl)
  if (is.na(n.ctl)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Control-first sample size not a number.")
  } else if (n.ctl <= 0) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Control-first sample size must be positive.")
  } else if (n.ctl != round(n.ctl)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Control-first sample size must be an integer")
  }
  
  sd <- as.numeric(input$stdev)
  if (is.na(sd)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Standard deviation not a number.")
  } else if (sd <= 0) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Standard deviation must be positive.")
  }
  
  delta.trt <- as.numeric(input$delta_trt)
  if (is.na(delta.trt)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Treatment mean change not a number.")
  }
  
  delta.ctl <- as.numeric(input$delta_ctl)
  if (is.na(delta.ctl)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Control mean change not a number.")
  }
  
  rho <- as.numeric(input$rho)
  if (is.na(rho)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Within-subject correlation not a number.")
  } else if (rho <= -1 || rho >= 1) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Within-subject correlation not in range (-1 to 1), exclusive")
  }
  
  alpha <- as.numeric(input$alpha)
  if (is.na(alpha)) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Significance level not a number.")
  } else if (alpha <= 0 || alpha >= 1) {
    input.valid <- FALSE
    message <- paste0(message, "<li>Significance level not in range (0 to 1), exclusive")
  }
  
  
  message <- paste0(message, "</ul>")
  
  
  # simulation
  if (input.valid) {
    return(
      sapply(1 : 1000, simulate,
             n.trt = as.numeric(n.trt),
             n.ctl = as.numeric(n.ctl),
             delta.trt = as.numeric(input$delta_trt),
             delta.ctl = as.numeric(input$delta_ctl),
             sd = sd,
             rho = as.numeric(input$rho),
             design = input$design,
             alpha = as.numeric(input$alpha))
    )
  } else {
    reactive.message(message)
    return(NULL)
  }
  
})

output$plot_pval <- renderPlot({
  if (!is.null(result())) {
    plot.data <- data.frame(t(result()))
    
    brx <- pretty(range(plot.data$pval), 
                  n = nclass.Sturges(plot.data$pval),min.n = 1)
    
    plot <- ggplot(plot.data, aes(x = pval)) +
      geom_histogram(breaks = brx,
                     color = "darkgray",
                     fill = "white") +
      xlab("p-value")
    
    return(plot)
  }
})

output$plot_tstat <- renderPlot({
  if (!is.null(result())) {
    plot.data <- data.frame(t(result()))
    
    brx <- pretty(range(plot.data$tstat), 
                  n = nclass.Sturges(plot.data$tstat),min.n = 1)
    
    plot <- ggplot(plot.data, aes(x = tstat)) +
      geom_histogram(breaks = brx,
                     color = "darkgray",
                     fill = "white") +
      xlab("t-statistic")
    
    return(plot)
  }
  
})

output$plot_est <- renderPlot({
  if (!is.null(result())) {
    plot.data <- data.frame(t(result()))
    
    brx <- pretty(range(plot.data$est), 
                  n = nclass.Sturges(plot.data$est),min.n = 1)
    
    plot <- ggplot(plot.data, aes(x = est)) +
      geom_histogram(breaks = brx,
                     color = "darkgray",
                     fill = "white") +
      xlab("Effect estimate")
    
    return(plot)
  }
})

output$summary <- renderUI({
  if (!is.null(result())) {
    plot.data <- data.frame(t(result()))
    paste0("Estimated power (", nrow(plot.data), " iterations): ",
           round(100 * mean(plot.data$pval < 0.05), 1), "%")
  } else {
    HTML(reactive.message())
  }
})
observeEvent(input$compute, {
  
  potential.matches <- list()
  unmatched <- character(0)
  for (j in need.matches) {
    potential.matches[[j]] <- data %>%
      get_matches(j, include.self = FALSE)
    if (nrow(potential.matches[[j]]) == 0) {
      unmatched <- c(unmatched, j)
    }
  }
  
  if (length(unmatched) > 0) {
    alert(paste0("Warning! The following subjects do not have matches: ",
                paste0(unmatched, collapse = ", "),
                ". They will be dropped from the analysis."))
  }
  
  set.seed(input$seed)
  
  n.iters <- as.numeric(input$n_iters)
  
  mat <- matrix(NA, nrow = n.iters, ncol = 1 + length(need.matches) + 2)
  colnames(mat) <- c("iter",
                     paste0("val.", need.matches),
                     "delta.hat",
                     "se.hat")
  
  for (i in 1 : nrow(mat)) {
    for (j in need.matches) {
      
      if (nrow(potential.matches[[j]]) > 0) {
        mat[i, paste0("val.", j)] <- potential.matches[[j]] %>%
          sample_n(1) %>%
          pull(follow.up)
      } else {
        mat[i, paste0("val.", j)] <- NA
      }
    }
    
    data.calc <- data
    data.calc$follow.up[data.calc$subject %in% need.matches] <- mat[i, paste0("val.", need.matches)]
    
    test <- t.test(I(follow.up - baseline) ~ regimen, data = data.calc)
    mat[i, "delta.hat"] <- diff(rev(test$estimate))
    mat[i, "se.hat"] <- test$stderr
  }
  
  tab <- data.frame(mat)
  tab$se.hat <- round(tab$se.hat, 2)
  tab$iter <- 1 : n.iters
  
  # print(tab)
  
  
  tab <- tab %>% rename("Iteration" = iter,
                        "Effect estimate" = delta.hat,
                        "Estimated SE" = se.hat)
  
  # output$table_exec <- renderTable(tab)
  output$table_exec <- renderDataTable(tab,
                                       rownames = FALSE)
  
  theta.hat <- mat[, "delta.hat"]
  theta.hat.mi <- mean(theta.hat)
  
  theta.hat.se <- mat[, "se.hat"]
  se.within <- sqrt(mean(theta.hat.se ^ 2))
  se.between <- sd(theta.hat.se)
  se.mi <- sqrt(se.within ^ 2 + (1 + 1 / n.iters) * se.between ^ 2)
  
  test.naive <- t.test(I(follow.up - baseline) ~ regimen, data = data)
  
  output$results <- renderUI(
    HTML(paste0(
      "Missing outcome deletion",
      "<ul>",
      "<li>Estimate: ",
      round(diff(rev(test.naive$estimate)), 2),
      "</li>",
      "<li>Standard error: ",
      round(test.naive$stderr, 2),
      "</li>",
      "<li>95% CI: (",
      paste(round(diff(rev(test.naive$estimate)) +
                    qnorm(c(0.025, 0.975)) * test.naive$stderr, 2),
            collapse = ", "),
      ")</li>",
      "</ul>",
      "Multiple imputation",
      "<ul>",
      "<li>Estimate: ",
      round(theta.hat.mi, 2),
      "</li>",
      "<li>Standard error: ",
      round(se.mi, 2),
      "</li>",
      "<li>95% CI: (",
      paste(round(theta.hat.mi + qnorm(c(0.025, 0.975)) * se.mi, 2), collapse = ", "),
      ")</li>",
      "</ul>"
    ))
  )
  
  output$plot_estimates <- renderPlot({
    brx <- pretty(range(tab$`Effect estimate`), 
                  n = nclass.Sturges(tab$`Effect estimate`),min.n = 1)
    
    ggplot(tab, aes(x = `Effect estimate`)) +
      geom_histogram(breaks = brx,
                     color = "darkgray",
                     fill = "white")
  })
  
  output$plot_ses <- renderPlot({
    brx <- pretty(range(tab$`Estimated SE`), 
                  n = nclass.Sturges(tab$`Estimated SE`),min.n = 1)
    
    ggplot(tab, aes(x = `Estimated SE`)) +
      geom_histogram(breaks = brx,
                     color = "darkgray",
                     fill = "white")
  })

  show("table_exec")
  show("plot_estimates")
  show("plot_ses")
  
})

observe({
  input$match_age_how
  input$match_age_margin
  input$match_age_cutoff
  
  input$match_bmi_how
  input$match_bmi_margin
  input$match_bmi_cutoff
  
  input$match_regimen
  
  input$match_baseline_how
  input$match_baseline_margin
  input$match_baseline_cutoff
  
  input$match_cycles_count
  input$match_cycles_how
  input$match_cycles_margin
  input$match_cycles_cutoff
  
  input$seed
  input$n_iters
  
  hide("table_exec")
  hide("plot_estiamtes")
  hide("plot_ses")
  
  output$results <- renderUI(
    HTML("The configuration has been changed. Press 'Compute' to show results.")
  )
})

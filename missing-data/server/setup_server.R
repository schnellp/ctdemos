# data <- generate.data()

# need.matches <- data %>% filter(is.na(follow.up)) %>% pull(subject) %>% as.character()

get_matches <- function(data, target.id, include.self = FALSE) {
  target.row <- data %>% filter(subject == target.id)
  
  data.show <- data
  
  ### age ###
  
  target.age <- target.row %>% pull(age)
  
  if (input$match_age_how == "margin") {
    data.show <- data.show %>%
      filter(abs(age - target.age) <= input$match_age_margin)
  } else if (input$match_age_how == "category") {
    if (target.age <= input$match_age_cutoff) {
      data.show <- data.show %>%
        filter(age <= input$match_age_cutoff)
    } else {
      data.show <- data.show %>%
        filter(age > input$match_age_cutoff)
    }
  }
  
  ### bmi ###
  
  target.bmi <- target.row %>% pull(bmi)
  
  if (input$match_bmi_how == "margin") {
    data.show <- data.show %>%
      filter(abs(bmi - target.bmi) <= input$match_bmi_margin)
  } else if (input$match_bmi_how == "category") {
    if (target.bmi <= input$match_bmi_cutoff) {
      data.show <- data.show %>%
        filter(bmi <= input$match_bmi_cutoff)
    } else {
      data.show <- data.show %>%
        filter(bmi > input$match_bmi_cutoff)
    }
  }
  
  ### regimen ###
  
  if (input$match_regimen) {
    target.regimen <- target.row %>% pull(regimen)
    
    data.show <- data.show %>%
      filter(regimen == target.regimen)
  }
  
  ### baseline ###
  
  target.baseline <- target.row %>% pull(baseline)
  
  if (input$match_baseline_how == "margin") {
    data.show <- data.show %>%
      filter(abs(baseline - target.baseline) <= input$match_baseline_margin)
  } else if (input$match_baseline_how == "category") {
    if (target.baseline <= input$match_baseline_cutoff) {
      data.show <- data.show %>%
        filter(baseline <= input$match_baseline_cutoff)
    } else {
      data.show <- data.show %>%
        filter(baseline > input$match_baseline_cutoff)
    }
  }
  
  ### cycles ###
  
  target.cycle.1 <- target.row %>% pull(cycle.1)
  target.cycle.2 <- target.row %>% pull(cycle.2)
  target.cycle.3 <- target.row %>% pull(cycle.3)
  
  if (input$match_cycles_count != "0") {
    if (input$match_cycles_how == "margin") {
      margin <- as.numeric(input$match_cycles_margin)
    
      
      data.show <- data.show %>%
        mutate(
          match.1 = abs(cycle.1 - target.cycle.1) <= margin,
          match.2 = abs(cycle.2 - target.cycle.2) <= margin,
          match.3 = abs(cycle.3 - target.cycle.3) <= margin
        ) %>%
        mutate(n.matches = rowSums(.[c("match.1", "match.2", "match.3")], na.rm = TRUE)) %>%
        filter(n.matches >= as.numeric(input$match_cycles_count)) %>%
        select(-c(match.1, match.2, match.3, n.matches))
      
    } else if (input$match_cycles_how == "category") {
      
    }
  }
  
  ### final processing ###
  
  if (include.self) {
    data.show <- data.show %>% filter(subject == target.id | !is.na(follow.up))
  } else {
    data.show <- data.show %>% filter(subject != target.id & !is.na(follow.up))
  }
  
  data.show
}

output$data_table <- renderDataTable(
  
  {
    data.show <- data
    
    if (input$show_matches_for != "none") {
      
      target.id <- input$show_matches_for
      
      data.show <- data %>% get_matches(target.id, include.self = TRUE)
    }
    
    dt <- datatable(data.show, options = list(pageLength = 50), rownames = FALSE)
    
    if (input$show_matches_for != "none") {
      dt <- dt %>% formatStyle("subject", target = "row",
                               backgroundColor = styleEqual(
                                 c(input$show_matches_for),
                                 "lightblue",
                                 default = NULL
                               ))
    }
    
    dt
  }
  
)

output$match_table <- renderTable({
  tab <- data.frame(
    subject = as.character(need.matches),
    possible.match.ids = as.character(NA),
    possible.match.values = as.character(NA),
    
    stringsAsFactors = FALSE
  )
  
  for (i in 1 : nrow(tab)) {
    matches <- data %>% get_matches(tab$subject[i], include.self = FALSE)
    tab$possible.match.ids[i] <- paste(matches %>% pull(subject), collapse = ", ")
    tab$possible.match.values[i] <- paste(matches %>% pull(follow.up) %>% sort(), collapse = ", ")
  }
  
  tab %>% rename("Subject" = subject,
                 "Subject matches" = possible.match.ids,
                 "Matched follow.up values (sorted)" = possible.match.values)
})

observe({
  shinyjs::toggle(id = "match_age_margin", condition = input$match_age_how == "margin")
  shinyjs::toggle(id = "match_age_cutoff", condition = input$match_age_how == "category")
})

observe({
  shinyjs::toggle(id = "match_bmi_margin", condition = input$match_bmi_how == "margin")
  shinyjs::toggle(id = "match_bmi_cutoff", condition = input$match_bmi_how == "category")
})

observe({
  shinyjs::toggle(id = "match_baseline_margin", condition = input$match_baseline_how == "margin")
  shinyjs::toggle(id = "match_baseline_cutoff", condition = input$match_baseline_how == "category")
})

observe({
  shinyjs::toggle(id = "match_cycles_margin", condition = input$match_cycles_how == "margin")
  shinyjs::toggle(id = "match_cycles_cutoff", condition = input$match_cycles_how == "category")
})

output$plot_outcome_age <- renderPlot({
  plot.data <- data %>%
    mutate(follow.up.status = if_else(is.na(follow.up), "missing", "observed"))
  
  ggplot(plot.data, aes(x = age, y = follow.up)) +
    geom_point() +
    labs(x = "Age",
         y = "Follow-up") +
    geom_vline(data = plot.data %>% filter(is.na(follow.up)),
               aes(xintercept = age),
               linetype = 2)
})

output$plot_outcome_bmi <- renderPlot({
  plot.data <- data %>%
    mutate(follow.up.status = if_else(is.na(follow.up), "missing", "observed"))
  
  ggplot(plot.data, aes(x = bmi, y = follow.up)) +
    geom_point() +
    labs(x = "BMI",
         y = "Follow-up") +
    geom_vline(data = plot.data %>% filter(is.na(follow.up)),
               aes(xintercept = bmi),
               linetype = 2)
})

output$plot_outcome_baseline <- renderPlot({
  plot.data <- data %>%
    mutate(follow.up.status = if_else(is.na(follow.up), "missing", "observed"))
  
  ggplot(plot.data, aes(x = baseline, y = follow.up)) +
    geom_point() +
    labs(x = "Baseline",
         y = "Follow-up") +
    geom_vline(data = plot.data %>% filter(is.na(follow.up)),
               aes(xintercept = baseline),
               linetype = 2)
})

output$plot_outcome_treatment <- renderPlot({
  plot.data <- data %>%
    mutate(follow.up.status = if_else(is.na(follow.up), "missing", "observed"))
  
  ggplot(plot.data, aes(x = regimen, y = follow.up)) +
    geom_jitter(width = 0.2, height = 0) +
    labs(x = "Regimen",
         y = "Follow-up") +
    annotate("text", x = "docetaxel", y = -Inf, vjust = -1, size = 5,
             label = paste(plot.data %>% filter(is.na(follow.up), regimen == "docetaxel") %>% nrow(),
                           "missing")) +
    annotate("text", x = "paclitaxel", y = -Inf, vjust = -1, size = 5,
             label = paste(plot.data %>% filter(is.na(follow.up), regimen == "paclitaxel") %>% nrow(),
                           "missing"))
})

output$text_plot_note <- renderText({
  "Vertical lines indicate missing outcome values."
})

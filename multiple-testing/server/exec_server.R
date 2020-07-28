exec_state <- reactiveValues(initialized = FALSE,
                             positions = NULL,
                             step_string = "<ol></ol>")
state_history <- list()
state_index <- NA

tests.exec <- data.frame(
  id = numeric(),
  hypname = character(),
  alpha = numeric(),
  pval = numeric(),
  stringsAsFactors = FALSE
)

nodes.exec <- data.frame(
  id = numeric(),
  label = character(),
  color = character()
)

edge.mat.exec <- matrix(nrow = 0, ncol = 4)
colnames(edge.mat.exec) <- c("id", "from", "to", "weight")

edges.exec <- data.frame(id = edge.mat.exec[, "id"],
                    from = edge.mat.exec[, "from"], to = edge.mat.exec[, "to"],
                    label = edge.mat.exec[, "weight"],
                    value = edge.mat.exec[, "weight"])

output$network_exec <- renderVisNetwork({
  
  np <- input$network_positions
  
  for (id in nodes.exec$id) {
    nodes.exec$x[nodes.exec$id == id] <<-
      np[[as.character(id)]]$x / 50
    nodes.exec$y[nodes.exec$id == id] <<-
      np[[as.character(id)]]$y / 50
  }
  
  if (exec_state$initialized) {
    visNetwork(nodes.exec, edges.exec, height = "500px") %>%
      visOptions(manipulation = FALSE,
                 highlightNearest = FALSE,
                 autoResize = TRUE) %>%
      visNodes(shape = "box") %>%
      visIgraphLayoutMine() %>%
      visEdges(arrows = 'to',
               arrowStrikethrough = FALSE,
               smooth = list(enabled = TRUE,
                             type = "curvedCCW",
                             roundness = "0.15"),
               color = list(color = "#848484",
                            highlight = "#404084")) %>%
      visInteraction(
        dragNodes = FALSE,
        dragView = TRUE,
        selectable = FALSE
      )
  }
  
  
})

observeEvent(input$reset, {
  
  visNetworkProxy("network") %>% visGetPositions(input = "network_positions")
  
  tests.exec <<- tests
  nodes.exec <<- nodes
  nodes.exec$color <<- "lightblue"
  edge.mat.exec <<- edge.mat
  edges.exec <<- edges
    
  
  exec_state$initialized <<- TRUE
  
  state_history <<- list(
    list(
      description = "Initialization",
      tests = tests.exec,
      nodes = nodes.exec,
      edge.mat = edge.mat.exec,
      edges = edges.exec
    )
  )
  
  repeat {
    li <- length(state_history)
    
    next.rejection <- find.next.rejection(state_history[[length(state_history)]])
    
    # print(next.rejection)
    
    if (length(next.rejection) == 0) {
      failed.to.reject <- state_history[[li]]$tests[
        !is.na(state_history[[li]]$tests[, "alpha"]),
        "hypname"]
      
      state_history[[li + 1]] <<-
        list(
          description = paste("Fail to reject:", paste(failed.to.reject, collapse = ", ")),
          tests = state_history[[li]]$tests,
          nodes = state_history[[li]]$nodes,
          edge.mat = state_history[[li]]$edge.mat,
          edges = state_history[[li]]$edges
        )
      
      break
    } else {
      state_history[[li + 1]] <<-
        list(
          description = paste("Reject",
                              state_history[[li]]$tests[
                                state_history[[li]]$tests[, "id"] == next.rejection,
                                "hypname"]),
          tests = state_history[[li]]$tests,
          nodes = state_history[[li]]$nodes,
          edge.mat = state_history[[li]]$edge.mat,
          edges = state_history[[li]]$edges
        )
      
      state_history[[li + 1]]$nodes[
        state_history[[li + 1]]$nodes$id == next.rejection,
        "color"] <<- "lightgreen"
      
      ### redistribute alpha ###
      
      li <- li + 1
      
      state_history[[li + 1]] <<- redistribute.alpha(state_history[[li]],
                                                     next.rejection)
      
      ### update edge weights ###
      
      li <- li + 1
      
      state_history[[li + 1]] <<- update.edges(state_history[[li]],
                                               next.rejection)
    }
    
    
  }
  
  state_index <<- 1
  
  update_step_string()
  
  enable("step_forward")
  
  visNetworkProxy("network_exec") %>%
    visUpdateNodes(state_history[[state_index]]$nodes) %>%
    visUpdateEdges(state_history[[state_index]]$edges)
  
})

observeEvent(input$step_forward, {
  state_index <<- state_index + 1
  
  if (state_index > 1) {
    enable("step_backward")
  }
  
  if (state_index == length(state_history)) {
    disable("step_forward")
  }
  
  update_step_string()
  
  visNetworkProxy("network_exec") %>%
    visUpdateNodes(state_history[[state_index]]$nodes) %>%
    visUpdateEdges(state_history[[state_index]]$edges)
})

observeEvent(input$step_backward, {
  state_index <<- state_index - 1
  
  if (state_index == 1) {
    disable("step_backward")
  }
  
  if (state_index < length(state_history)) {
    enable("step_forward")
  }
  
  update_step_string()
  
  visNetworkProxy("network_exec") %>%
    visUpdateNodes(state_history[[state_index]]$nodes) %>%
    visUpdateEdges(state_history[[state_index]]$edges)
})

observeEvent(input$step_end, {
  
})

find.next.rejection <- function(graph) {
  rejectable <- which(graph$tests[, "pval"] < graph$tests[, "alpha"])
  reject <- rejectable[which.min(graph$tests[rejectable, "pval"])]
  
  reject
}

redistribute.alpha <- function(graph, rejected.node) {
  rejected.row <- which(graph$tests[, "id"] == rejected.node)
  
  ### node ###
  
  for (edge.id in unique(graph$edges$id)) {
    edge.row <- which(graph$edge.mat[, "id"] == edge.id)
    if (edge.mat[edge.row, "from"] == rejected.node) {
      target.row <- which(graph$tests[, "id"] == edge.mat[edge.row, "to"])
      tests[target.row, "alpha"] <- tests[target.row, "alpha"] +
        edge.mat[edge.row, "weight"] * tests[rejected.row, "alpha"]
    }
  }
  
  tests[rejected.node, "alpha"] <- NA
  
  graph$description <- paste("Redistributing alpha from", tests[rejected.row, "hypname"])
  graph$tests <- tests
  graph$nodes <- data.frame(id = tests$id,
                            label = construct.node.labels(tests))
  graph$edge.mat <- edge.mat
  graph$edges <- data.frame(id = edge.mat[, "id"],
                            from = edge.mat[, "from"], to = edge.mat[, "to"],
                            label = edge.mat[, "weight"],
                            value = edge.mat[, "weight"])
  
  return(graph)
}

update.edges <- function(graph, rejected.node) {
  
  rejected.row <- which(graph$tests[, "id"] == rejected.node)
  
  graph.new <- graph
  graph.new$edge.mat[, "weight"] <- 0
  graph.new$description <- "Reweighting edges"
  
  for (i in 1 : nrow(graph$edge.mat)) {
    l <- graph$edge.mat[i, "from"]
    m <- graph$edge.mat[i, "to"]
    j <- rejected.node
    
    # l <- tail_of(graph, edge)
    # m <- head_of(graph, edge)
    # j <- V(graph)[rejected.node]
    
    er.lm <- which(graph$edge.mat[, "from"] == l & graph$edge.mat[, "to"] == m)
    er.lj <- which(graph$edge.mat[, "from"] == l & graph$edge.mat[, "to"] == j)
    er.jl <- which(graph$edge.mat[, "from"] == j & graph$edge.mat[, "to"] == l)
    er.jm <- which(graph$edge.mat[, "from"] == j & graph$edge.mat[, "to"] == m)
    
    # e.lm <- get.edge.ids(graph, c(l, m))
    # e.lj <- get.edge.ids(graph, c(l, j))
    # e.jl <- get.edge.ids(graph, c(j, l))
    # e.jm <- get.edge.ids(graph, c(j, m))
    
    
    
    if (l != m &
        !is.na(graph$tests[graph$tests[, "id"] == l, "alpha"]) &
        !is.na(graph$tests[graph$tests[, "id"] == m, "alpha"])
    ) {
      
      if (graph$edge.mat[er.lj, "weight"] * graph$edge.mat[er.jl, "weight"] < 1) {
        graph.new$edge.mat[er.lm, "weight"] <-
          (graph$edge.mat[er.lm, "weight"] +
             graph$edge.mat[er.lj, "weight"] * graph$edge.mat[er.jm, "weight"]) /
          (1 - graph$edge.mat[er.lj, "weight"] * graph$edge.mat[er.jl, "weight"])
      }
    }
    
  }
  
  graph.new$edges <- data.frame(id = graph.new$edge.mat[, "id"],
                      from = graph.new$edge.mat[, "from"], to = graph.new$edge.mat[, "to"],
                      label = graph.new$edge.mat[, "weight"],
                      value = graph.new$edge.mat[, "weight"])
  
  return(graph.new)
}

output$test <- renderText({
  exec_state$step_string
})

update_step_string <- function() {
  string <- "<ol>"
  for (i in 1 : length(state_history)) {
    state <- state_history[[i]]
    string <- paste(
      string,
      "<li>",
      ifelse(i == state_index,
             paste("<strong>", state$description, "</strong>"),
             state$description),
      "</li>"
    )
  }
  string <- paste(string, "</ol>")
  
  exec_state$step_string <<- string
}

disable("step_forward")
disable("step_backward")
disable("step_end")
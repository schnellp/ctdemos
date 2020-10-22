footers <- list(
  default = "Click to select nodes or edges.
             Drag nodes to move.",
  adding_edge = "<span style = 'color: blue; font-weight : bold;'>
  Click a node to choose end of new edge.
  </span>"
)

n.tests <- 3

adding_edge_from <- NULL

selections <- reactiveValues(edge = NULL,
                             node = NULL,
                             type = "none")

tests <- data.frame(
  id = 1 : n.tests,
  hypname = paste0("H", 1 : n.tests),
  alpha = floor(10000 * 0.05 / n.tests) / 10000,
  pval = rep(NA, n.tests),
  stringsAsFactors = FALSE
  )

construct.node.labels <- function(tests) {
  labels <- paste0(tests$hypname,
                   "\nalpha = ", signif(tests$alpha),
                   "\np = ", signif(tests$pval))
  return(labels)
}

nodes <- data.frame(id = tests$id,
                    label = construct.node.labels(tests),
                    x = c(0, -4, 4),
                    y = c(-3, 3, 3))

add_node <- function() {
  new.id <- which.max(!((1 : (nrow(nodes) + 1) %in% nodes$id)))
  
  tests <<- rbind(tests,
                  data.frame(
                    id = new.id,
                    hypname = paste0("H", new.id),
                    alpha = 0,
                    pval = NA,
                    stringsAsFactors = FALSE
                    ))
  
  nodes <<- data.frame(id = tests$id,
                       label = construct.node.labels(tests))
  
  visNetworkProxy("network") %>%
    visUpdateNodes(nodes) %>%
    visSelectNodes(new.id, highlightEdges = TRUE, clickEvent = TRUE)
  
  update_inputs(list(nodes = new.id))
}

update_node_par <- function(id, hypname, alpha, pval) {
  tests[tests[, "id"] == id, "hypname"] <<- hypname
  tests[tests[, "id"] == id, "alpha"] <<- alpha
  tests[tests[, "id"] == id, "pval"] <<- pval
  
  nodes <<- data.frame(id = tests$id,
                      label = construct.node.labels(tests))
  visNetworkProxy("network") %>% visUpdateNodes(nodes)
  
  output$shiny_return <- renderText({
    paste("Family alpha:", sum(tests$alpha))
  })
}

delete_node <- function(id) {
  affected.edges <- edges$id[edges$from == id | edges$to == id]
  for (edge.id in affected.edges) {
    delete_edge(edge.id)
  }
  
  tests <<- tests[tests[, "id"] != id, ]
  nodes <<- data.frame(id = tests$id,
                       label = construct.node.labels(tests))
  
  selections$type <- "none"
  selections$node <- NULL
  disable("hypname")
  disable("alpha")
  disable("pval")
  disable("delete_node")
  disable("add_edge")
  hypname_value <- NA
  alpha_value <- NA
  pval_value <- NA
  
  visNetworkProxy("network") %>% visRemoveNodes(id)
  
  output$shiny_return <- renderText({
    paste("Family alpha:", sum(tests$alpha))
  })
}

edge.mat <- expand.grid(1 : 3, 1 : 3)
edge.mat <- edge.mat[edge.mat[, 1] != edge.mat[, 2], ]

weight <- matrix(1 / (nrow(nodes) - 1), nrow = nrow(nodes), ncol = nrow(nodes))
diag(weight) <- 0

edge.mat <- cbind(1 : nrow(edge.mat), edge.mat, weight[cbind(edge.mat[, 1], edge.mat[, 2])])
colnames(edge.mat) <- c("id", "from", "to", "weight")




edges <- data.frame(id = edge.mat[, "id"],
                    from = edge.mat[, "from"], to = edge.mat[, "to"],
                    label = signif(edge.mat[, "weight"]),
                    value = edge.mat[, "weight"])

add_edge <- function(from, to) {
  new.id <- which.max(!((1 : (nrow(edges) + 1) %in% edges$id)))
  
  edge.mat <<- rbind(edge.mat,
                     c(new.id, from, to, 0))
  
  edges <<- data.frame(id = edge.mat[, "id"],
                       from = edge.mat[, "from"], to = edge.mat[, "to"],
                       label = signif(edge.mat[, "weight"]),
                       value = edge.mat[, "weight"])
  
  visNetworkProxy("network") %>%
    visUpdateEdges(edges) %>%
    visSelectEdges(new.id)
  
  update_inputs(event = list(edges = list(new.id)))
}


update_edge_weight <- function(id, weight) {
  edge.mat[edge.mat[, "id"] == id, "weight"] <<- weight
  edges <<- data.frame(id = edge.mat[, "id"],
                      from = edge.mat[, "from"], to = edge.mat[, "to"],
                      label = signif(edge.mat[, "weight"]),
                      value = edge.mat[, "weight"])
  visNetworkProxy("network") %>% visUpdateEdges(edges)
}

delete_edge <- function(id, update_ui = TRUE) {
  edge.mat <<- edge.mat[edge.mat[, "id"] != id, ]
  edges <<- data.frame(id = edge.mat[, "id"],
                       from = edge.mat[, "from"], to = edge.mat[, "to"],
                       label = signif(edge.mat[, "weight"]),
                       value = edge.mat[, "weight"])
  
  if (update_ui) {
    selections$type <- "none"
    selections$edge <- NULL
    disable("weight")
    disable("delete_edge")
    weight_value <- NA
    
    visNetworkProxy("network") %>% visRemoveEdges(id)
  }
}

output$network <- renderVisNetwork({
  
  visNetwork(nodes, edges, height = "500px",
             footer = footers$default) %>%
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
    visInteraction(selectable = TRUE) %>%
    visEvents(
              click = "function(nodes) {
              Shiny.setInputValue('current_node_id', nodes);}",
              dragStart = "function(nodes) {
              Shiny.setInputValue('current_node_id', nodes);}"
              )
})

update_inputs <- function(event) {
  # alert("update_inputs")
  # visNetworkProxy("network") %>% visGetSelectedNodes() %>% visGetSelectedEdges()
  
  # if (is.null(input$network_selectedNodes)) {
  #   selections$node <- NULL
  #   if (is.null(input$network_selectedEdges)) {
  #     selections$type <- "none"
  #     selections$edge <- NULL
  #   } else {
  #     selections$type <- "edge"
  #     selections$edge <- as.numeric(input$network_selectedEdges)
  #   }
  # } else {
  #   selections$type <- "node"
  #   selections$node <- as.numeric(input$network_selectedNodes)
  # }
  
  if (length(event$nodes) == 0) {
    selections$node <- NULL
    if (length(event$edges) == 0) {
      selections$type <- "none"
      selections$edge <- NULL
    } else {
      selections$type <- "edge"
      selections$edge <- as.numeric(event$edges[[1]])
    }
  } else {
    selections$type <- "node"
    selections$node <- as.numeric(event$nodes[[1]])
  }
  
  if (selections$type == "edge") {
    enable("weight")
    enable("delete_edge")
    weight_value <- as.numeric(edge.mat[edge.mat[, "id"] == selections$edge, "weight"])
  } else {
    disable("add_edge")
    disable("weight")
    weight_value <- NA
  }
  
  if (selections$type == "node") {
    enable("hypname")
    enable("alpha")
    enable("pval")
    enable("delete_node")
    enable("add_edge")
    hypname_value <- tests[tests[, "id"] == selections$node, "hypname"]
    alpha_value <- as.numeric(tests[tests[, "id"] == selections$node, "alpha"])
    pval_value <- as.numeric(tests[tests[, "id"] == selections$node, "pval"])
  } else {
    disable("hypname")
    disable("alpha")
    disable("pval")
    disable("delete_node")
    disable("add_edge")
    hypname_value <- NA
    alpha_value <- NA
    pval_value <- NA
  }
  
  updateNumericInput(session, "weight",
                     value = weight_value)
  
  updateTextInput(session, "hypname",
                  value = hypname_value)
  updateNumericInput(session, "alpha",
                     value = alpha_value)
  updateNumericInput(session, "pval",
                     value = pval_value)
  
  if (!is.null(adding_edge_from)) {
    if (selections$type == "node" && selections$node != adding_edge_from) {
      adding_edge_to <- selections$node
      if (!any(edges$from == adding_edge_from & edges$to == adding_edge_to)) {
        add_edge(adding_edge_from, adding_edge_to)
      }
      
      
    }
    adding_edge_from <<- NULL
  }
  
  if (!is.null(adding_edge_from)) {
    visNetworkProxy("network") %>%
      visSetTitle(footer = footers$adding_edge)
  } else {
    visNetworkProxy("network") %>%
      visSetTitle(footer = footers$default)
  }
  
  
  # print(paste("t", selections$type, "n", selections$node, "e", selections$edge))
}

# observe({
#   input$current_node_id
#   update_inputs()
# })

observeEvent(input$current_node_id, {
  # print(input$current_node_id$nodes)
  # print(input$current_node_id$edges[[1]])
  update_inputs(input$current_node_id)
})

output$shiny_return <- renderText({
  
  paste("Family alpha:", sum(tests$alpha))
})

observeEvent(input$check_validity, {
  
  output.string <- build_validation_string()
  
  if (output.string == "") {
    output.string <- "Graph valid."
  } else {
    output.string <- paste("Warnings:",
                           "<ul>", output.string, "</ul>")
  }
  
  output$warnings <- renderText({output.string})
})

build_validation_string <- function() {
  output.string <- ""
  for (node_id in nodes$id) {
    test_row <- which(tests[, "id"] == node_id)
    
    tot_edge_weights <- sum(edge.mat[edge.mat[, "from"] == node_id, "weight"])
    if (abs(tot_edge_weights - 1) > 1e-4) {
      output.string <- paste(
        output.string,
        "<li>",
        tests[test_row, "hypname"],
        "has total outgoing edge weights",
        tot_edge_weights,
        "</li>"
      )
    }
    if (is.na(tests[test_row, "alpha"])) {
      output.string <- paste(
        output.string,
        "<li>",
        tests[test_row, "hypname"],
        "is missing a value for alpha</li>"
      )
    } else {
      if (tests[test_row, "alpha"] < 0 ||
          tests[test_row, "alpha"] > 1) {
        output.string <- paste(
          output.string,
          "<li>",
          tests[test_row, "hypname"],
          "has invalid value for alpha: must be in [0, 1]</li>"
        )
      }
    }
    
    if (is.na(tests[test_row, "pval"])) {
      output.string <- paste(
        output.string,
        "<li>",
        tests[test_row, "hypname"],
        "is missing a p-value</li>"
      )
    } else {
      if (tests[test_row, "pval"] <= 0 ||
          tests[test_row, "pval"] >= 1) {
        output.string <- paste(
          output.string,
          "<li>",
          tests[test_row, "hypname"],
          "has invalid p-value: must be in (0, 1)</li>"
        )
      }
    }
    
  }
  
  output.string
}


output$view_manip <- renderPrint({
  input$network_manip_graphChange
})

### Node editing ###

observeEvent(input$add_node, {
  add_node()
})

observeEvent(input$hypname, {
  if (selections$type == "node") {
    update_node_par(as.numeric(selections$node),
                    input$hypname,
                    as.numeric(input$alpha),
                    as.numeric(input$pval))
  }
})

observeEvent(input$alpha, {
  if (selections$type == "node") {
    update_node_par(as.numeric(selections$node),
                    input$hypname,
                    as.numeric(input$alpha),
                    as.numeric(input$pval))
  }
})

observeEvent(input$pval, {
  if (selections$type == "node") {
    update_node_par(as.numeric(selections$node),
                    input$hypname,
                    as.numeric(input$alpha),
                    as.numeric(input$pval))
  }
})

observeEvent(input$delete_node, {
  if (selections$type == "node") {
    delete_node(as.numeric(selections$node))
  }
})

### Edge editing ###

observeEvent(input$add_edge, {
  if (selections$type == "node") {
    
    
    # visNetworkProxy("network") %>%
      # visUpdateNodes(data.frame(id = selections$node, color = "lightgreen"))
      # visUnselectAll()
    
    adding_edge_from <<- selections$node
    
    visNetworkProxy("network") %>%
      visSetTitle(footer = footers$adding_edge)
    # update_inputs()
  }
})

observeEvent(input$weight, {
  if (selections$type == "edge") {
    update_edge_weight(as.numeric(selections$edge), as.numeric(input$weight))
  }
})

observeEvent(input$delete_edge, {
  if (selections$type == "edge") {
    delete_edge(as.numeric(selections$edge))
  }
})

disable("hypname")
disable("alpha")
disable("pval")
disable("delete_node")
disable("add_edge")

disable("add_edge")
disable("weight")
disable("delete_edge")
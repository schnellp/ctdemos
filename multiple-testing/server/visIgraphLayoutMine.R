
### adapted from visNetwork::visIgraphLayout ###

visIgraphLayoutMine <- function(graph,
                            layout = "layout_nicely",
                            physics = FALSE, 
                            smooth = FALSE,
                            type = "square", 
                            randomSeed = NULL, 
                            layoutMatrix = NULL, ...){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visIgraphLayout with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  if(!all(c("nodes", "edges") %in% names(graph$x))){
    stop("Need 'nodes' and 'edges' informations on network")
  }
  
  if(!type %in% c("square", "full")){
    stop("type must be one of 'square' or 'full'")
  }
  
  if(!requireNamespace("igraph", quietly = TRUE)){
    stop("This function need 'igraph' package to compute layout. Please 
         install it before.")
  }
  
  ctrl <- getAnywhere(layout)
  if(length(ctrl$objs) == 0){
    stop("Can't find '", layout, "' function. Please verify it")
  }
  
  if(!is.function(ctrl$objs[[1]])){
    stop("'", layout, "' must be a function.")
  }
  
  igraphlayout <- list(type = type)
  
  ig <- igraph::graph_from_data_frame(graph$x$edges[, c("from", "to")], directed = TRUE, 
                                      vertices = graph$x$nodes[, c("id", setdiff(names(graph$x$nodes), "id"))])
  
  if(!is.null(randomSeed)){
    set.seed(randomSeed)
  }
  if("layout.norm" %in% layout){
    if (is.null(layoutMatrix)) {
      stop("'layout.norm' requires a layout argument (a matrix with two or three columns), passed by layoutMatrix argument")
    }
    coord <- ctrl$objs[[1]](layout = layoutMatrix, ...)
  } else if("layout_with_sugiyama" %in% layout){
    coord <- ctrl$objs[[1]](graph = ig, ...)$layout
    coord[, 2] <- max(coord[, 2]) - coord[, 2] + 1
  } else {
    coord <- ctrl$objs[[1]](graph = ig, ...)
  }
  
  graph$x$nodes$x <- coord[, 1]
  graph$x$nodes$y <- coord[, 2]
  
  # to <- c(-1, 1)
  # from <- range(graph$x$nodes$x, na.rm = TRUE, finite = TRUE)
  # if(length(unique(from)) > 1){
  #   graph$x$nodes$x <- (graph$x$nodes$x - from[1])/diff(from) * diff(to) + to[1]
  # }
  # 
  # from <- range(graph$x$nodes$y, na.rm = TRUE, finite = TRUE)
  # if(length(unique(from)) > 1){
  #   graph$x$nodes$y <- (graph$x$nodes$y - from[1])/diff(from) * diff(to) + to[1]
  # }
  
  # graph$x$nodes$physics = physics
  
  graph$x$igraphlayout <- igraphlayout
  
  graph %>% visNodes(physics = physics) %>%
    visEdges(smooth = smooth) %>% visPhysics(stabilization = FALSE)
  }
#' S3 method returngraph
returngraph <- function(x,
                        type = "task",
                        neighbouroption = "task",
                        subID = NULL,
                        index = NULL) {
  UseMethod("returngraph", x, type, neighbouroption, subID, index)

}


#' return igraph object from jeek result specified by user input
#'
#' This function can return an igraph object from jeek result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer)
#' @param x output generated from jeek function (jeek class)
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @return an igraph object of graph / subgraph from jeek result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from jeek
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = jeek(X = exampleData, 0.3, covType = "cov", parallel = TRUE)
#' graph = returngraph(result)
#' }
#' @export
#' @import igraph
returngraph.jeek <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {
    return(.returngraph_jointnets(x,type,neighbouroption,subID,index))
}

#' return graph from simule result
#'
#' This function can return an igraph object from simule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer)
#' @param x output generated from simule function (simule class)
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @return an igraph object of graph / subgraph from simule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from simule
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", FALSE)
#' returngraph.simule(result)
#' }
#' @export
#' @import igraph
returngraph.simule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {
    return(.returngraph_jointnets(x,type,neighbouroption,subID,index))
  }

#' return graph from wsimule result
#'
#' return graph from simule result
#'
#' This function can return an igraph object from simule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer)
#' @param x output generated from simule function (simule class)
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @return an igraph object of graph / subgraph from simule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from simule
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", FALSE)
#' returngraph.simule(result)
#' }
#' @export
#' @import igraph
returngraph.wsimule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {
    return(.returngraph_jointnets(x,type,neighbouroption,subID,index))
}

#' return graph from fasjem result
#'
#' return graph from simule result
#'
#' This function can return an igraph object from simule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer)
#' @param x output generated from simule function (simule class)
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @return an igraph object of graph / subgraph from simule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from simule
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", FALSE)
#' returngraph.simule(result)
#' }
#' @export
#' @import igraph
returngraph.fasjem <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {
    return(.returngraph_jointnets(x,type,neighbouroption,subID,index))
}

#' return graph from diffee result
#'
#' This function can return an igraph object from simule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer)
#' @param x output generated from simule function (simule class)
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @return an igraph object of graph / subgraph from simule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from simule
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", FALSE)
#' returngraph.simule(result)
#' }
#' @export
#' @import igraph
returngraph.diffee <-
  function(x,
           type = "task",
           index = NULL) {
    ### diffee only has difference graph
    if (!(type == "task" | type == "neighbour")) {stop("please specify a correct type")}
    return(.returngraph_jointnets(x,type,"task",NULL,index))
  }

#' core function to return graph from jointnets result (core method for S3 method returngraph.<methodname>)
.returngraph_jointnets <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {

    adj = .make.adj.matrix(x)
    diag(adj) = 0
    gadj = graph.adjacency(adj, mode = "upper", weighted = TRUE)
    K = length(x)


    if (!is.null(E(gadj)$weight)) {E(gadj)$color = grDevices::rainbow(K+1)[E(gadj)$weight]}
    ### ignore subID and index
    if (type == "share") {gadj = subgraph.edges(gadj, which(E(gadj)$weight == K + 1), delete.vertices = FALSE)}

    else if (type == "taskspecific") {
      ### ignore index
      if (0 %in% subID) {
        stop("please specify valid task number(s)")
      }
      if (is.null(subID)) {
        stop("please specify task number(s)")
      }
      if (!prod(subID %in% (1:K))) {
        stop("please specify valid task number(s)")
      }
      gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% subID), delete.vertices = FALSE)
    }

    else if (type == "task") {
      if (!is.null(subID)) {
        if (!prod(subID %in% (0:K))) {
          stop("please specify valid task number(s)")
        }
        ### when subID = 0, gadj will be shared graph
        ### when subID = others, gadj will be graph for task with subID (including shared part)
        gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% c(subID, K + 1)), delete.vertices = FALSE)
      }
      ### when subID is null, return all graphs
    }

    else if (type == "neighbour") {
      try(if (!prod(index %in% (1:vcount(gadj)))) {stop("please specify valid index number(s)")})

      gadj = subgraph.edges(gadj, unlist(incident_edges(gadj, index)) , delete.vertices = FALSE)
      if (neighbouroption == "task") {
        if (!is.null(subID)) {
          try (if (!prod(subID %in% (0:K))) {stop("please specify valid task number(s)")})
          gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% c(subID, K + 1)), delete.vertices = FALSE)
        }
      }

      else if (neighbouroption == "taskspecific") {
        try (if (!prod(subID %in% (1:K))) {stop("please specify valid task number(s)")})
        gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% subID), delete.vertices = FALSE)
      }

      else {
        stop("please specify a valid neighbouroption")
      }
    }

    else {
      stop("please specify a correct type")
    }

    return(gadj)
  }

#' helper function to make adjacency matrix
#' @param theta result from jointnets
.make.adj.matrix <-
  function(theta, separate=FALSE)
  {
    K = length(theta)
    adj = list()
    if(separate)
    {
      for(k in 1:K)
      {
        adj[[k]] = (abs(theta[[k]])>1e-5)*1
      }
    }
    if(!separate)
    {
      adj = 0*theta[[1]]
      for(k in 1:K)
      {
        adj = adj+(abs(theta[[k]])>1e-5)*2^(k-1)
      }
    }
    return(adj)
  }

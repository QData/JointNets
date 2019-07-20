#' return igraph object from jointnet result specified by user input
#'
#' This function returns an igraph object from jointnet result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from any one of the jointnet functions
#' @param ... additional arguments, see \code{\link{returngraph.simule}}, \code{\link{returngraph.wsimule}},
#' \code{\link{returngraph.diffee}}, \code{\link{returngraph.jeek}} for details.
#' @return an igraph object of graph / subgraph from jointnet result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from jointnet
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = jeek(X = exampleData, 0.3, covType = "cov", parallel = FALSE)
#' graph = returngraph(result)
#' @export
returngraph <- function(x, ...) {
  UseMethod("returngraph", x)
}

#' return igraph object from jeek result specified by user input
#'
#' This function can return an igraph object from jeek result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from jeek function (jeek class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... not used
#' @return an igraph object of graph / subgraph from jeek result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from jeek
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = jeek(X = exampleData, 0.3, covType = "cov", parallel = FALSE)
#' graph = returngraph(result)
#' @export
#' @import igraph
returngraph.jeek <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    return(returngraph_jointnets(x, type, neighbouroption, subID, index))

  }

#' return igraph object from simule result specified by user input
#'
#' This function can return an igraph object from simule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from simule function (simule class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... not used
#' @return an igraph object of graph / subgraph from simule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from simule
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", FALSE)
#' graph = returngraph(result)
#' @export
#' @import igraph
returngraph.simule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    return(returngraph_jointnets(x, type, neighbouroption, subID, index))

  }

#' return igraph object from wsimule result specified by user input
#'
#' This function can return an igraph object from wsimule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from wsimule function (wsimule class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... not used
#' @return an igraph object of graph / subgraph from wsimule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from wsimule
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = wsimule(X = exampleData , lambda = 0.1, epsilon = 0.45,
#' W = matrix(1,20,20), covType = "cov", FALSE)
#' graph = returngraph(result)
#' @export
#' @import igraph
returngraph.wsimule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    return(returngraph_jointnets(x, type, neighbouroption, subID, index))

  }


#' return igraph object from simulation result specified by user input
#'
#' This function can return an igraph object from simulation result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from simulatino functino
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... not used
#' @return an igraph object of graph / subgraph from simulation result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from simulation
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = simulation(n=c(100,100,100))$simulatedgraphs
#' graph = returngraph(result)
#' @export
#' @import igraph
returngraph.simulation <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    return(returngraph_jointnets(x, type, neighbouroption, subID, index))
  }


#' return igraph object from diffee result specified by user input
#'
#' This function can return an igraph object from diffee result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from diffee function (diffee class)
#' @param type type of graph. There are two options:
#' * "task" (difference graph)
#' * "neighbour" (zoom into nodes in the difference graph specified further by parameter
#' **"index"** (node id)
#' @param neighbouroption unused
#' @param subID unused
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... unused
#' @return an igraph object of graph / subgraph from diffee result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph own their own
#' generated from diffee
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = diffee(exampleData[[1]], exampleData[[2]], 0.45)
#' graph = returngraph(result)
#' @export
#' @import igraph
returngraph.diffee <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    ### diffee only has difference graph
    if (!(type == "task" |
          type == "neighbour")) {
      stop("please specify a correct type")
    }
    return(returngraph_jointnets(x, type, "task", NULL, index))
  }



#' return igraph object from kdiffnet result specified by user input
#'
#' This function can return an igraph object from kdiffnet result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from kdiffnet function (kdiffnet class)
#' @param type type of graph. There are two options:
#' * "task" (difference graph)
#' * "neighbour" (zoom into nodes in the difference graph specified further by parameter
#' **"index"** (node id)
#' @param neighbouroption unused
#' @param subID unused
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... unused
#' @return an igraph object of graph / subgraph from kdiffnet result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph own their own
#' generated from kdiffnet
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = kdiffnet(exampleData[[1]], exampleData[[2]],
#' W = matrix(1,20,20), g = rep(0,20),epsilon = 0.2,
#' lambda = 0.4,covType = "cov")
#' graph = returngraph(result)
#' @export
#' @import igraph
returngraph.kdiffnet <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    ### kdiffnet only has difference graph
    if (!(type == "task" |
          type == "neighbour")) {
      stop("please specify a correct type")
    }
    return(returngraph_jointnets(x, type, "task", NULL, index))
  }


#' return igraph object from jgl result specified by user input
#'
#' This function can return an igraph object from jgl result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from jgl function (jgl class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param ... not used
#' @return an igraph object of graph / subgraph from jgl result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph on their own
#' generated from jgl
#' @export
#' @import igraph
#' @examples
#' library(JointNets)
#' data(exampleData)
#' result = jgl(X = exampleData , lambda1 = 1, lambda2 = 1)
#' graph = returngraph(result)
returngraph.jgl <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           ...) {
    return(returngraph_jointnets(x, type, neighbouroption, subID, index))

  }

returngraph_jointnets <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {
    adj = make_adj_matrix(x)
    diag(adj) = 0
    gadj = graph.adjacency(adj, mode = "upper", weighted = TRUE)
    if (!is.null(colnames(x$graphs[[1]]))) {
      V(gadj)$label = colnames(x$graphs[[1]])
    }
    K = length(x$graphs)

    if (!is.null(E(gadj)$weight)) {
      E(gadj)$color = grDevices::rainbow(K + 1)[E(gadj)$weight]
    }
    ### ignore subID and index
    if (type == "share") {
      gadj = subgraph.edges(gadj, which(E(gadj)$weight == K + 1), delete.vertices = FALSE)
    }

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
      try(if (!prod(index %in% (1:vcount(gadj)))) {
        stop("please specify valid index number(s)")
      })

      gadj = subgraph.edges(gadj, unlist(incident_edges(gadj, index)) , delete.vertices = FALSE)
      if (neighbouroption == "task") {
        if (!is.null(subID)) {
          try (if (!prod(subID %in% (0:K))) {
            stop("please specify valid task number(s)")
          })
          gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% c(subID, K + 1)), delete.vertices = FALSE)
        }
      }

      else if (neighbouroption == "taskspecific") {
        try (if (!prod(subID %in% (1:K))) {
          stop("please specify valid task number(s)")
        })
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

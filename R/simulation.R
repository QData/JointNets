#' function to simulate multiple sparse graphs
#' @param p number of features
#' @param N number of tasks
#' @param seedNum seed number for random simulation
#' @param s controls sparsity of the generated graph
#' @param ss controls sparsity of the generated graph
#' @return a list of N related sparse pXp precision matrices (graphs)
#' @import MASS
#' @importFrom stats rbinom
simulateGraph <-
  function(p = 20,
           N = 2,
           seedNum = 37,
           s = 0.1,
           ss = 0.1) {
    #library(MASS)
    #set parameters
    # p = 1000
    # N = 3
    set.seed(seedNum)
    graphs <- list()
    I = diag(1, p, p)
    graph_shared = matrix(1, p, p)
    for (j in 1:p) {
      for (k in j:p) {
        graph_shared[j, k] = 0.5 * rbinom(1, 1, ss)
        graph_shared[k, j] = graph_shared[j, k]
      }
    }
    #generate the simulate graph
    #first one is the all off diag element has 0.1*N to be 0.5 and others to be 0
    for (i in 1:N) {
      graphs[[i]] <- matrix(1, p, p)
    }
    for (i in 1:N) {
      for (j in 1:p) {
        for (k in j:p) {
          graphs[[i]][j, k] = 0.5 * rbinom(1, 1, s * N)
          graphs[[i]][k, j] = graphs[[i]][j, k]
        }
      }
      graphs[[i]] = graphs[[i]] + graph_shared
    }

    for (i in 1:N) {
      for (j in 1:p) {
        graphs[[i]][j, j] = 1
      }
    }

    for (i in 1:N) {
      graphs[[i]] = (graphs[[i]] - I) + (abs(min(eigen(graphs[[i]] - I)$value)) + 1) * I
    }

    out = list(graphs = graphs, share = graph_shared)
    class(out) = "simulation"
    return(out)
  }


#' function to generate samples from a single precision matrix
#' @param precision pxp precision matrix (generated from simulateGraph)
#' @param n number of samples
#' @return a list of nXp randomly generated gaussian samples from pxp precision matrix
generateSamples <-
  function(precision,
           n = 100)
  {
    invg <- solve(precision)
    mu = matrix(0, 1, dim(precision)[1])
    samples = mvrnorm(n = n,
                      mu = mu,
                      Sigma = invg)
    return(samples)
  }

#' function to generate a list of samples from simulatedGraph result
#' @param simulate result from simulateGraph
#' @param n a vector of corresponding size to indicate number of samples for each task
#' @return a list of length(n) data matrices
#' @details if n is c(100,200,300) and p is 20, the function will return a list of 3 data matrices of size (100x20,200x20,300x20)
generateSampleList <-
  function(simulate, n)
  {
    K = length(simulate$graphs)
    out = list()
    for (i in 1:K) {
      out[[i]] = generateSamples(simulate$graphs[[i]], n[i])
    }
    return(out)
  }



# this is the main function

#' simulate multiple sparse graphs and generate samples
#' @param p number of features (number of nodes)
#' @param n a vector indicating number of samples and tasks, for example c(100,200,300) for 3 tasks and 100,200 and 300 samples for task 1, 2 and 3
#' @param seedNum seed number for random simulation
#' @param s positive number that controls sparsity of the generated graphs
#' @param ss positive number that controls sparsity of the shared part of generated graphs
#' @return a list comprising $simulatedgraphs (multiple related simulated graphs) and $simulatedsamples (samples generated from multiple related graphs)
#' @import MASS
#' @export
#' @examples
#' library(JointNets)
#' simulateresult = simulation(p = 20,  n = c(100,100))
#' plot(simulateresult$simulatedgraphs)
simulation <- function(p = 20,
                     n,
                     seedNum = 37,
                     s = 0.1,
                     ss = 0.1
                     )
  {
  N = length(n)
  graphs = simulateGraph(p,N,seedNum,s,ss)
  samples = generateSampleList(graphs,n)
  out = list(simulatedgraphs = graphs, simulatedsamples = samples)
  return(out)
}

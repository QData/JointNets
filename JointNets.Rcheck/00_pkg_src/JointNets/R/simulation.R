#' function to simulate multiple sparse graphs
#' @param p number of features
#' @param N number of tasks
#' @param seedNum seed number for random simulation
#' @param s controls sparsity of the generated graph
#' @param ss controls sparsity of the generated graph
#' @return a list of N related sparse pXp precision matrices (graphs)
#' @import MASS
#' @export
simulateGraph <-
  function(p = 20,
           N = 2,
           seedNum = 37,
           s = 0.1,
           ss = 0.1) {
    library(MASS)
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


#' function to generate samples from precision matrix
#' @param precision precision matrix generated from simulateGraph
#' @param n number of samples
#' @param ... extra parameter passed to mvrnorm
#' @return a list of nXp randomly generated gaussian samples from precision matrix
#' @export
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
#' @export
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

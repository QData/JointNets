#' function to plot comparsion plot between result and simulationresultd graph
#' @param result result generated from any of the jointnet methods
#' @param simulationresult simulationresultd result from simulationresultGraph
#' @examples
#' \dontrun{
#' library(JointNets)
#' simulationresult = simulation(p = 20,  n = c(100,100))
#' result = simule(simulationresult$simulatedsamples, 0.2, 0.5, covType = "cov", TRUE)
#' comparisonplot(result,simulationresult)
#' }
#' @export
comparisonplot <-
  function(result,simulationresult) {
    UseMethod("comparisonplot", result)
  }

#' function to plot comparsion plot between result and simulationresultd graph
#' @method comparisonplot simule
#' @export
#' @export comparisonplot simule
#' @examples
#' \dontrun{
#' library(JointNets)
#' simulationresult = simulation(p = 20,  n = c(100,100))
#' result = simule(simulationresult$simulatedsamples, 0.2, 0.5, covType = "cov", TRUE)
#' comparisonplot(result,simulationresult)
#' }
comparisonplot.simule <-
  function(result,
           simulationresult)
  {
    K = length(simulationresult$simulatedgraphs$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2, K + 1))

    for (i in 1:K) {
      plot(
        simulationresult$simulatedgraphs,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(simulationresult$simulatedgraphs, type = "share", layout = layout)

    for (i in 1:K) {
      plot(
        result,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(result, type = "share", layout = layout)


  }

#' function to plot comparsion plot between result and simulationresultd graph
#' @method comparisonplot wsimule
#' @export
#' @export comparisonplot wsimule
#' @examples
#' \dontrun{
#' library(JointNets)
#' simulationresult = simulation(p = 20,  n = c(100,100))
#' result = wsimule(simulationresult$simulatedsamples, 0.2, 1, W = matrix(1,20,20), covType = "cov", TRUE)
#' comparisonplot(result,simulationresult)
#' }
comparisonplot.wsimule <-
  function(result,
           simulationresult)
  {
    K = length(simulationresult$simulatedgraphs$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2, K + 1))

    for (i in 1:K) {
      plot(
        simulationresult$simulatedgraphs,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(simulationresult$simulatedgraphs, type = "share", layout = layout)

    for (i in 1:K) {
      plot(
        result,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(result, type = "share", layout = layout)


  }

#' function to plot comparsion plot between result and simulationresultd graph
#' @method comparisonplot fasjem
#' @export
#' @export comparisonplot fasjem
#' @examples
#' \dontrun{
#' library(JointNets)
#' simulationresult = simulation(p = 20,  n = c(100,100))
#' result = fasjem(simulationresult$simulatedsamples, method = "fasjem-g", 2, 0.1, 0.1, 0.05, 20)
#' comparisonplot(result,simulationresult)
#' }
comparisonplot.fasjem <-
  function(result,
           simulationresult)
  {
    K = length(simulationresult$simulatedgraphs$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2, K))

    for (i in 1:K) {
      plot(
        simulationresult$simulatedgraphs,
        type = "task",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }


    for (i in 1:K) {
      plot(
        result,
        type = "task",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }


  }

#' function to plot comparsion plot between result and simulationresult graph
#' @method comparisonplot jeek
#' @export
#' @export comparisonplot jeek
#' @examples
#' \dontrun{
#' library(JointNets)
#' simulationresult = simulation(p = 20,  n = c(100,100))
#' result = jeek(simulationresult$simulatedsamples,0.25,covType = "kendall",parallel = TRUE)
#' comparisonplot(result,simulationresult)
#' }
comparisonplot.jeek <-
  function(result,
           simulationresult)
  {
    K = length(simulationresult$simulatedgraphs$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2,K))

    for (i in 1:K) {
      plot(
        simulationresult$simulatedgraphs,
        type = "task",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }


    for (i in 1:K) {
      plot(
        result,
        type = "task",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }


  }

#' function to plot comparsion plot between result and simulationresult graph
#' @method comparisonplot diffee
#' @export
#' @export comparisonplot diffee
#' @examples
#' \dontrun{
#' library(JointNets)
#' simulationresult = simulation(p = 20,  n = c(100,100))
#' result = diffee(simulationresult$simulatedsamples[[1]], simulationresult$simulatedsamples[[2]], 1)
#' comparisonplot(result,simulationresult)
#' }
comparisonplot.diffee <-
  function(result,
           simulationresult)
  {
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2,1))

      plot(
        simulationresult$simulatedgraphs,
        type = "taskspecific",
        subID = c(1,2),
        layout = layout,
        main = "simulation difference graph"
      )

      plot(
        result,
        layout = layout
      )
  }


#' function to plot comparsion plot between result and simulationresult graph
#' @method comparisonplot diffeek
#' @export
#' @export comparisonplot diffeek
comparisonplot.diffeek <-
  function(result,
           simulationresult)
  {
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2,1))

    plot(
      simulationresult$simulatedgraphs,
      type = "taskspecific",
      subID = c(1,2),
      layout = layout,
      main = "simulation difference graph"
    )

    plot(
      result,
      layout = layout
    )



  }

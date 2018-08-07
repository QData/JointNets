comparisonplot <-
  function(result) {
    UseMethod("comparisonplot", result)
  }


#' function to plot comparsion plot between result and simulated graph
#' @param result result generated from any of the jointnet methods
#' @param simulate simulated result from simulateGraph
comparisonplot.simule <-
  function(result,
           simulate)
  {
    K = length(simulate$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2, K + 1))

    for (i in 1:K) {
      plot(
        simulate,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(simulate, type = "share", layout = layout)

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


comparisonplot.wsimule <-
  function(result,
           simulate)
  {
    K = length(simulate$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2, K + 1))

    for (i in 1:K) {
      plot(
        simulate,
        type = "taskspecific",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )
    }
    plot(simulate, type = "share", layout = layout)

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


comparisonplot.fasjem <-
  function(result,
           simulate)
  {
    K = length(simulate$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2, K))

    for (i in 1:K) {
      plot(
        simulate,
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


comparisonplot.jeek <-
  function(result,
           simulate)
  {
    K = length(simulate$graphs)
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2,K))

    for (i in 1:K) {
      plot(
        simulate,
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


comparisonplot.diffee <-
  function(result,
           simulate)
  {
    graphics.off()
    par(ask = F)
    graph = returngraph(result)
    layout = layout_nicely(graph, dim = 2)
    par(mfrow = c(2,1))

  ### need to revise on this
      plot(
        simulate,
        type = "task",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )

      plot(
        result,
        type = "task",
        neighbouroption = "task",
        subID = i,
        layout = layout
      )



  }


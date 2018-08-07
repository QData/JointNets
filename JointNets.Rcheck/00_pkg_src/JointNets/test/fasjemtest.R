library(JointNets)
graphics.off()
par(ask = F)
par(mfrow = c(1, 1))


readline(prompt = " JEEK TEST ")


readline(prompt = " CANCER DATA")

{
  data(cancer)
  cancerlist = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]),
                    as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))
  result = fasjem(X = cancerlist, method = "fasjem-g", 2, 0.1, 0.1, 0.05, 20)

  label = colnames(cancer[[1]])
  graph = returngraph(result)
  layout = layout_nicely(graph, dim = 2)
  par(mfrow = c(3, 3))
  plot(result,
       type = "task",
       layout = layout)
  plot(result,
       type = "share",
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 1,
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 2,
       layout = layout)
  nodeid = which(label %in% c("MELK", "E2F3", "BB_S4")) ### look for id

  plot(result,
       type = "neighbour",
       index = nodeid,
       layout = layout)

  plot(
    result,
    type = "neighbour",
    subID = 0,
    index = nodeid,
    layout = layout
  )

  plot(
    result,
    type = "neighbour",
    neighbouroption = "taskspecific",
    subID = 1,
    index = nodeid,
    layout = layout
  )

  plot(
    result,
    type = "neighbour",
    neighbouroption = "taskspecific",
    subID = 2,
    index = nodeid,
    layout = layout
  )

}

readline(prompt = " SYNTHETIC DATA")

{
  data(exampleData)
  result = fasjem(X = exampleData, method = "fasjem-g", 0.5, 0.1, 0.1, 0.05, 10)
  graph = returngraph(result)
  layout = layout_nicely(graph, dim = 2)
  par(mfrow = c(3, 3))

  data(exampleDataGraph)
  plot(result,  type = "share", layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 1,
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 2,
       layout = layout)
  plot(
    exampleDataGraph[[1]],
    layout = layout,
    vertex.label.font = 2,
    vertex.shape = "none",
    vertex.label.color = "gray40",
    vertex.label.cex = .7,
    vertex.frame.color = "white",
    edge.color = rainbow(3)[3],
    vertex.size = 10 ,
    main = "shared groundtruth"
  )
  plot(
    exampleDataGraph[[2]],
    layout = layout,
    vertex.label.font = 2,
    vertex.shape = "none",
    vertex.label.color = "gray40",
    vertex.label.cex = .7,
    vertex.frame.color = "white",
    edge.color = rainbow(3)[1],
    vertex.size = 10 ,
    main = "task 1 specific groundtruth"
  )
  plot(
    exampleDataGraph[[3]],
    layout = layout,
    vertex.label.font = 2,
    vertex.shape = "none",
    vertex.label.color = "gray40",
    vertex.label.cex = .7,
    vertex.frame.color = "white",
    edge.color = rainbow(3)[2],
    vertex.size = 10 ,
    main = "task 2 specific groundtruth"
  )
  nodeid = c(3, 8) ### node id
  plot(
    result,
    type = "neighbour",
    neighbouroption = "taskspecific",
    subID = 1,
    index = nodeid,
    layout = layout
  )
  plot(
    result,
    type = "neighbour",
    neighbouroption = "taskspecific",
    subID = 2,
    index = nodeid,
    layout = layout
  )
}

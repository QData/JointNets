library(JointNets)

graphics.off()
par(ask = FALSE)
par(mfrow = c(1, 1))

readline(prompt = "Press [enter] to continue to cancer demo with 2 tasks (not v. pcr) and 26 features (26 cancer types) ")

data(cancer)
cancerlist = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"), ]),
                  as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"), ]))

result = jgl(cancerlist,0.1,0.5)
label = colnames(cancer[[1]])
graph = returngraph(result)
layout = layout_nicely(graph, dim = 2)
#plot(result)

readline(prompt = "Press [enter] to display four plots showing all graphs, shared graph, task specific 1 and task specific 2")


{
  par(mfrow = c(2, 2))
  plot(result, type = "task", layout = layout)
  plot(result, type = "share", layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 1,
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 2,
       layout = layout)
}


readline(prompt = "Press [enter] to display four plots zooming into node MELK, E2F3 and BB_S4 on previous four plots")


{
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

readline(prompt = "Press [enter] to continue to NIPS word count demo with 2 tasks (before 2006 and after 2006) and 37 features (37 words)")

data(nip_37_data)
label = colnames(nip_37_data[[1]])
result = jgl(nip_37_data,0.8,0.1)
graph = returngraph(result)
layout = layout_nicely(graph, dim = 2)

readline(prompt = "Press [enter] to display four plots showing all graphs, shared graph, task specific 1 and task specific 2")


{
  par(mfrow = c(2, 2))
  plot(result, type = "task", layout = layout)
  plot(result, type = "share", layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 1,
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 2,
       layout = layout)
}


readline(prompt = "Press [enter] to display four plots zooming into node data and probability on previous four plots")


{
  nodeid = which(label %in% c("data", "probability")) ### look for id
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


readline(prompt = "Press [enter] to continue to synthetic Gaussian data demo with 2 tasks and 20 features")


data(exampleData)
result = jgl(exampleData,0.1,0.01)
graph = returngraph(result)
layout = layout_nicely(graph, dim = 2)
label = NULL

readline(prompt = "Press [enter] to view the four plots showing all graphs, shared graph, task 1 and task 2 specific graphs")

{
  par(mfrow = c(2, 2))
  plot(result,  type = "task", layout = layout)
  plot(result,  type = "share", layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 1,
       layout = layout)
  plot(result,
       type = "taskspecific",
       subID = 2,
       layout = layout)
}

### comparsion display

readline(prompt = "Press [enter] to view the comparsion between simulated graphs and ground truth graphs")
par(mfrow = c(2, 3))
data(exampleDataGraph)

{
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
}


readline(prompt = "Press [enter] to view plots zooming into node 3 and 8 in different graphs")
nodeid = c(3, 8) ### node id

{
  par(mfrow = c(2, 2))
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

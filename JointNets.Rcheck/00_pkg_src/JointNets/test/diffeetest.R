library(JointNets)
par(mfrow = c(1, 1))



readline(prompt = "DIFFEE Test")

### cancer data
readline(prompt = "CANCER DATA")
{
  data(cancer)
  cancerlist = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"), ]),
                    as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"), ]))
  result = diffee(cancerlist[[1]], cancerlist[[2]], 40)
  label = colnames(cancer[[1]])
  graph = returngraph(result)
  layout = layout_nicely(graph, dim = 2)
  graphics.off()
  par(mfrow = c(3, 3))

  ### plot 7 tests
  plot.diffee(result, layout = layout)
  plot(result, layout = layout)
  plot(result, vertex.label = 1, layout = layout)
  plot(result, main = "hellp", layout = layout)
  plot(result,
       kkk = 1,
       main = "hellp",
       layout = layout)
  plot(result, index = which(label %in% c("CA12", "BB_S4")), layout = layout)
  plot(
    result,
    type = "neighbour",
    index = which(label %in% c("CA12", "BB_S4")),
    layout = layout
  )

}


### example data (compare with ground truth)
readline(prompt = "EXAMPLE DATA")
{
  data(exampleData)
  result = diffee(exampleData[[1]], exampleData[[2]], lambda = 0.45)
  data(exampleDataGraph)
  layout = layout_nicely(exampleDataGraph[[1]], dim = 2)

  graphics.off()
  par(mfrow = c(1, 2))


  ### plot comparsion
  plot.diffee(result, layout = layout)
  plot(
    union(exampleDataGraph[[2]], exampleDataGraph[[3]]),
    layout = layout,
    vertex.label.font = 2,
    vertex.shape = "none",
    vertex.label.color = "gray40",
    vertex.label.cex = .7,
    vertex.frame.color = "white",
    vertex.size = 10 ,
    edge.color = rainbow(1)[1],
    main = "difference graph ground truth"
  )
}

library(JointNets)



readline(prompt="DIFFEE Test")



data(cancer)
cancerlist = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]),
                  as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))
result = diffee(cancerlist[[1]], cancerlist[[2]], 40)
label = colnames(cancer[[1]])
graph = returngraph(result)
layout = layout_nicely(graph,dim=2)



readline(prompt="CANCER DATA")



{
graphics.off()
par(mfrow=c(3,3))
plot.diffee(result, layout = layout)
plot(result, layout = layout)
plot(result, vertex.label = 1,layout = layout)
plot(result, main = "hellp",layout = layout)
plot(result, kkk = 1, main = "hellp", layout = layout)
plot(result, index = which(label %in% c("CA12","BB_S4")), layout = layout)
plot(result, type = "neighbour", index = which(label %in% c("CA12","BB_S4")), layout = layout)
}



readline(prompt="EXAMPLE DATA")



data(exampleData)
result = diffee(exampleData[[1]], exampleData[[2]], lambda = 0.45, covType = "cov", thre = "soft")
data(exampleDataGraph)
layout = layout_nicely(exampleDataGraph[[1]], dim = 2)


{
  graphics.off()
  par(mfrow=c(3,3))
  plot.diffee(result,layout = layout)
  plot(union(exampleDataGraph[[2]], exampleDataGraph[[3]]),layout = layout,vertex.label.font=2,
              vertex.shape="none",
              vertex.label.color="gray40",
              vertex.label.cex=.7, vertex.frame.color="white", vertex.size = 10 ,
              edge.color = rainbow(1)[1],
              main = "difference graph ground truth")
}





if(FALSE)
{
### plot the estimated graphs by diffee


readline(prompt="Press [enter] to continue to synthetic Gaussian data demo with 2 tasks and 20 features")


### load the  example data
data(exampleData)
result = diffee(exampleData[[1]], exampleData[[2]], 0.45)


### plot the estimated graphs by diffee

data(exampleDataGraph)
par(mfrow=c(1,2))
label = NULL
layout = layout_nicely(exampleDataGraph[[1]], dim = 2)

readline(prompt="Press [enter] to view comparison between ground truth and generated difference graph")
{
  plot.diffee(result,graphlabel = label, graphlayout = layout)

  plot.igraph(union(exampleDataGraph[[2]], exampleDataGraph[[3]]),layout = layout,vertex.label.font=2,
              vertex.shape="none",
              vertex.label.color="gray40",
              vertex.label.cex=.7, vertex.frame.color="white", vertex.size = 10 ,
              edge.color = rainbow(1)[1],
              main = "difference graph ground truth")
}


readline(prompt="Press [enter] to continue to NIPS word count demo with 2 tasks (before 2006 and after 2006) and 37 features (37 words)")

par(mfrow=c(1,1))

### load nips word count data
data(nip_37_data)

label = colnames(nip_37_data[[1]])

result = diffee(nip_37_data[[1]], nip_37_data[[2]] , 0.0035)

readline(prompt="Press [enter] to display the difference graph")

plot.diffee(result, graphlabel = label)
}

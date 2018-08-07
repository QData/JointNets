### simule, jeek shared graph same as shared edge between individual graphs?
### jeek cannot run parallel
### Error in .backwardMap(covX, thre) : unused argument (thre) (diffee cannot run)
### enviroment?

{
  data(cancer)
  data("exampleData")
  data("nip_37_data")
  result = jeek(X = exampleData, 0.3, covType = "cov", parallel = FALSE)
  plot(result)
}


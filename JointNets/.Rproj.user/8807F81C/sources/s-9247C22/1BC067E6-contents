#'helper function to add row/col names to jointnet precision matrix output
#'To help label igraph object in returngraph and plot
#'@param output output of jointnets
#'@param datalist orginial data list
add_name_to_out<-function(output,datalist){
  cname = colnames(datalist[[1]])
  if (!is.null(cname)){
  for (i in 1:length(datalist)){
    rownames(output[[i]]) <- cname
    colnames(output[[i]]) <- cname
  }
  }
  return(output)
}





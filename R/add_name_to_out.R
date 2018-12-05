#'helper function to add row/col names to JointNets precision matrix output
#'To help label igraph object in returngraph and plot
#'@param output output of jointnets
#'@param datalist orginial data list
#'@param ... unused
#'@return output with names from datalist
add_name_to_out <- function(output, datalist, ...) {
  cname = colnames(datalist[[1]])
  if (!is.null(cname)) {
    for (i in 1:length(datalist)) {
      rownames(output$graphs[[i]]) <- cname
      colnames(output$graphs[[i]]) <- cname
    }
    if (!is.null(output$share)) {
      rownames(output$share) <- cname
      colnames(output$share) <- cname
    }
  }
  return(output)
}

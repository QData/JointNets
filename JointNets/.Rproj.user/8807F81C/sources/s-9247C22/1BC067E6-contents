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

#' helper function to make adjacency matrix
#' @param theta result from jointnets
make.adj.matrix <-
  function(theta, separate=FALSE)
  {
    K = length(theta)
    adj = list()
    if(separate)
    {
      for(k in 1:K)
      {
        adj[[k]] = (abs(theta[[k]])>1e-5)*1
      }
    }
    if(!separate)
    {
      adj = 0*theta[[1]]
      for(k in 1:K)
      {
        adj = adj+(abs(theta[[k]])>1e-5)*2^(k-1)
      }
    }
    return(adj)
  }


#' helper function to make title
maketitle <-
  function(x,
           type = "task",
           subID = NULL,
           index = NULL,
           neighbouroption = "task",
           graphlabel = NULL)
  {
    if (class(x) != "diffee"){
    if (type == "share") {
      return ("Shared Graph")
    }

    if (type == "taskspecific") {
      temp = paste(as.character(subID), collapse = ", ")
      return (paste("Task", temp, "Specific Graph"))
    }

    if (type == "task") {
      if (is.null(subID)) {
        return ("All Graphs")
      }
      else {
        if (length(subID) == 1) {
          if (subID == 0) {
            return ("Shared Graph")
          }
          else{
            return (paste("Task", subID, "Graph"))
          }
        }
        else {
          if (0 %in% subID) {
            temp = subID[-(which(subID %in% 0))]

            return(paste("Task", paste(as.character(temp), collapse = ", ")), "Graph")
          }
          else {
            return (paste("Task", paste(
              as.character(subID), collapse = ", "
            ), "Graph"))
          }
        }
      }
    }

    if (type == "neighbour") {
      second = ""
      first = ""

      if (neighbouroption == "task") {
        if (length(subID) == 1) {
          if (subID == 0) {
            second = "on shared graph"
          }
          else {
            second = paste("on task",
                           paste(as.character(subID), collapse = ", "),
                           "graph")
          }
        }
        else {
          if (!is.null(subID)) {
            if (0 %in% subID) {
              temp = subID[-(which(subID %in% 0))]

              second = paste("on task",
                             paste(as.character(temp), collapse = ", "),
                             "graph")
            }
            else {
              second = paste("on task",
                             paste(as.character(subID), collapse = ", "),
                             "graph")
            }
          }
          else {
            second = "on all graphs"
          }
        }
      }
      else{
        second = paste("on task",
                       paste(as.character(subID), collapse = ", "),
                       "specific graph")
      }

      if (is.null(graphlabel) || is.na(graphlabel)) {
        first = paste("Zoom in at node", paste(as.character(index), collapse = ", "))
      }

      else {
        first = paste("Zoom in at node", paste(as.character(graphlabel[index]), collapse = ", "))
      }

      return (paste(first, second))
    }
    }
    else {
      if (type == "task"){
        return ("difference graph")
      }
      if (type == "neighbour"){
        second = "on difference graph"
        if (is.null(graphlabel) || is.na(graphlabel)) {
          first = paste("Zoom in at node", paste(as.character(index), collapse = ", "))
        }

        else {
          first = paste("Zoom in at node", paste(as.character(graphlabel[index]), collapse = ", "))
        }
      }
      return (paste(first,second))
    }

  }

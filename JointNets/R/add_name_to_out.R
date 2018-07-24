#'helper function to add row/col names to jointnet precision matrix output
#'To help label igraph object in returngraph and plot
#'@param output output of jointnets
#'@param datalist orginial data list
add_name_to_out<-function(output,datalist,...){
  UseMethod("add_name_to_out",output)
}

add_name_to_out.diffee<-function(output,datalist,...){
  cname = colnames(datalist[[1]])
  if (!is.null(cname)){
      rownames(output$difference) <- cname
      colnames(output$difference) <- cname
  }
  return(output)
}

add_name_to_out.simule<-function(output,datalist,...){
  cname = colnames(datalist[[1]])
  if (!is.null(cname)){
    for (i in 1:length(datalist)){
      rownames(output$graphs[[i]]) <- cname
      colnames(output$graphs[[i]]) <- cname
    }
    rownames(output$share) <- cname
    colnames(output$share) <- cname
  }
  return(output)
}

add_name_to_out.wsimule<-function(output,datalist,...){
  cname = colnames(datalist[[1]])
  if (!is.null(cname)){
    for (i in 1:length(datalist)){
      rownames(output$graphs[[i]]) <- cname
      colnames(output$graphs[[i]]) <- cname
    }
    rownames(output$share) <- cname
    colnames(output$share) <- cname
  }
  return(output)
}

add_name_to_out.fasjem<-function(output,datalist,...){
  cname = colnames(datalist[[1]])
  if (!is.null(cname)){
    for (i in 1:length(datalist)){
      rownames(output$graphs[[i]]) <- cname
      colnames(output$graphs[[i]]) <- cname
    }
  }
  return(output)
}

add_name_to_out.jeek<-function(output,datalist,...){
  cname = colnames(datalist[[1]])
  if (!is.null(cname)){
    for (i in 1:length(datalist)){
      rownames(output$graphs[[i]]) <- cname
      colnames(output$graphs[[i]]) <- cname
    }
  }
  return(output)
}






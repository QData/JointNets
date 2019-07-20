
maketitle <- function(x,...) {
  UseMethod("maketitle",x)
}


maketitle.diffee <- function(x,
                             type = "task",
                             index = NULL,
                             graphlabel = NULL,
                             ...) {
  maketitle_difference(x,type,index,graphlabel)
}


maketitle.kdiffnet <- function(x,
                             type = "task",
                             index = NULL,
                             graphlabel = NULL,
                             ...) {
  maketitle_difference(x,type,index,graphlabel)
}

maketitle.simule <- function(x,
                             type = "task",
                             subID = NULL,
                             index = NULL,
                             neighbouroption = "task",
                             graphlabel = NULL,
                             ...) {
  maketitle_joint(x,type,subID,index,neighbouroption,graphlabel)
}

maketitle.wsimule <- function(x,
                             type = "task",
                             subID = NULL,
                             index = NULL,
                             neighbouroption = "task",
                             graphlabel = NULL,
                             ...) {
  maketitle_joint(x,type,subID,index,neighbouroption,graphlabel)
}

maketitle.jeek <- function(x,
                             type = "task",
                             subID = NULL,
                             index = NULL,
                             neighbouroption = "task",
                             graphlabel = NULL,
                             ...) {
  maketitle_joint(x,type,subID,index,neighbouroption,graphlabel)
}


maketitle.simulation <- function(x,
                             type = "task",
                             subID = NULL,
                             index = NULL,
                             neighbouroption = "task",
                             graphlabel = NULL,
                             ...) {
  maketitle_joint(x,type,subID,index,neighbouroption,graphlabel)
}

maketitle.jgl <- function(x,
                           type = "task",
                           subID = NULL,
                           index = NULL,
                           neighbouroption = "task",
                           graphlabel = NULL,
                           ...) {
  maketitle_joint(x,type,subID,index,neighbouroption,graphlabel)
}


maketitle_difference <-
  function(x,
           type = "task",
           index = NULL,
           graphlabel = NULL)
  {
    name = class(x)
    if (type == "task") {
      return (paste(name,"difference graph"))
    }
    if (type == "neighbour") {
      second = "on difference graph"
      if (is.null(graphlabel) || is.na(graphlabel)) {
        first = paste("Zoom in at node", paste(as.character(index), collapse = ", "))
      }

      else {
        first = paste("Zoom in at node", paste(as.character(graphlabel[index]), collapse = ", "))
      }
    }
    return (paste(name, first, second))
  }

maketitle_joint <-
  function(x,
           type = "task",
           subID = NULL,
           index = NULL,
           neighbouroption = "task",
           graphlabel = NULL)
  {
    name = class(x)
    if (type == "share") {
      return (paste(name,"Shared Graph"))
    }

    if (type == "taskspecific") {
      temp = paste(as.character(subID), collapse = ", ")
      return (paste(name,"Task", temp, "Specific Graph"))
    }

    if (type == "task") {
      if (is.null(subID)) {
        return (paste(name,"All Graphs"))
      }
      else {
        if (length(subID) == 1) {
          if (subID == 0) {
            return (paste(name,"Shared Graph"))
          }
          else{
            return (paste(name,"Task", subID, "Graph"))
          }
        }
        else {
          if (0 %in% subID) {
            temp = subID[-(which(subID %in% 0))]

            return(paste(name,"Task", paste(as.character(temp), collapse = ", ")), "Graph")
          }
          else {
            return (name,paste("Task", paste(
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

      return (paste(name,first, second))
    }
  }

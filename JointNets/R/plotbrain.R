#' plot 3d brain network from jointnet result
#'
#' This function plots 3d brain network from jointnet result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from any one of the jointnet functions (simule,wsimule,jeek,fasjem,diffee)
#' @param ... additional arguments
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' }
#' @export
plotbrain <- function(x, ...) {
  UseMethod("plotbrain", x)
}



#' plot 3d brain network from simule result
#'
#' This function plots 3d brain network from simule result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from simule
#' @param ... additional arguments
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' }
#' @method plotbrain simule
#' @export
#' @export plotbrain.simule
plotbrain.simule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           ...)
  {
    plotbrain_joint(x,
                    type,
                    neighbouroption,
                    subID,
                    index,
                    hastitle,
                    haslegend,
                    ...)

  }



#' plot 3d brain network from wsimule result
#'
#' This function plots 3d brain network from wsimule result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from wsimule
#' @param ... additional arguments
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' }
#' @method plotbrain wsimule
#' @export
#' @export plotbrain.wsimule
plotbrain.wsimule <- function(x, ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend,
                  ...)
}

#' plot 3d brain network from fasjem result
#'
#' This function plots 3d brain network from fasjem result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from fasjem
#' @param ... additional arguments
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' }
#' @method plotbrain fasjem
#' @export
#' @export plotbrain.fasjem
plotbrain.fasjem <- function(x, ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend,
                  ...)
}


#' plot 3d brain network from diffee result
#'
#' This function plots 3d brain network from diffee result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from diffee
#' @param ... additional arguments
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' }
#' @method plotbrain diffee
#' @export
#' @export plotbrain.diffee
plotbrain.diffee <- function(x, ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend,
                  ...)
}



#' plot 3d brain network from jeek result
#'
#' This function plots 3d brain network from jeek result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from jeek
#' @param ... additional arguments
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' }
#' @method plotbrain jeek
#' @export
#' @export plotbrain.jeek
plotbrain.jeek <- function(x, ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend,
                  ...)
}



plotbrain_joint <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           ...) {
    ### print to rstudio viewer panel
    ### options(rgl.printRglwidget = FALSE)

    subID = unique(subID)
    index = unique(index)
    gadj = returngraph(
      x,
      type = type,
      neighbouroption = neighbouroption,
      subID = subID,
      index = index
    )

    title = NA
    if (hastitle) {
      glabel = V(gadj)$label
      if (hasArg('vertex.label')) {
        glabel = args$vertex.label
      }
      ## make title according to user input
      title = maketitle(
        x,
        type = type,
        subID = subID,
        index = index,
        graphlabel = glabel,
        neighbouroption = neighbouroption
      )
    }

        rgl::rgl.open()
        rgl::par3d(windowRect = c(100, 100, 1000, 1000))


        rgl::title3d(main = title); next3d()

        rgl::legend3d(
          "topright" ,
          legend = c(paste("task", c(
            1:length(x$graphs)
          ), "specific"), "share"),
          col = grDevices::rainbow(length(x$graphs) + 1),
          pch = 16,
          cex = 1,
          inset = c(0.02)
        ); next3d()

        misc3d::contour3d(
          neurobase::readnii(
            system.file("MNI152_T1_1mm_brain.nii.gz", package = "brainR"),
            reorient = FALSE
          ),
          level = 3000,
          alpha = 0.5,
          draw = TRUE,
          add = TRUE,
          rescale = FALSE
        )

        igraph::rglplot(
          gadj,
          vertex.label.font = ifelse(hasArg('vertex.label.font'), args$vertex.label.font, 2),
          vertex.shape =  ifelse(hasArg('vertex.shape'), args$vertex.shape, "circle"),
          vertex.label.color = ifelse(
            hasArg('vertex.label.color'),
            args$vertex.label.color,
            "black"
          ),
          vertex.color = ifelse(hasArg('vertex.color'), args$vertex.color, "black"),
          edge.width = ifelse(hasArg('edge.width'), args$edge.width, 100),
          vertex.label.cex = ifelse(hasArg('vertex.label.cex'), args$vertex.label.cex, .7),
          vertex.size = ifelse(hasArg('vertex.size'), args$vertex.size, 300),
          vertex.frame.color = ifelse(
            hasArg('vertex.frame.color'),
            args$vertex.frame.color,
            "white"
          ),

          add = TRUE,
          draw = TRUE,
          rescale = FALSE,
          ...
        )

        #vertex.label
        #vertex.color
        #vertex.label = label,

  }


if(FALSE){
plotbrain_joint <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           ...) {
    subID = unique(subID)
    index = unique(index)
    gadj = returngraph(
      x,
      type = type,
      neighbouroption = neighbouroption,
      subID = subID,
      index = index
    )

    title = NA
    if (hastitle) {
      glabel = V(gadj)$label
      if (hasArg('vertex.label')) {
        glabel = args$vertex.label
      }
      ## make title according to user input
      title = maketitle(
        x,
        type = type,
        subID = subID,
        index = index,
        graphlabel = glabel,
        neighbouroption = neighbouroption
      )
    }

    rgl::rgl.open()
    rgl::par3d(windowRect = c(100, 100, 1000, 1000))

    misc3d::contour3d(
      neurobase::readnii(
        system.file("MNI152_T1_1mm_brain.nii.gz", package = "brainR"),
        reorient = FALSE
      ),
      level = 3000,
      alpha = 0.5,
      draw = TRUE,
      add = FALSE,
      rescale = FALSE
    )

    #vertex.label
    #vertex.color
    #vertex.label = label,
    igraph::rglplot(
      gadj,
      vertex.label.font = ifelse(hasArg('vertex.label.font'), args$vertex.label.font, 2),
      vertex.shape =  ifelse(hasArg('vertex.shape'), args$vertex.shape, "circle"),
      vertex.label.color = ifelse(
        hasArg('vertex.label.color'),
        args$vertex.label.color,
        "black"
      ),
      vertex.color = ifelse(hasArg('vertex.color'), args$vertex.color, "black"),
      edge.width = ifelse(hasArg('edge.width'), args$edge.width, 100),
      vertex.label.cex = ifelse(hasArg('vertex.label.cex'), args$vertex.label.cex, .7),
      vertex.size = ifelse(hasArg('vertex.size'), args$vertex.size, 300),
      vertex.frame.color = ifelse(
        hasArg('vertex.frame.color'),
        args$vertex.frame.color,
        "white"
      ),

      add = TRUE,
      draw = TRUE,
      rescale = FALSE,
      ...
    )

    legend(
      "topright" ,
      legend = c(paste("task", c(
        1:length(x$graphs)
      ), "specific"), "share"),
      col = grDevices::rainbow(length(x$graphs) + 1),
      pch = 16,
      cex = 1,
      inset = c(0.02)
    )

    if (FALSE){
    rgl::bgplot3d({
      plot.new()
      if (hastitle) {
        title(main = title, line = 3)
      }
      if (haslegend) {
        legend(
          "topright" ,
          legend = c(paste("task", c(
            1:length(x$graphs)
          ), "specific"), "share"),
          col = grDevices::rainbow(length(x$graphs) + 1),
          pch = 16,
          cex = 1,
          inset = c(0.02)
        )
      }
    })
    }

  }
}

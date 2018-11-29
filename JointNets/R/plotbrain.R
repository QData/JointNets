#' plot 3d brain network from JointNets result
#'
#' This function plots 3d brain network from JointNets result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from any one of the JointNets functions (simule,wsimule,jeek,fasjem,diffee,diffeek)
#' @param ... additional arguments, please see \code{\link{plotbrain.simule}}, \code{\link{plotbrain.wsimule}} and etc for details
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simule(ABIDE_aal116_timeseries, 0.2, 1, covType = "cov", TRUE)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
#' }
#' @import rgl
#' @import brainR
#' @import misc3d
#' @import oro.nifti
#' @export
plotbrain <- function(x, ...) {
  UseMethod("plotbrain", x)
}


#' plot 3d brain network from simule result
#'
#' This function plots 3d brain network from simule result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from simule
#' @param ... additional arguments, please see \code{\link{plot.simule}} for details
#' @param layout numeric matrix (3 columns) that specifies node positions in 3d brain network (atlas coordinates)
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simule(ABIDE_aal116_timeseries, 0.2, 1, covType = "cov", TRUE)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
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
#' @param ... additional arguments, please see \code{\link{plot.wsimule}} for details
#' @param layout numeric matrix (3 columns) that specifies node positions in 3d brain network (atlas coordinates)
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = wsimule(ABIDE_aal116_timeseries, 0.2, 1, W = matrix(1,116,116), covType = "cov", TRUE)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
#' }
#' @method plotbrain wsimule
#' @export
#' @export plotbrain.wsimule
plotbrain.wsimule <-
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

#' plot 3d brain network from fasjem result
#'
#' This function plots 3d brain network from fasjem result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from fasjem
#' @param ... additional arguments, please see \code{\link{plot.fasjem}} for details
#' @param layout numeric matrix (3 columns) that specifies node positions in 3d brain network (atlas coordinates)
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = fasjem(X = ABIDE_aal116_timeseries, method = "fasjem-g", 0.001, 0.1, 0.1, 0.05, 20)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
#' }
#' @method plotbrain fasjem
#' @export
#' @export plotbrain.fasjem
plotbrain.fasjem <-
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

#' plot 3d brain network from diffee result
#'
#' This function plots 3d brain network from diffee result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from diffee
#' @param ... additional arguments, please see \code{\link{plot.diffee}} for details
#' @param layout numeric matrix (3 columns) that specifies node positions in 3d brain network (atlas coordinates)
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = diffee(ABIDE_aal116_timeseries[[1]], ABIDE_aal116_timeseries[[2]], 0.001)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
#' }
#' @method plotbrain diffee
#' @export
#' @export plotbrain.diffee
plotbrain.diffee <- function(x,
                             type = "task",
                             neighbouroption = "task",
                             subID = NULL,
                             index = NULL,
                             hastitle = TRUE,
                             haslegend = TRUE,
                             ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend = FALSE,
                  ...)
}


#' plot 3d brain network from diffeek result
#'
#' This function plots 3d brain network from diffeek result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from diffee
#' @param ... additional arguments, please see \code{\link{plot.diffeek}} for details
#' @param layout numeric matrix (3 columns) that specifies node positions in 3d brain network (atlas coordinates)
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = diffeek(ABIDE_aal116_timeseries[[1]], ABIDE_aal116_timeseries[[2]], W = matrix(1,116,116), g = 0,epsilon = 0.1, lambda = 0.001)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
#' }
#' @method plotbrain diffeek
#' @export
#' @export plotbrain.diffeek
plotbrain.diffeek <- function(x,
                              type = "task",
                              neighbouroption = "task",
                              subID = NULL,
                              index = NULL,
                              hastitle = TRUE,
                              haslegend = TRUE,
                              ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend = FALSE,
                  ...)
}


#' plot 3d brain network from jeek result
#'
#' This function plots 3d brain network from jeek result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from jeek
#' @param ... additional arguments, please see \code{\link{plot.jeek}} for details
#' @param layout numeric matrix (3 columns) that specifies node positions in 3d brain network (atlas coordinates)
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' \dontrun{
#' library(JointNets)
#' graphics.off()
#' par(ask=F)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = jeek(X = ABIDE_aal116_timeseries,0.25,covType = "kendall",parallel = TRUE)
#' plotbrain(result, type = "task", neighbouroption = "task", subID = NULL, index = NULL, layout = layout)
#' }
#' @method plotbrain jeek
#' @export
#' @export plotbrain.jeek
plotbrain.jeek <- function(x,
                           type = "task",
                           neighbouroption = "task",
                           subID = NULL,
                           index = NULL,
                           hastitle = TRUE,
                           haslegend = TRUE,
                           ...) {
  plotbrain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend,
                  ...)
}

### main function for plotbrain
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

    ### make title for the plot
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

    ### intialize plot
    rgl::mfrow3d(nr = 1, nc = 1, sharedMouse = TRUE)
    rgl::par3d(windowRect = c(100, 100, 1000, 1000))
    ### display background brain (refer to multipleRegion_plot from brainKCCA package)
    misc3d::contour3d(
      oro.nifti::readNIfTI(
        system.file("MNI152_T1_1mm_brain.nii.gz", package = "brainR"),
        reorient = FALSE
      ),
      level = 3000,
      alpha = 0.5,
      draw = TRUE,
      add = TRUE,
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

    ### plot title
    if (hastitle) {
      title3d(main = title, sub = NULL, xlab = NULL, ylab = NULL,
              zlab = NULL, line = NA, ...)
    }
    ### plot legend
    if (haslegend) {
      legend3d(
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

  }


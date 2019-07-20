#' plot 3d brain network from JointNets result
#'
#' This function plots 3d brain network from JointNets result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from any one of the JointNets functions
#' @param ... additional arguments, please see \code{\link{plot_brain.simule}}, \code{\link{plot_brain.wsimule}} and etc for details
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "simule"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = simule(ABIDE_aal116_timeseries, 0.2, 1, covType = "cov", FALSE)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
#' @import rgl
#' @import brainR
#' @import misc3d
#' @import oro.nifti
#' @export
plot_brain <- function(x, ...) {
  UseMethod("plot_brain", x)
}


#' plot 3d brain network from simule result
#'
#' This function plots 3d brain network from simule result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from simule function (simule class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend determines whether the graph legend is displayed or not (TRUE to display / FALSE to hide)
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot()
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "simule"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = simule(ABIDE_aal116_timeseries, 0.2, 1, covType = "cov", FALSE)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
#' @method plot_brain simule
#' @export
#' @export plot_brain.simule
plot_brain.simule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           hasbackground = TRUE,
           ...)
  {
    plot_brain_joint(x,
                    type,
                    neighbouroption,
                    subID,
                    index,
                    hastitle,
                    haslegend,
                    hasbackground,
                    ...)

  }



#' plot 3d brain network from wsimule result
#'
#' This function plots 3d brain network from wsimule result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from wsimule function (wsimule class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend determines whether the graph legend is displayed or not (TRUE to display / FALSE to hide)
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot()
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "simule"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = wsimule(ABIDE_aal116_timeseries, 0.2, 1,
#' W = matrix(1,116,116), covType = "cov", FALSE)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
#' @method plot_brain wsimule
#' @export
#' @export plot_brain.wsimule
plot_brain.wsimule <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           hasbackground = TRUE,
           ...)
  {
    plot_brain_joint(x,
                    type,
                    neighbouroption,
                    subID,
                    index,
                    hastitle,
                    haslegend,
                    hasbackground,
                    ...)

  }



#' plot 3d brain network from diffee result
#'
#' This function plots 3d brain network from diffee result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from diffee function (diffee class)
#' @param type type of graph. There are two options:
#' * "task" (difference graph)
#' * "neighbour" (zoom into nodes in the difference graph specified further by parameter
#' **"index"** (node id)
#' @param neighbouroption not used
#' @param subID not used
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend not used
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot()
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "simule"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = diffee(ABIDE_aal116_timeseries[[1]],
#' ABIDE_aal116_timeseries[[2]], 0.001)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
#' @method plot_brain diffee
#' @export
#' @export plot_brain.diffee
plot_brain.diffee <- function(x,
                             type = "task",
                             neighbouroption = "task",
                             subID = NULL,
                             index = NULL,
                             hastitle = TRUE,
                             haslegend = TRUE,
                             hasbackground = TRUE,
                             ...) {
  plot_brain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend = FALSE,
                  hasbackground,
                  ...)
}


#' plot 3d brain network from kdiffnet result
#'
#' This function plots 3d brain network from kdiffnet result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from kdiffnet function (kdiffnet class)
#' @param type type of graph. There are two options:
#' * "task" (difference graph)
#' * "neighbour" (zoom into nodes in the difference graph specified further by parameter
#' **"index"** (node id)
#' @param neighbouroption not used
#' @param subID not used
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend not used
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot()
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "simule"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = kdiffnet(ABIDE_aal116_timeseries[[1]], ABIDE_aal116_timeseries[[2]],
#' W = matrix(1,116,116), g = rep(0,116), epsilon = 0.1, lambda = 0.001)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
#' @method plot_brain kdiffnet
#' @export
#' @export plot_brain.kdiffnet
plot_brain.kdiffnet <- function(x,
                              type = "task",
                              neighbouroption = "task",
                              subID = NULL,
                              index = NULL,
                              hastitle = TRUE,
                              haslegend = TRUE,
                              hasbackground = TRUE,
                              ...) {
  plot_brain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend = FALSE,
                  hasbackground,
                  ...)
}


#' plot 3d brain network from jeek result
#'
#' This function plots 3d brain network from jeek result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from jeek function (jeek class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend determines whether the graph legend is displayed or not (TRUE to display / FALSE to hide)
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot()
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "simule"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = jeek(X = ABIDE_aal116_timeseries,0.25,
#' covType = "kendall",parallel = FALSE)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
#' @method plot_brain jeek
#' @export
#' @export plot_brain.jeek
plot_brain.jeek <- function(x,
                           type = "task",
                           neighbouroption = "task",
                           subID = NULL,
                           index = NULL,
                           hastitle = TRUE,
                           haslegend = TRUE,
                           hasbackground = TRUE,
                           ...) {
  plot_brain_joint(x,
                  type,
                  neighbouroption,
                  subID,
                  index,
                  hastitle,
                  haslegend,
                  hasbackground,
                  ...)
}


#' plot 3d brain network from jgl result
#'
#' This function plots 3d brain network from jgl result
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param x output generated from jgl function (jgl class)
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend determines whether the graph legend is displayed or not (TRUE to display / FALSE to hide)
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot()
#' @return 3d (rgl) brain network
#' @details The function plots brain network using \code{\link{rglplot.igraph}}
#' @method plot_brain jgl
#' @export
#' @export plot_brain.jgl
#' @examples
#' library(JointNets)
#' graphics.off()
#' par(ask=FALSE)
#' par(mfrow=c(1,1))
#' data(ABIDE_aal116_timeseries)
#' data(aal116coordinates)
#' layout = cbind(aal116coordinates$x.mni + 90,
#' aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)
#' result = simulation(p=116, s = 0.001, ss = 0.001, n = c(1,1))$simulatedgraphs
#' class(result) = "jgl"
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout, hasbackground = FALSE)
#' \donttest{
#' result = jgl(ABIDE_aal116_timeseries, 0.2, 1)
#' plot_brain(result, type = "task", neighbouroption = "task",
#' subID = NULL, index = NULL, layout = layout)
#' }
plot_brain.jgl <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           hasbackground = TRUE,
           ...)
  {
    plot_brain_joint(x,
                    type,
                    neighbouroption,
                    subID,
                    index,
                    hastitle,
                    haslegend,
                    hasbackground,
                    ...)

  }

#' plot 3d brain network
#' @param x output generated from JointNets Methods
#' @param type type of graph. There are four options:
#' * "task" (graph for each task (including shared part) specified further by subID (task number))
#' * "share" (shared graph for all tasks)
#' * "taskspecific" (graph for each task specific graph (excluding shared part)
#' specified further by subID (task number) )
#' * "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter **"type"** is **"neighbour"**. There are two options:
#' * "task" (zoom into graph for each task (including shared part))
#' * "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display. There are four options:
#' * 0 (only allowed when
#' **"type"** is **"task"** or **"type"** is **"neighbour"** and **"neighbouroption"** is **"task"**) (selects share graph)
#' * positive task number (selects that particular task)
#' * a vector of task number (selects multiple tasks)
#' * NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter **"type"** is **"neighbour"**.
#' This parameter could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param hastitle determines whether the graph title is displayed or not (TRUE to display / FALSE to hide)
#' @param haslegend determines whether the graph legend is displayed or not (TRUE to display / FALSE to hide)
#' @param hasbackground determines whether the reference brain is plotted or not (TRUE to display / FALSE to hide)
#' @param ... extra parameters passed to igraph::rglplot() and level in misc::contour3d()
#' @return 3d (rgl) brain network
#' @import methods
plot_brain_joint <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           hastitle = TRUE,
           haslegend = TRUE,
           hasbackground = TRUE,
           ...) {
    args = list(...)
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
      if (methods::hasArg('vertex.label')) {
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
    #rgl::mfrow3d(nr = 1, nc = 1, sharedMouse = TRUE)
    #rgl::open3d()
    #rgl::par3d(windowRect = c(100, 100, 1000, 1000))
    rgl::par3d(skipRedraw=TRUE)
    #rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 612, 612))
    ### display background brain (refer to multipleRegion_plot from brainKCCA package)
    if (hasbackground){
    misc3d::contour3d(
      oro.nifti::readNIfTI(
        system.file("MNI152_T1_1mm_brain.nii.gz", package = "brainR"),
        reorient = FALSE
      ),
      level = ifelse(methods::hasArg('level'), args$level, 3000),
      alpha = ifelse(methods::hasArg('alpha'), args$alpha, 0.5),
      draw = TRUE,
      add = FALSE,
      rescale = FALSE
    )
    }
    #vertex.label
    #vertex.color
    #vertex.label = label,
    igraph::rglplot(
      gadj,
      vertex.label.font = ifelse(methods::hasArg('vertex.label.font'), args$vertex.label.font, 2),
      vertex.shape =  ifelse(methods::hasArg('vertex.shape'), args$vertex.shape, "circle"),
      vertex.label.color = ifelse(
        methods::hasArg('vertex.label.color'),
        args$vertex.label.color,
        "black"
      ),
      vertex.color = ifelse(methods::hasArg('vertex.color'), args$vertex.color, "black"),
      edge.width = ifelse(methods::hasArg('edge.width'), args$edge.width, 100),
      vertex.label.cex = ifelse(methods::hasArg('vertex.label.cex'), args$vertex.label.cex, .7),
      vertex.size = ifelse(methods::hasArg('vertex.size'), args$vertex.size, 300),
      vertex.frame.color = ifelse(
        methods::hasArg('vertex.frame.color'),
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
    ###text3d(x=1.1, y=c(.9,1,1.1), z=1.1, c(paste("task", c(
    ###   1:length(x$graphs)
    ### ), "specific"), "share") ,
    ###col=grDevices::rainbow(length(x$graphs) + 1))
    #points3d(x=1.2,y=c(.9,1,1.1),z=1.1, col=as.numeric(as.factor(loadings[,4])), size=5)


    if (haslegend) {
      bgplot3d({
      par(mar=c(0,0,0,0))
      par(ask = FALSE)
      plot(0,0, type="n", xlim=0:1, ylim=0:1, xaxs="i", yaxs="i", axes=FALSE, bty="n")
      graphics::legend(
        "topright" ,
        legend = c(paste("task", c(
          1:length(x$graphs)
        ), "specific"), "share"),
        col = grDevices::rainbow(length(x$graphs) + 1),
        pch = 16,
        cex = 1,
        inset = c(0.02)
      )
      })
    }

    rgl::par3d(skipRedraw=FALSE)

  }


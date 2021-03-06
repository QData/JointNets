% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/returngraph.R
\name{returngraph.simulation}
\alias{returngraph.simulation}
\title{return igraph object from simulation result specified by user input}
\usage{
\method{returngraph}{simulation}(x, type = "task",
  neighbouroption = "task", subID = NULL, index = NULL, ...)
}
\arguments{
\item{x}{output generated from simulatino functino}

\item{type}{type of graph. There are four options:
\itemize{
\item "task" (graph for each task (including shared part) specified further by subID (task number))
\item "share" (shared graph for all tasks)
\item "taskspecific" (graph for each task specific graph (excluding shared part)
specified further by subID (task number) )
\item "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
and index (node id))
}}

\item{neighbouroption}{determines what type of graph to zoom into when parameter \strong{"type"} is \strong{"neighbour"}. There are two options:
\itemize{
\item "task" (zoom into graph for each task (including shared part))
\item "taskspecific" (zoom into graph for each task specific (excluding shared part))
}}

\item{subID}{selects which task to display. There are four options:
\itemize{
\item 0 (only allowed when
\strong{"type"} is \strong{"task"} or \strong{"type"} is \strong{"neighbour"} and \strong{"neighbouroption"} is \strong{"task"}) (selects share graph)
\item positive task number (selects that particular task)
\item a vector of task number (selects multiple tasks)
\item NULL (selects all tasks (all graphs))
}}

\item{index}{determines which node(s) to zoom into when parameter \strong{"type"} is \strong{"neighbour"}.
This parameter could either be an integer or vector of integers representing node ids
(zoom into one node or multiple nodes)}

\item{...}{not used}
}
\value{
an igraph object of graph / subgraph from simulation result specified by user input
}
\description{
This function can return an igraph object from simulation result for user to work with directly
}
\details{
the function aims to provide users the flexibility to explore and visualize the graph on their own
generated from simulation
}
\examples{
library(JointNets)
data(exampleData)
result = simulation(n=c(100,100,100))$simulatedgraphs
graph = returngraph(result)
}
\author{
Beilun Wang, Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
}

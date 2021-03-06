% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jgl.R
\name{jgl}
\alias{jgl}
\title{wrapper for function JGL fromo package "JGL"}
\usage{
jgl(X, lambda1, lambda2, ...)
}
\arguments{
\item{X}{data list}

\item{lambda1}{The tuning parameter for the graphical lasso penalty.}

\item{lambda2}{The tuning parameter for the fused or group lasso penalty.}

\item{...}{optional parameters passed to JGL() from "JGL" package}
}
\value{
a list of estimated precision matrix
}
\description{
wrapper for function JGL fromo package "JGL"
}
\examples{
library(JointNets)
data(exampleData)
result = jgl(exampleData,0.1,0.01)
plot(result)
}

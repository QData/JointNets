% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jeek.R
\name{jeek}
\alias{jeek}
\title{A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in
Learning Multiple Related Sparse Gaussian Graphical Models}
\usage{
jeek(X, lambda, W = NA, covType = "cov", intertwined = FALSE,
  parallel = FALSE)
}
\arguments{
\item{X}{A List of input matrices. They can be data matrices or
covariance/correlation matrices. If every matrix in the X is a symmetric
matrix, the matrices are assumed to be covariance/correlation matrices.}

\item{lambda}{A positive number. The hyperparameter controls the sparsity
level of the matrices. The \eqn{\lambda_n} in the following section:
Details.}

\item{W}{A list of weight matrices. The hyperparameter intergrating the
additional knowledge into the model. The \eqn{W_{ij}} is large means that
node i and node j have less probability to connect with each other. The
default value of each entry is 1, which means there is no additional
knowledge in the formulation.}

\item{covType}{A parameter to decide which Graphical model we choose to
estimate from the input data.

If covType = "cov", it means that we estimate multiple sparse Gaussian
Graphical models. This option assumes that we calculate (when input X
represents data directly) or use (when X elements are symmetric representing
covariance matrices) the sample covariance matrices as input to the JEEK
algorithm.

If covType = "kendall", it means that we estimate multiple nonparanormal
Graphical models. This option assumes that we calculate (when input X
represents data directly) or use (when X elements are symmetric representing
correlation matrices) the kendall's tau correlation matrices as input to the
JEEK algorithm.}

\item{intertwined}{indicate whether to use intertwined covariance matrix}

\item{parallel}{A boolean. This parameter decides if the package will use
the multithreading architecture or not.}
}
\value{
\item{$graphs}{A list of the estimated inverse covariance/correlation
matrices.}
}
\description{
A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in
Learning Multiple Related Sparse Gaussian Graphical Models. Please run
demo(jeek) to learn the basic functions provided by this package.  For
further details, please read the original paper: Beilun Wang, Arshdeep
Sekhon, Yanjun Qi (2018).
}
\details{
The JEEK algorithm is a novel Joint Elementary Estimator incorporating
additional Knowledge (JEEK) to infer multiple related sparse Gaussian
Graphical models from large-scale heterogeneous data. It solves the
following equation: \deqn{ \min\limits_{\Omega^{tot}_I, \Omega^{tot}_S}
||W^{tot}_I \circ \Omega^{tot}_I||_1 + ||W^{tot}_S\circ \Omega^{tot}_S|| }
Subject to : \deqn{ ||W^{tot}_I \circ (\Omega^{tot} -
inv(T_v(\hat{\Sigma}^{tot}))) ||_{\infty} \le \lambda_n } \deqn{ ||W^{tot}_S
\circ (\Omega^{tot} - inv(T_v(\hat{\Sigma}^{tot}))) ||_{\infty} \le
\lambda_n } \deqn{ \Omega^{tot} = \Omega^{tot}_S + \Omega^{tot}_I }

Please also see the equation (3.7) in our paper. The \eqn{\lambda_n} is the
hyperparameter controlling the sparsity level of the matrices and it is the
\code{lambda} in our function. For further details, please see our paper:
Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable Joint Estimator
for Integrating Additional Knowledge in Learning Multiple Related Sparse
Gaussian Graphical Models. ICML 2018

if labels are provided in the datalist as column names, result will contain labels (to be plotted)
}
\examples{
library(JointNets)
data(exampleData)
result = jeek(X = exampleData, 0.3, covType = "cov", parallel = FALSE)
plot(result)
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable
Joint Estimator for Integrating Additional Knowledge in Learning Multiple
Related Sparse Gaussian Graphical Models. \url{https://arxiv.org/abs/1806.00548}
}
\author{
Beilun Wang
}

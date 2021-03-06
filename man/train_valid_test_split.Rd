% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_valid_test_split.R
\name{train_valid_test_split}
\alias{train_valid_test_split}
\title{split a datalist to train,validation and test}
\usage{
train_valid_test_split(datalist, ratio, seed)
}
\arguments{
\item{datalist}{a datalist}

\item{ratio}{ratio of the split (train, validation and test), eg, c(0.8,0.1,0.1)}

\item{seed}{seed number}
}
\value{
a list of train,validation and test datalist
}
\description{
split a datalist to train,validation and test
}
\examples{
library(JointNets)
data("nip_37_data")
}

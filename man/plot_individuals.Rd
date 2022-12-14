% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_individuals.R
\name{plot_individuals}
\alias{plot_individuals}
\title{Maps of individuals}
\usage{
plot_individuals(objet_pls, i = 1, j = 1)
}
\arguments{
\item{objet_pls}{: object of class S3 inheriting from "PLS".}

\item{i}{: integer, default=1. It is the index of the outcome to be selected.}

\item{j}{: integer, default=1. It is the index of the components to be selected.}
}
\value{
plot
}
\description{
Plots the individuals in relation with one component and u1. The details of u1 are given under ‘Details’.
}
\details{
Regression of y on the first component th1.
y=c1*th1+y1

u1 is the normalization of y : u1=y/c1

It allows us to obtain the first bisector as the least squares line of u1 on th1.
}
\examples{
d<-train_test_splits(iris,0.7)
train<-d$data_train
objet<-fit_pls(Species~.,train,n_components=4)
plot_individuals(objet,i=2,j=1)

}

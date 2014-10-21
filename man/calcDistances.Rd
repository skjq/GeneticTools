\name{calcDistances}
\alias{calcDistances}

\title{
  Calculate Distances Between Time Series
}

\description{
  This is an experimental function to calculate distances between time series.
}
\usage{
  calcDistances(X, method="euclidean", nodes=20)
}

\arguments{
  \item{X}{Matrix with time series values.}
  \item{method}{Method  of choice to calculate the distance.}
  \item{nodes}{Number of nodes for the spline-based approach.}
}

\details{
 This is an early attempt to implement a distance measure for time series clustering. It has not yet been used in
 productive code yet and hence should be used carefully.

 Currently there are two different methods available, the default is \code{method="euclidean"}, calculating a basic euclidean distance.
 The alternative is \code{"spline"}, what is a spline based approach.

 The method \code{"spline"}, however, is currently still under development and hence it is not available yet. For the latest 'nightbuild'
 of the package with the latest version, please visit the GIT project page (https://github.com/fischuu/GeneticTools.git).
}
\value{
 A matrix with pairwise distances.
}

\author{
  Daniel Fischer
}

\keyword{
  methods
}
 

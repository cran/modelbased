% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zero_crossings.R
\name{zero_crossings}
\alias{zero_crossings}
\title{Find zero crossings of a vector}
\usage{
zero_crossings(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
Vector of zero crossings.
}
\description{
Find zero crossings of a vector, i.e., indices when the numeric variable
crosses 0.
}
\examples{
x <- sin(seq(0, 4 * pi, length.out = 100))
plot(x)
zero_crossings(x)
}
\seealso{
Based on the \code{uniroot.all} function from the rootSolve package.
}

\name{print.regindep}
\alias{print.regindep}
\alias{summary.regindep}
\title{Print summary of regindept objects}
\description{
  \code{\link{print.regindep}} is the method used to print a \code{\link{regindep}} object created 
  by the \code{\link{regindep}} function.
}
\usage{
\method{print}{regindep}(x, ...)
\method{summary}{regindep}(object, ...)
}
\arguments{
  \item{x}{A \code{\link{regindep}} object. See documentation for \code{\link{regindep}}.}
  \item{object}{A \code{\link{regindep}} object. See documentation for \code{\link{regindep}}.}
  \item{...}{Additional arguments, to be passed to lower-level functions in the future.}

}
\details{Automatically called when an object of class \code{\link{regindep}} is printed.
  Currently, \code{\link{print.regindep}} summarizes relative resources, impacts, and constraints of an actor.
}
\seealso{regindep}

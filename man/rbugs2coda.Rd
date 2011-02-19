\name{rbugs2coda}
\alias{rbugs2coda}
\title{ Rbugs to coda }
\description{Convert a \code{rbugs} object to a \code{coda} format}
\usage{
rbugs2coda(model, burnin = NULL, thin = NULL)
}
\arguments{
  \item{model}{an object of class 'rbugs', see \code{\link{rbugs}} for details}
  \item{burnin}{a burn-in period for the chains}
  \item{thin}{the thining interval between consecutives observations}
}
\seealso{\code{\link{rbugs}}}
\keyword{coda}

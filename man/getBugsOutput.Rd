\name{getBugsOutput}
\alias{getBugsOutput}
\title{ Collect the MCMC samples from BUGS}
\description{
  Collect the MCMC samples from BUGS.
}
\usage{
getBugsOutput(n.chains, workingDir, linbugs = TRUE)
}
\arguments{
  \item{n.chains}{ The number of chains BUGS had run.}
  \item{workingDir}{ A character string specifying the name of the
    directory where the output files are saved.}
  \item{linbugs}{ If TRUE, linbugs is used. }
}
\details{
  This function assumes that under \code{\workingDir}, there is a coda
  index file \file{codaIndex.txt}, and the coda output for
  \code{n.chains} chains are named as coda1.txt, coda2.txt, ...
}
\value{
  A list of matrix whose columns and rows contain the monitored
  parameters and the MCMC iterations, respectively.
}
\author{ Jun Yan \email{jyan@stat.uiowa.edu}}
\keyword{utilities}
\keyword{interface}

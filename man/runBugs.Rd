\name{runBugs}
\alias{runBugs}
\title{ Execute a BUGS Srcipt from R }
\description{
  Create the system command to run BUGS and execute it.
}
\usage{
runBugs(bugs=Sys.getenv("BUGS"), script, n.chains, workingDir,
        useWine=FALSE, wine=Sys.getenv("WINE"), verbose = TRUE)
}
\arguments{
  \item{bugs}{ A character string specifying the full name (including
    path) of the BUGS executable. It must end with .txt to be
    recognizable by BUGS.}
  \item{script}{ A character string specifying the full name (including
    path) of the script to be run. It must end with .txt to be
    recognizable by BUGS.}
  \item{n.chains}{The number of MCMC chains.}
  \item{workingDir}{The working directory where the output files from
    BUGS are stored.}
  \item{useWine}{ A logical value indicating if wine is to be used.}
  \item{wine}{ A character string specifying the full name (including
    path) of the wine executable.}
  \item{verbose}{If TRUE, print the log file from BUGS.}
}
\value{
  None.
}
\author{ Jun Yan \email{jyan@stat.uiowa.edu}}
\seealso{  \code{\link{rbugs}}}
\keyword{interface}

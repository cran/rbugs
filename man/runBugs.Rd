\name{runBugs}
\alias{runBugs}
\title{ Execute a BUGS Srcipt from R }
\description{
  Create the system command to run BUGS and execute it.
}
\usage{
runBugs(bugs=system("which OpenBUGS", TRUE), script, n.chains, workingDir,
        OpenBugs = TRUE, Windows=TRUE, verbose = TRUE)
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
  \item{OpenBugs}{ If TRUE, OpenBugs is used. }
  \item{Windows}{ If TRUE, Windows plataform is used. }
  \item{verbose}{If TRUE, print the log file from BUGS.}
}
\value{
  None.
}
\author{Jun Yan \email{jyan@stat.uconn.edu} and Marcos Prates \email{marcosop@est.ufmg.br}}
\seealso{  \code{\link{rbugs}}}
\keyword{interface}

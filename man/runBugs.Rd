\name{runBugs}
\alias{runBugs}
\title{ Execute a BUGS Srcipt from R }
\description{
  Create the system command to run BUGS and execute it.
}
\usage{
runBugs(bugs=Sys.getenv("BUGS"), script, useWine=FALSE,
        wine=Sys.getenv("WINE"))
}
\arguments{
  \item{bugs}{ A character string specifying the full name (including
    path) of the BUGS executable. It must end with .txt to be
    recognizable by BUGS.}
  \item{script}{ A character string specifying the full name (including
    path) of the script to be run. It must end with .txt to be
    recognizable by BUGS.}
  \item{useWine}{ A logical value indicating if wine is to be used.}
  \item{wine}{ A character string specifying the full name (including
    path) of the wine executable.}
}
\value{
  None.
}
\author{ Jun Yan \email{jyan@stat.uiowa.edu}}
\seealso{  \code{\link{rbugs}}}
\keyword{interface}

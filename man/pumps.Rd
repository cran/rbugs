\name{pumps}
\alias{pumps}
\docType{data}
\title{ 10 Power Plant Pumps }
\description{
  10 Power Plant Pumps.
}
\usage{data(pumps)}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{t}{The length of the operation time.}
    \item{x}{The number of failures.}
  }
}
\examples{
data(pumps)
pumps.data <- list(t = pumps$t, x = pumps$x, N = nrow(pumps))
pumps.model <- file.path(.path.package("rbugs"), "bugs/model", "pumps.bug")
file.show(pumps.model)
pumps.inits <- file.path(.path.package("rbugs"), "bugs/inits", "pumps.txt")
file.show(pumps.inits)
inits <- list(dget(pumps.inits))
parameters <- c("theta", "alpha", "beta")

\dontrun{
#ifdef windows
## Define the full name of bugs in .Renviron.
## In .Renviron, add a line similar to this:
##  BUGS="c:/program files/winbugs14/winbugs14.exe"

pumps.sim <- rbugs(data = pumps.data, inits, parameters,
                   pumps.model, n.chains = 1, n.iter = 1000,
                   bugs="e:/program files/winbugs14/winbugs14.exe")
#endif
#ifdef unix
## The workingDir must be a directory which can be seen by wine
## The nominalWorkingDir gives the directory name recoganizable for
## Windows.
## The full name of the bugs and wine executables need to set.
## In .Renviron, add lines similar to these:
##   BUGS="c:/program files/winbugs14/winbugs14.exe"
##   WINE="/var/scratch/jyan/wine-20040408/wine" 

pumps.sim <- rbugs(data = pumps.data, inits, parameters, 
                   pumps.model, n.chains = 1, n.iter = 1000,
                   workingDir="/var/scratch/jyan/c/tmp",
                   bugsWorkingDir="c:/tmp",
                   useWine=TRUE)
#endif
}
}
\keyword{datasets}

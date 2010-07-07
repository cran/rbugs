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
## Windows using WinBugs
pumps.sim <- rbugs(data = pumps.data, inits, parameters,
                   pumps.model, n.chains = 1, n.iter = 1000,
                   bugs = "C:/Program Files/WinBUGS14/WinBUGS14.exe",
                   bugsWorkingDir="C:/TEMP/rbugs/",
                   OpenBugs = FALSE)

## Windows using OpenBUGS
pumps.sim <- rbugs(data = pumps.data, inits, parameters,
                   pumps.model, n.chains = 1, n.iter = 1000,
                   bugs = "C:/Program Files/OpenBugs310/OpenBugs.exe",
                   bugsWorkingDir="C:/TEMP/rbugs/",
                   OpenBugs=TRUE)

## MCMC Analysis
library("coda")
pumps.mcmc <- rbugs2coda(schools.sim)
summary(pumps.mcmc)
effectiveSize(pumps.mcmc)

#endif
#ifdef unix
## The workingDir must be a directory which can be seen by wine
## The nominalWorkingDir gives the directory name recoganizable for
## Windows.
## The full name of the bugs and wine executables need to set.
## In .Renviron, add lines similar to these:
## If using WinBugs
##   BUGS="c:/program files/winbugs14/winbugs14.exe"
##   WINE="/var/scratch/jyan/wine-20040408/wine" 
## If using OpenBUGS
##   BUGS="c:/program files/OpenBUGS310/OpenBugs.exe"
##   WINE="/var/scratch/jyan/wine-20040408/wine" 

pumps.sim <- rbugs(data = pumps.data, inits, parameters, 
                   pumps.model, n.chains = 1, n.iter = 1000,
                   workingDir="/var/scratch/jyan/c/tmp",
                   bugsWorkingDir="c:/tmp/",
                   useWine=TRUE)

## MCMC Analysis
library("coda")
pumps.mcmc <- as.mcmc(pumps.sim$chain1)
summary(pumps.mcmc)
effectiveSize(pumps.mcmc)

#endif
}
}
\keyword{datasets}

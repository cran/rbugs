\name{rbugs}
\alias{rbugs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Run BUGS from R in a BATCH}
\description{
  Generate files (data, init, script) that are necessary to run BUGS,
  call BUGS through the OS, and collect the MCMC output.
}
\usage{
rbugs(data, inits, paramSet, model,
      n.chains = 1, n.iter = 2000, n.burnin = floor(n.iter/2),
      n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/1000)),
      debug = FALSE,
      bugs = Sys.getenv("BUGS"),
      workingDir = getwd(),
      bugsWorkingDir = workingDir,
      useWine = FALSE, wine = Sys.getenv("WINE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a list of data object to be used by BUGS }
  \item{inits}{ a list of list of initial values, or a function which
    returns a list of initial values }
  \item{paramSet}{ a vector of the names of the parameters to be
    monitored }
  \item{model}{ the file name of the model description }
  \item{n.chains}{ the number of chains to be monitored }
  \item{n.iter}{ the number of iteration of each chain }
  \item{n.burnin}{ the length of the burn-in }
  \item{n.thin}{ thinning rate }
  \item{debug}{ a logical value indicating whether or not closing the
    BUGS window }
  \item{bugs}{ the full name (including the path) of the BUGS executable }
  \item{workingDir}{ a directory to store all the intermediate files }
  \item{bugsWorkingDir}{ the nominal name of the working directory,
    which is the same as workingDir is useWine is "FALSE". If useWine is
    "TRUE", it should be a directory which is recognizable by the
    Windows system though wine. }
  \item{useWine}{ a logical value with "TRUE" meaning wine is used }
  \item{wine}{ if useWine==TRUE, the full name of wine }
}
\value{
  A list of MCMC outputs with length \code{nChains}.
}
\examples{
## see examples in \code{\link{pumps}} and \code{\link{schools}}
}
\author{
  Jun Yan \email{jyan@stat.uiowa.edu}
}
\keyword{ interface }

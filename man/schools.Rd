\name{schools}
\alias{schools}
\docType{data}
\title{ 8 schools }
\description{
  8 schools analysis from section 5.5 of "Bayesian Data Analysis" by
  Andrew Gelman, John B. Carlin,  Hal S. Stern, and Donald B. Rubin.
}
\usage{data(schools)}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{school}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E} \code{F} \code{G} \code{H}}
    \item{estimate}{a numeric vector}
    \item{sd}{a numeric vector}
  }
}
\source{
  Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
  \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.
}
\examples{
data(schools)
J <- nrow(schools)
y <- schools$estimate
y <- rnorm(length(y))
sigma.y <- schools$sd
schools.data <- list ("J", "y", "sigma.y") 
## schools.data <- list(J=J, y=y, sigma.y=sigma.y)
inits <- function() {list (theta=rnorm(J,0,100),
                           mu.theta=rnorm(1,0,100),
                           sigma.theta=runif(1,0,100))}
parameters <- c("theta", "mu.theta", "sigma.theta")
schools.bug <- system.file("bugs/model/schools.bug", package="rbugs")
file.show(schools.bug)

\dontrun{
## no tested examples for mac-os.

#ifdef unix
schools.sim <- rbugs(data=schools.data, inits, parameters,
                     schools.bug, n.chains=3, n.iter=1000,
                     bugs="/usr/bin/OpenBUGS",
                     bugsWorkingDir="~/tmp/")
                    
## generate files only
schools.sim <- rbugs(data=schools.data, inits, parameters,
                     schools.bug, n.chains=3, n.iter=1000,
                     bugsWorkingDir="~/tmp/",
                     OpenBugs=TRUE, genFilesOnly=TRUE)

## MCMC analysis
library("coda")
schools.mcmc <- rbugs2coda(schools.sim)
summary(schools.mcmc)
effectiveSize(schools.mcmc)
gelman.diag(schools.mcmc)

#endif
#ifdef windows
## Windows using WinBugs
schools.sim <- rbugs(data=schools.data, inits, parameters,
                     schools.bug, n.chains=3, n.iter=1000,
                     bugs="C:/Program Files/WinBUGS14/WinBUGS14.exe",
                     bugsWorkingDir="C:/temp/", 
                     OpenBugs = FALSE)
                     
## Windows using OpenBUGS
schools.sim <- rbugs(data=schools.data, inits, parameters,
                     schools.bug, n.chains=3, n.iter=1000,
                     bugs="C:/Program Files/OpenBugs321/OpenBugs.exe",
                     bugsWorkingDir="C:/temp/",
                     OpenBugs=TRUE)

##MCMC analysis
library("coda")
schools.mcmc <- rbugs2coda(schools.sim)
summary(schools.mcmc)
effectiveSize(schools.mcmc)
gelman.diag(schools.mcmc)
              
#endif

}
}
\keyword{datasets}

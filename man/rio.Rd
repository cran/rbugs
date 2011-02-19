\name{rio}
\alias{rio}
\docType{data}
\title{ Dengue Fever in the State of Rio de Janeiro }
\description{
  Shape file of the state of Rio de Janeiro
}
\usage{data(rio)}
\format{
  A shape file with 92 observations on the following 9 variables.
  \describe{
    \item{GeoCode}{Rio de Janeiro counties numeric codes}
    \item{Name}{Rio de Janeiro counties names}
    \item{Lat}{Rio de Janeiro counties latitude}
    \item{Long}{Rio de Janeiro counties longitude}
    \item{Pop}{Rio de Janeiro counties population}
    \item{Income}{Rio de Janeiro counties percentage of homes living with less than one minimun salary}
    \item{Urban}{Rio de Janeiro counties percentage of urban area}
    \item{Dengue}{Counts of dengue fever cases by county in the state of Rio de Janeiro}
    \item{E}{Expected counts of dengue fever cases by county in the state of Rio de Janeiro, conditioned on the total
             number of cases the stratified by population rate}
  }
}

\examples{
data(rio)

\dontrun{
library(spdep)
rio.bug <- file.path(.path.package("rbugs"), "bugs/model", "dengue.bug")

## Get the neighbor structure for Rio
rj.nb=poly2nb(rio)
 
## Number of cases by county
Y = rio@data$Dengue

## Get covariates
X <- as.matrix(rio@data[,c("Urban","Income")])

## Get expected values
E <- rio@data$E

## Number of covariates
P <- ncol(X)

## Total counties
N <- length(rj.nb)
 
## Number of neighbors of each county
num <- sapply(rj.nb, length)
 
## Adjacency neighbor list of each county
adj <- unlist(rj.nb)

## Total sum of the number of neighbors in the map
sumNumNeigh <- length(adj)
 
## Set data file
data.rio <- list (N=N, P=P, Y=Y, adj=adj, X=X, E=E, num=num, sumNumNeigh=sumNumNeigh)

## Generate the intial values for spatial vector (u), random noise (v) and covariates effects (beta)
u.aux <- rep(0,N)
v.aux <- rep(0,N)
beta.aux <- rep(0,P)

## Generate the list with the nescessary information in the init file
inits <- list( list(beta = beta.aux, tau.u = 1, tau.v = 1, u=u.aux, v=v.aux, Int=0.0))

## no tested examples for mac-os.

#ifdef unix
rio.sim <- rbugs(data.rio, inits, parameters,
                 rio.bug, n.chains=2, n.iter=60000, 
                 n.burnin = 10000, n.thin = 10, 
                 bugs="/usr/bin/OpenBUGS",
                 bugsWorkingDir="~/tmp/")

## MCMC analysis
library("coda")
rio.mcmc <- rbugs2coda(rio.sim)
summary(rio.mcmc)
effectiveSize(rio.mcmc)
gelman.diag(rio.mcmc)

#endif
#ifdef windows
## Windows using WinBugs
rio.sim <- rbugs(data.rio, inits, parameters,
                 rio.bug, n.chains=2, n.iter=60000, 
                 n.burnin = 10000, n.thin = 10, 
                 bugs="C:/Program Files/WinBUGS14/WinBUGS14.exe",
                 bugsWorkingDir="C:/temp/", 
                 OpenBugs = FALSE)
                   
## Windows using OpenBUGS
rio.sim <- rbugs(data.rio, inits, parameters,
                 rio.bug, n.chains=2, n.iter=60000, 
                 n.burnin = 10000, n.thin = 10, 
                 bugs="C:/Program Files/OpenBugs321/OpenBugs.exe",
                 bugsWorkingDir="C:/temp/",
                 OpenBugs=TRUE)

##MCMC analysis
library("coda")
rio.mcmc <- rbugs2coda(rio.sim)
summary(rio.mcmc)
effectiveSize(rio.mcmc)
gelman.diag(rio.mcmc)
              
#endif

}
}
\keyword{datasets}

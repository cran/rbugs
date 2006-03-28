\name{genBugsScript}
\alias{genBugsScript}
\title{Generating Script File for BUGS}
\description{
  Generating a script file to run BUGS batch-mode.
}
\usage{
genBugsScript(paramSet, n.chains, n.iter, n.burnin, n.thin, dic,
              model.file, data.file, inits.files,
              workingDir=NULL, bugsWorkingDir=getwd(),
              script, debug = FALSE, useWine=FALSE,
              linbugs=TRUE, seed=314159)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paramSet}{A character vector of the names of the
    parameter to be monitored.}
  \item{n.chains}{ The number of chains to be monitored.}
  \item{n.iter}{ The number of iterations of each chain.}
  \item{n.burnin}{ The length of the burn-in. }
  \item{n.thin}{ Thinning rate.}
  \item{dic} {If TRUE, dic will be monitored }
  \item{model.file}{ A character string of the name of file which
    contains the model specification for BUGS. It should end with .txt
    to be recognizable by BUGS.}
  \item{data.file}{ A character string of the name of the data file. It
    should end with .txt to be recognizable by BUGS. }
  \item{inits.files}{ A vector of file names of initial values. They
    should end with .txt to be recognizable by BUGS.}
  \item{workingDir}{ A character string specifying the directory to
    store temporary files for running BUGS.}
  \item{bugsWorkingDir}{ A character string specifying the directory
    which is recognizable by windows, if using wine.}
  \item{script}{ A character string naming the files to print the script
    to. It must end with .txt to be recognizable by BUGS.}
  \item{debug}{ A logical value indicating whether or not closing the
    BUGS window after running the script.}
  \item{useWine}{ If TRUE, BUGS is used via wine.}
  \item{linbugs}{ If TRUE, linbugs is used. }
  \item{seed}{ An integer of random number seed.} 
}
\value{
  None.
}
\details{
  This function only write a script to \file{script}, which uses the
  file names of \file{model.file}, \file{data.file}, and
  \file{inits.files}, without actually reading them. These file names
  should be correct when using \code{\link{runBugs}}.

  workingDir and bugsWorkingDir establish the translation between a
  directory native to the operating system and a directory recognizable
  by wine. They are the same if useWine == FALSE.
}
\author{ Jun Yan \email{jyan@stat.uiowa.edu}}
\seealso{ \code{\link{genDataFile}}, \code{\link{genInitsFile}},
  \code{\link{format4Bugs}}
}
\examples{
## no tested examples for mac-os.
\dontrun{
#ifdef unix
script.file <- paste(tempfile("script"), ".txt", sep="")
genBugsScript(paramSet=c("alpha", "theta"), n.chains=2, n.keep=1500,
              n.burnin=500, n.thin=1,
              model.file="/var/scratch/c/tmp/model.txt",
              data.file="/var/scratch/c/tmp/data.txt",
              inits.file=c("/var/scratch/c/tmp/init1.txt",
                           "/var/scratch/c/tmp/init2.txt"),
              workingDir="/var/scratch/c/tmp",
              bugsWorkingDir="c:/tmp",
              script=script.file,
              debug=FALSE, useWine=TRUE)
file.show(script.file)
unlink(script.file)
#endif
#ifdef windows
script.file <- paste(tempfile("script"), ".txt", sep="")
genBugsScript(paramSet=c("alpha", "theta"), n.chains=2, n.keep=1500,
              n.burnin=500, n.thin=1,
              model.file="c:/tmp/model.txt",
              data.file="c:/tmp/data.txt",
              inits.file=c("c:/tmp/init1.txt",
                           "c:/tmp/init2.txt"),
              workingDir="c:/tmp",
              script=script.file,
              debug=FALSE, useWine=TRUE)
file.show(script.file)
unlink(script.file)
#endif
}
}
\keyword{utilities}
\keyword{interface}

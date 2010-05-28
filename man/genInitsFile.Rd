\name{genInitsFile}
\alias{genInitsFile}
\title{Generating Initial Value Files for BUGS}
\description{
  Generating the files of initial values for MCMC which can be used in
  the script for running BUGS in batch-mode.
}
\usage{
genInitsFile(n.chains, inits, initsFileStem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.chains}{The number of chains to run.}
  \item{inits}{A list of list of initial values or a function which
    returns a list of initial values. See details.}
  \item{initsFileStem}{A character string naming the files to print
    to. See details.}
}
\details{
  if \code{inits} is a list, it should have length \code{n.chains}, and
  each element of the list should be a list which contains one set of
  initial values. If \code{inits} is a function, it will be run
  \code{n.chains} times to generate a list of list of initial values.

  The \code{initsFileStem} is the stem of the file names. The resulted
  file names end with .txt. For example, if \code{n.chains} is 3 and
  \code{initsFileStem} is "init", then the file names will be init1.txt,
  init2.txt, and init3.txt.
}
\value{
  None.
}
\author{ Jun Yan \email{jyan@stat.uiowa.edu} and Marcos Prates \email{marcos.prates@uconn.edu} }
\seealso{  \code{\link{genDataFile}}, \code{\link{genBugsScript}}}
\examples{
## when inits is a list:
inits <- list(list(alpha=1, beta=2), list(alpha=4, beta=4))
genInitsFile(2, inits, "foo.init")
file.show("foo.init1.txt")
file.show("foo.init2.txt")
## when inits is a function:
inits <- function() list(alpha=rnorm(2, sd=100),
                         beta=rgamma(1, 0.1, 0.001))
genInitsFile(2, inits, "foo.init")
file.show("foo.init1.txt")
file.show("foo.init2.txt")
unlink("foo.init1.txt")
unlink("foo.init2.txt")
}
\keyword{utilities}

\name{genDataFile}
\alias{genDataFile}
\title{Generating the Data File for BUGS}
\description{
  Generating the data file which can be used in the script file for
  runing BUGS batch-mode.
}
\usage{
genDataFile(dataList, dataFile)
}
\arguments{
  \item{dataList}{A list of data that are needed by BUGS. Its elements
    must be numeric vector or matrices.}
  \item{dataFile}{ A character string naming the file to print to. It
    must end with .txt to be recoganizable by BUGS.}
}
\value{
  None.
}
\author{Jun Yan \email{jyan@stat.uconn.edu} and Marcos Prates \email{marcos.prates@uconn.edu}}
\seealso{ \code{\link{genInitsFile}}, \code{\link{genBugsScript}},
  \code{\link{format4Bugs}}
}
\examples{
dat <- list(a = runif(1), b=rnorm(2), c=matrix(rexp(4), 2, 2))
genDataFile(dat, "foo.txt")
file.show("foo.txt")
unlink("foo.txt")
}
\keyword{utilities}

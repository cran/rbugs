\name{format4Bugs}
\alias{format4Bugs}
\title{ Format a Data List as BUGS Data Input}
\description{
  Transform a data list into a format which can be read in as data by BUGS.
}
\usage{
format4Bugs(dataList, digits = 5)
}
\arguments{
  \item{dataList}{ A list of data to be formatted.}
  \item{digits}{The desired number of significant digits.}
}
\value{
  A charactor vector of data readable by BUGS.
}
\author{ Jun Yan \email{jyan@stat.uiowa.edu} and Marcos Prates \email{marcosop@est.ufmg.br}}
\examples{
dat <- list(a = runif(1), b=rnorm(2), c=matrix(rexp(4), 2, 2))
format4Bugs(dat)
}
\keyword{utilities}
\keyword{interface}

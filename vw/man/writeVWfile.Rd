\name{writeVWfile}
\alias{writeVWfile}
\title{Write Vowpal Wabbit data}
\usage{
writeVWfile(y, X, case_weights = NULL, namespaces = NULL,
  file = tempfile())
}
\arguments{
  \item{y}{the target variable}

  \item{X}{the data.frame of X variables.  Can include
  numeric and character data}

  \item{case_weights}{the weights for each observations}

  \item{namespaces}{the namespace to which each variable is
  assigned}

  \item{file}{the file to write the data to.}
}
\value{
The path of the file
}
\description{
This function writes data in vowpal wabbit format
}
\examples{
data(iris)
f <- writeVWfile(runif(6), head(iris))
f
cat(readChar(f, file.info(f)$size))
unlink(f)
}
\references{
http://stackoverflow.com/questions/11940573/r-convert-data-frame-to-input-file-improve-performance
}


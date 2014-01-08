\name{cacheVW}
\alias{cacheVW}
\title{Write Vowpal Wabbit data to the VW cache}
\usage{
cacheVW(y, X, case_weights = NULL, namespaces = NULL, run_checks = TRUE,
  vw_cache = getOption("vw_cache"))
}
\arguments{
  \item{y}{The target variable}

  \item{X}{The data.frame of X variables.  Can include
  numeric and character data}

  \item{case_weights}{The weights for each observations}

  \item{namespaces}{The namespace to which each variable is
  assigned}

  \item{run_checks}{Whether to run the vw check function}
}
\value{
The path of the file
}
\description{
This function writes data in vowpal wabbit format to the VW
cache folder
}
\examples{
data(iris)
f <- cacheVW(runif(6), head(iris))
f
cat(readChar(f, file.info(f)$size))
unlink(f)
}
\references{
http://stackoverflow.com/questions/11940573/r-convert-data-frame-to-input-file-improve-performance
}


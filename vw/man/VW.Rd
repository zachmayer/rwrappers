\name{VW}
\alias{VW}
\title{Fit a Vowpal Wabbit model}
\usage{
  VW(y = NULL, X = NULL, case_weights = NULL,
    namespaces = NULL, file = NULL, control = vwControl())
}
\arguments{
  \item{y}{the target variable}

  \item{X}{the data.frame of X variables.  Can include
  numeric and character data}

  \item{case_weights}{the weights for each observations}

  \item{namespaces}{the namespace to which each variable is
  assigned}

  \item{file}{the previously-saved file of vowpal wabbit
  data}

  \item{control}{the control parameters for the vw
  function}
}
\value{
  An object of class VowpalWabbit
}
\description{
  Given an iput dataset, this function caches the data and
  fits a vw model to the cached data.
}
\examples{
data(iris)
model <- VW(iris[,1], iris[,-1], control=vwControl(invert_hash=paste0(getOption('vw_cache'), '/model.text')))
cat(file=model$control$invert_hash)
unlink(model$control$final_regressor)
unlink(model$control$cache_file)
unlink(model$control$data)
unlink(model$control$invert_hash)
}


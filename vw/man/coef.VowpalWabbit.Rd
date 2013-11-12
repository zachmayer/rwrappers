\name{coef.VowpalWabbit}
\alias{coef.VowpalWabbit}
\title{Show the coefficients of a VW model}
\usage{
  coef.VowpalWabbit(model, data = tempfile(),
    predictions = tempfile(), cache_warning = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{model}{an object of class VowpalWabbit}

  \item{data}{a file to save temp data to when getting the
  CFs}

  \item{A}{file to save temp predictions to}

  \item{cache_warning}{Warn about caching and cash
  inversion}

  \item{verbose}{Whether to list issues as we try to read
  the CFs from a file}
}
\value{
  A matrix
}
\description{
  #TODO: use new, "reversed hash" human readable model
}


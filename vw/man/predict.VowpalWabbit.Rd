\name{predict.VowpalWabbit}
\alias{predict.VowpalWabbit}
\title{Make predictions from a vowpal wabbit model}
\usage{
\method{predict}{VowpalWabbit}(model, X = NULL, y = NULL, file = NULL,
  case_weights = NULL, default_y = 0, predictions = tempfile(),
  verbose = TRUE, ...)
}
\arguments{
  \item{model}{an object of class VowpalWabbit}

  \item{X}{the data to use for predicting}

  \item{file}{the new data file for predicting}

  \item{case_weights}{Case weights for the test set}

  \item{predictions}{the file to save the predicitons to}
}
\value{
A vector or a matrix
}
\description{
Make predictions from a vowpal wabbit model
}


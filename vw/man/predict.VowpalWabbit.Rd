\name{predict.VowpalWabbit}
\alias{predict.VowpalWabbit}
\title{Make predictions from a vowpal wabbit model}
\usage{
  predict.VowpalWabbit(model, X = NULL, file = NULL,
    case_weights = NULL, predictions = tempfile(),
    return_all = FALSE, ...)
}
\arguments{
  \item{model}{an object of class VowpalWabbit}

  \item{X}{the data to use for predicting}

  \item{file}{the new data file for predicting}

  \item{case_weights}{Case weights for the test set}

  \item{predictions}{the file to save the predicitons to}

  \item{Whether}{to return predictions for ALL passes of
  the models}
}
\value{
  A vector or a matrix
}
\description{
  Make predictions from a vowpal wabbit model
}


\name{update.VowpalWabbit}
\alias{update.VowpalWabbit}
\title{Update a vowpal wabbit model}
\usage{
update.VowpalWabbit(model, passes = 1, final_regressor = tempfile(),
  readable_model = NULL, verbose = TRUE)
}
\arguments{
  \item{model}{an object of class VowpalWabbit}

  \item{passes}{the number of additional passes to fit}

  \item{final_regressor}{the path for saving the new model}

  \item{readable_model}{the path to save the readable model
  (really the hashes)}
}
\value{
An object of class VowpalWabbit
}
\description{
This function starts with an existing vowpal wabbit model,
and then runs some more passes
}


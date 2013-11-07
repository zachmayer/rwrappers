\name{vwControl}
\alias{vwControl}
\title{VW Control}
\usage{
  vwControl(vw_path = getOption("vw_path"),
    final_regressor = NULL, cache_file = NULL, passes = 1,
    compressed = TRUE, loss_function = "squared",
    noconstant = FALSE, l1 = 1, l2 = 1, quadratic = FALSE,
    sort_features = FALSE, audit = FALSE, quiet = FALSE,
    adaptive = FALSE, exact_adaptive_norm = FALSE,
    nonormalize = FALSE, conjugate_gradient = FALSE,
    bfgs = FALSE, ...)
}
\arguments{
  \item{vw_path}{...}

  \item{final_regressor}{...}

  \item{cache_file}{}

  \item{passes}{}

  \item{compressed}{}

  \item{loss_function}{}

  \item{noconstant}{}

  \item{l1}{}

  \item{l2}{}

  \item{quadratic}{}

  \item{sort_features}{}

  \item{audit}{}

  \item{quiet}{}

  \item{adaptive}{}

  \item{exact_adaptive_norm}{}

  \item{nonormalize}{}

  \item{conjugate_gradient}{}

  \item{bfgs}{}

  \item{...}{Additional parameters.  DANGER! If these are
  wrong, VW will crash}
}
\value{
  A list of control parameters
}
\description{
  This function builds a list of command line arguments for
  vowpal wabbit.  I'm lazy and haven't full documented this
  yet, so see vwHelp()
}
\examples{
vwControl()
}
\references{
  https://github.com/JohnLangford/vowpal_wabbit/wiki/Command-line-arguments
}


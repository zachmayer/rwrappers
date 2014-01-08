\name{checkVW}
\alias{checkVW}
\title{Some checks for the setup of the VW environment}
\usage{
checkVW(vw_path = getOption("vw_path"), vw_cache = getOption("vw_cache"))
}
\arguments{
  \item{vw_path}{the path to the vw executible}
}
\description{
This checks that the path to the vw executible is set and
that a file exists.  It also checks that the VW cache has
been set
}
\examples{
checkVW()
}


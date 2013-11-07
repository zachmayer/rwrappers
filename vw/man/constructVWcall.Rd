\name{constructVWcall}
\alias{constructVWcall}
\title{Construct vw call}
\usage{
  constructVWcall(control = vwControl(help = TRUE))
}
\arguments{
  \item{control}{A list of control parameters for VW}
}
\value{
  A string
}
\description{
  This function builds a string from the list of VW control
  parameters.  This string will be sent to the command line
  to call the VW algoryhtm.
}
\examples{
constructVWcall()
}


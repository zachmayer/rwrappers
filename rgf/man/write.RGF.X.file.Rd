\name{write.RGF.X.file}
\alias{write.RGF.X.file}
\title{Write the RGF X matrix to a file}
\usage{
  write.RGF.X.file(X, finalTrainFile = tempfile())
}
\arguments{
  \item{X}{A data.frame}

  \item{finalTrainFile}{the file to write the matrix to}
}
\value{
  The path of the file
}
\description{
  This function writes a data.frame to the RGF file format
}
\examples{
data(iris)
f <- write.RGF.X.file(head(iris))
cat(readChar(f, file.info(f)$size))
unlink(f)
}


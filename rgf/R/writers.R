#' Write the RGF X matrix to a file
#' 
#' This function writes a data.frame to the RGF file format
#' 
#' @param X A data.frame
#' @param finalTrainFile the file to write the matrix to
#' @export
#' @return The path of the file
#' @examples 
#' data(iris)
#' f <- write.RGF.X.file(head(iris))
#' cat(readChar(f, file.info(f)$size))
#' unlink(f)
write.RGF.X.file <- function(X, finalTrainFile=tempfile()){
  require('caret')
  require('e1071')
  
	tempFile1 <- tempfile()

	dv1 <- dummyVars( ~., data = X, sparse = T)
	X2 <- as.matrix.csr(predict(dv1, X))
	write.matrix.csr( as(predict(dv1, X), "matrix.csr"), tempFile1 ,  fac = TRUE)
	
	header1 <- paste('sparse ',max(X2@ja)+1,sep = '')

	temp1 <- as.vector(readLines(con = tempFile1))
	write(c(header1, temp1), finalTrainFile)
  
  unlink(tempFile1)
  return(finalTrainFile)
}

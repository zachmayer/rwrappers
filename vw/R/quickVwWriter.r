

quickVwWriter <- function(y, X,file){
vtu <- names(X)
FactorCols1 <- sapply(X, function(col) is.factor(col))
factorCols <- vtu[FactorCols1]
nonFactorCols1 <- sapply(X, function(col) !is.factor(col))
numericCols <- vtu[nonFactorCols1]

score  <- y
score <- ifelse(score == 0, -1, 1)

train3 <- X[,factorCols]
noSpace <- function(x){
	if(class(x) %in% c('factor','character')){
	x <- as.character(x)
	x <- gsub('\\s','',x)
	x <- as.factor(x)
	return(x)
	}
}

#head(X)
train3 <- apply(train3, 2, noSpace)
head(train3)
train3  <- as.data.frame(apply(train3,2,factor))

cols   <- colnames(train3)
res <- apply(train3, 1, function(x) {
  idx  <- x != 0
  nms  <- cols[idx]
  vals <- x[idx]
  paste(nms, vals, sep=" ", collapse=" |")
})

train4 <- X[,numericCols]
cols2   <- colnames(train4)
res2 <- apply(train4, 1, function(x) {
  idx  <- x != 0
  nms  <- cols2[idx]
  vals <- x[idx]
  paste(nms, vals, sep=":", collapse=" |")
})


out <- paste(score, " |", as.vector(res)," |",  as.vector(res2), sep = '')
#print(out)

  write(out, file)
  
}

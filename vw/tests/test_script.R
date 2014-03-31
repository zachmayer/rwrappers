# #Setup
# set.seed(36601)
# gc(reset=TRUE)
# setwd('C:/Users/zmayer/Dropbox/Projects/zOld Projects/VW')
# options(vw_path='C:/vw/vw.exe')
# options(vw_cache='C:/vw_cache')
# 
# #Create Dataset
# #options(vw_path='C:/vw/vw.exe')
# set.seed(1234)
# dat <- iris
# trainrows <- runif(nrow(dat)) < .66
# X <- dat[trainrows, -4]
# y <- dat[trainrows, 4]
# testX <- dat[!trainrows, -4]
# testY <- dat[!trainrows, 4]
# 
# #Fit Model
# model <- VW(y, X, control=vwControl(
#   passes=3, l1=.0001, l2=.0001, 
#   readable_model=tempfile(),
#   noconstant=TRUE,
#   vw_path='C:/vw/vw.exe'))
# pred <- predict(model, testX)
# model
# coef(model)
# summary(pred)
# plot(pred, testY, col=dat[!trainrows, 'Species'])
# sqrt(mean((pred-testY)^2))
# 
# #Update Model
# model2 <- update(model, passes=5, readable_model=tempfile())
# pred2 <- predict(model2, testX)
# model2
# summary(pred2)
# plot(pred2, testY, col=dat[!trainrows, 'Species'])
# sqrt(mean((pred2-testY)^2))
# 
# #Fit a model with weights
# model3 <- VW(y, X, case_weights=runif(nrow(X)), namespaces=c('n', 'n', 'n', 'c'),
#              control=vwControl(
#                passes=25, 
#                l1=.0001, 
#                l2=.0001, 
#                vw_path='C:/vw/vw.exe',
#                readable_model=tempfile()))
# pred3 <- predict(model, testX)
# model3
# summary(pred3)
# plot(pred3, testY, col=dat[!trainrows, 'Species'])
# sqrt(mean((pred3-testY)^2))
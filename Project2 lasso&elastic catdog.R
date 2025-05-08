library(repr)
library(caret)
library(glmnet)

catdogdata<-read.table("catdogdata.txt") #rows are pictures

dog<-rep(0,198)
dog[100:198]<-1    #99 dogs resp cats, 50/50 cats/dogs in data so will do stratified sampling
catdogdata <- cbind(dog, catdogdata)
catdogdata <- as.matrix(catdogdata)

rotateM <- function(x) t(apply(x, 2, rev)) # the images are raster scans. Here, I just resort them for the
# default image command in R to plot them with the right orientation.

numfeatlasso <- list()     #collecting the feature selections for the 3 different seeds
numfeatelastinet <- list()
lassofeatpicts <- list()
elastifeatpicts <- list()
lassotestacc_kNN <- list()
elastinettestacc_kNN <- list()
lassotestacc_LDA <- list()
elastinettestacc_LDA <- list()
lassotestacc_nnet <- list()
elastinettestacc_nnet <- list()
iter <- 0
for (i in c(13057, 35382, 28673)) {  #creating or loop for different train test splits
  iter <- iter + 1
  set.seed(i)

#Train test split for holdout

train_idx <- sample(1:198, 0.8*198)
train_idx <- sort(train_idx)
test_idx <- 1:198
test_idx <- test_idx[-train_idx]     

train_data <- catdogdata[train_idx,]
test_data <- catdogdata[test_idx,]


#beginning feature selection methods
lassocv <- cv.glmnet(train_data[,-1],train_data[,1], alpha = 1) #Fitting lasso model and parameter tuning
elastinet <- cv.glmnet(train_data[,-1],train_data[,1], alpha = 0.5)
plot(lassocv) #plot for number of features to select
plot(elastinet)

numfeatlasso[[iter]] <- lassocv
numfeatelastinet[[iter]] <- elastinet

#Extracting features and creating data for hyperparameter optimization + eventual fitting of model
lassofeat <- coef(glmnet(train_data[,-1],train_data[,1], alpha = 1, lambda = lassocv$lambda.1se )) 
lassofeat <- labels(lassofeat[lassofeat[,1]!=0,])
elastinetfeat <- coef(glmnet(train_data[,-1],train_data[,1], alpha = 0.5, lambda = elastinet$lambda.1se )) 
elastinetfeat <- labels(elastinetfeat[elastinetfeat[,1]!=0,])
lassofeat[1] <- "V1"
elastinetfeat[1] <- "V1"
lassofeat <- c("dog", lassofeat)
elastinetfeat <- c("dog", elastinetfeat)

lasso_model_train <- train_data[,lassofeat]
lasso_model_test <- test_data[,lassofeat]
elastinet_model_train <- train_data[,elastinetfeat]
elastinet_model_test <- test_data[,elastinetfeat]


#Plotting features
lassopixels <- c()
elastinetpixels <- c()
for (lab in lassofeat[-1]) {
  lassopixels <- c(lassopixels, which(colnames(train_data[,-1]) == lab))
}

for (lab in elastinetfeat[-1]) {
  elastinetpixels <- c(elastinetpixels, which(colnames(train_data[,-1]) == lab))
}

lassopict <- rep(0, 64*64)
elastinetpict <- rep(0,64*64)
lassopict[lassopixels] <- 256
elastinetpict[elastinetpixels] <- 256
lassopict <- matrix(lassopict, ncol = 64)
elastinetpict <- matrix(elastinetpict, ncol = 64)

lassofeatpicts[[iter]] <- lassopict
elastifeatpicts[[iter]] <- elastinetpict


fitControl <- trainControl(## 10-fold CV 5 times
  method = "repeatedcv",
  number = 10,
  repeats = 5, verboseIter = TRUE)


#The below tunes and  fits knn on the training set and calculates train and test accuracy
lassoknnfit <- train(as.data.frame(lasso_model_train[,-1]), as.factor(lasso_model_train[,1]), method ="knn", trControl = fitControl, tuneGrid = data.frame(k = c(1,2,3,4,5,6,7,8,9,10)))
elastiknnfit <- train(as.data.frame(elastinet_model_train[,-1]), as.factor(elastinet_model_train[,1]), method ="knn", trControl = fitControl, tuneGrid = data.frame(k = c(1,2,3,4,5,6,7,8,9,10)))
par(mfrow = c(1,2))
plot(lassoknnfit, xlab = "k", ylab = "Accuracy, lassofit")
plot(elastiknnfit, xlab = "k", ylab = "Accuracy, elastinetfit")

#testaccuracy
lassobestknn <- class::knn(lasso_model_train[,-1], lasso_model_test[,-1], lasso_model_train[,1], k = lassoknnfit$bestTune)
lassoconfknn <- confusionMatrix(table(lassobestknn, lasso_model_test[,1]))


elastibestknn <- class::knn(elastinet_model_train[,-1], elastinet_model_test[,-1], elastinet_model_train[,1], k = elastiknnfit$bestTune)
elasticonfknn <- confusionMatrix(table(elastibestknn, elastinet_model_test[,1]))


lassotestacc_kNN[[iter]] <- lassoconfknn
elastinettestacc_kNN[[iter]] <- elasticonfknn

#trainaccuracy
#lassobestknn2 <- class::knn(lasso_model_train[,-1], lasso_model_train[,-1], lasso_model_train[,1], k = lassoknnfit$bestTune)
#lassoconfknn2 <- confusionMatrix(table(lassobestknn2, lasso_model_train[,1]))
#lassoconfknn2

#elastibestknn2 <- class::knn(elastinet_model_train[,-1], elastinet_model_train[,-1], elastinet_model_train[,1], k = elastiknnfit$bestTune)
#elasticonfknn2 <- confusionMatrix(table(elastibestknn2, elastinet_model_train[,1]))
#elasticonfknn2

#The below fits LDA and calc train and test accuracy
lassolda <- train(as.data.frame(lasso_model_train[,-1]), as.factor(lasso_model_train[,1]), method = "lda", trControl = fitControl)
elastilda <- train(as.data.frame(elastinet_model_train[,-1]), as.factor(elastinet_model_train[,1]), method = "lda", trControl = fitControl)

#testacc
lassopredlda <- predict(lassolda, newdata = lasso_model_test[,-1])
lassoldaconf <- confusionMatrix(table(lassopredlda, lasso_model_test[,1]))

lassotestacc_LDA[[iter]] <- lassoldaconf

elastinetlda <- predict(elastilda, newdata = elastinet_model_test[,-1])
elastildaconf <- confusionMatrix(table(elastinetlda, elastinet_model_test[,1]))

elastinettestacc_LDA[[iter]] <- elastildaconf
#The below tunes and fits neural network classifiers with one hidden layer
nnetGrid <-  expand.grid(size = seq(from = 3, to = 8, by = 1),        #which parameters to tune for
                         decay = seq(from = 0.1, to = 0.5, by = 0.1)) 
set.seed(123)
lassonetwork <- train(as.data.frame(lasso_model_train[,-1]), as.factor(lasso_model_train[,1]), method = "nnet", trControl = fitControl, tuneGrid = nnetGrid)
set.seed(123)
elastinetwork <- train(as.data.frame(elastinet_model_train[,-1]), as.factor(elastinet_model_train[,1]), method = "nnet", trControl = fitControl, tuneGrid = nnetGrid)


lassonetwpred <- predict(lassonetwork, newdata = lasso_model_test[,-1])
elastinetwpred <- predict(elastinetwork, newdata = elastinet_model_test[,-1])
lassonetwconf <- confusionMatrix(table(lassonetwpred,lasso_model_test[,1]))
elastinetwconf <- confusionMatrix(table(elastinetwpred,elastinet_model_test[,1]))

lassotestacc_nnet[[iter]] <- lassonetwconf
elastinettestacc_nnet[[iter]] <- elastinetwconf

}
#very cool :)


#stuff for plotting pictures 

#set.seed(1000012)
ssc<-sample(seq(1,198)[dog==0],2,replace=F)     #getting two random cats for plotting purposes
ssd<-sample(seq(1,198)[dog==1],2,replace=F)     #getting two random dogs for plotting purposes
#options(repr.plot.width=12, repr.plot.height=6)
par(mfrow=c(1,2))
image(seq(1,64),seq(1,64),rotateM(matrix(catdogdata[ssc[1],-1],64,64)),col=gray.colors(256),xlab="",ylab="")
image(seq(1,64),seq(1,64),rotateM(matrix(catdogdata[ssc[2],-1],64,64)),col=gray.colors(256),xlab="",ylab="")
image(seq(1,64),seq(1,64),rotateM(matrix(catdogdata[ssd[1],-1],64,64)),col=gray.colors(256),xlab="",ylab="")
image(seq(1,64),seq(1,64),rotateM(matrix(catdogdata[ssd[2],-1],64,64)),col=gray.colors(256),xlab="",ylab="")

par(mfrow = c(1,2))
image(seq(1,64),seq(1,64),rotateM(lassofeatpicts[[3]]),col=gray.colors(256),xlab="",ylab="")
image(seq(1,64),seq(1,64),rotateM(elastifeatpicts[[3]]),col=gray.colors(256),xlab="",ylab="")


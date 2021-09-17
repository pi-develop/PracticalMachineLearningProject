library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(rattle)
library(corrplot)
set.seed(90214)
training<-read.csv("~/Rcoursera/pml-training.csv")
testing<-read.csv("~/Rcoursera/pml-testing.csv")
dim(training)
dim(testing)
names(training)
names(testing)
training<-training[,-c(1:7)]
training <- training[,colMeans(is.na(training)) < .9]
nvz <- nearZeroVar(training)
training <- training[,-nvz]
dim(training)
inTrain <- createDataPartition(y=training$classe, p=0.7, list=F)
traindata <- training[inTrain,]
validata <- training[-inTrain,]
control <- trainControl(method="cv", number=3, verboseIter=F)
modtrees <- train(classe~., data=traindata, method="rpart", trControl = control, tuneLength = 5)
fancyRpartPlot(modtrees$finalModel)
predtrees <- predict(modtrees, validata)
cmtrees <- confusionMatrix(predtrees, factor(validata$classe))
cmtrees
modgbm <- train(classe~., data=traindata, method="gbm", trControl = control, tuneLength = 5, verbose = F)
modrf <- train(classe~., data=traindata, method="rf", trControl = control, tuneLength = 5)
predrf <- predict(modrf, valid)
cmrf <- confusionMatrix(predrf, factor(valid$classe))
cmrf
predgbm <- predict(modgbm, valid)
cmgbm <- confusionMatrix(predgbm, factor(valid$classe))
cmgbm
predict(modrf, testing)


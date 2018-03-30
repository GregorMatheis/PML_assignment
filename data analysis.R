library(tidyverse)
library(data.table)
library(caret)
library(readr)
library(doParallel)

pml_training<-read.csv("Data/pml-training.csv",  na.strings = c("NA", "#DIV/0!", ""))
pml_test<-read.csv("Data/pml-testing.csv",  na.strings = c("NA", "#DIV/0!", ""))

pml_training<-pml_training[,!grepl("^X|timestamp|window", names(pml_training))]
pml_test<-pml_test[,!grepl("^X|timestamp|window", names(pml_training))]

classe<-pml_training$classe
pml_training<-pml_training[,sapply(pml_training, is.numeric)]
pml_training$classe<-classe
cols<-apply(pml_training,2, function(x)any(is.na(x)))
cols<-names(cols[!cols])
pml_training<-pml_training[,cols]


problem_id<-pml_test$problem_id
pml_test<-pml_test[,sapply(pml_test, is.numeric)]
pml_test<-pml_test[,cols[1:52]]
pml_test$problem_id<-problem_id

inTrain<-createDataPartition(pml_training$classe, p=.7, list = F)
pml_train<-pml_training[inTrain,]
pml_train_test<-pml_training[-inTrain,]

controlRf <- trainControl(method="cv", 5)

registerDoParallel(cores = 8)
model1_rf<-
      train(classe~., data=pml_train,
            method="parRF", trControl=controlRf, 
            importance=TRUE, ntree=100) #parRF
model1_rf
model1_rf_pred<-predict(model1_rf, pml_train_test)
confusionMatrix(pml_train_test$classe, model1_rf_pred)


predict(model1_rf, newdata = pml_test)

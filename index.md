---
title       : Johns Hopkins University Practical Machine Learning Project
subtitle    : 
author      : Amir Abbas Darbanibasmanj
job         : Data_Driven Marketer and Data Mining Specialist | amirabbas.darbani@gmail.com
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Step 0 : loading the required Packages

library(caret)

library(AppliedPredictiveModeling)

library(kernlab)

library(randomForest)

--- 

## STEP 1 : Downloading Data: First We Download te data in the specific destination

setInternet2(TRUE)

trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

download.file(trainUrl, destfile = "D:/Coursera/Practical Machin Learning/Project/Data/train.csv")


testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(testUrl, destfile = "D:/Coursera/Practical Machin Learning/Project/Data/test.csv")

--- 

## STEP 2 : Reading Data: Then we read the data from working directory (Note: I used ----> na.strings=c("NA","#DIV/0!","") In order to replace all missing values with NA)

traindata <- read.csv("D:/Coursera/Practical Machin Learning/Project/Data/train.csv",  na.strings=c("NA","#DIV/0!",""), header= TRUE)

testdata <- read.csv("D:/Coursera/Practical Machin Learning/Project/Data/test.csv", na.strings=c("NA","#DIV/0!",""), header= TRUE)

--- 

## STEP 3 : Cleaning Data : A Summary of the Train data shows that all variables with missing values, NA, should be removed as follow:

traindata <- traindata[ , colSums(is.na(traindata)) == 0]

testdata <-  testdata[ , colSums(is.na(testdata)) == 0]

nearZeroColumns <- nearZeroVar(traindata, saveMetrics = TRUE)

traindata <- traindata[, nearZeroColumns$nzv==FALSE]

traindata$X <- NULL            # we do this so it does not interfer with ML Algorithms

traindata$user_name <- NULL

traindata$cvtd_timestamp <- NULL

traindata$row.names <- NULL

traindata$new_window <- NULL

traindata$raw_timestamp_part_1 <- NULL

traindata$raw_timestamp_part_2 <- NULL

traindata$num_window <- NULL



testdata$X <- NULL

testdata$user_name <- NULL

testdata$problem_id <- NULL

testdata$cvtd_timestamp <- NULL

testdata$row.names <- NULL

testdata$new_window <- NULL

--- 

## STEP 4 : Spliting the train data set into two parts (60% and 40%)

trainIndex <- createDataPartition(y = traindata$classe, p=0.6,list=FALSE)

trainPartition <- traindata[trainIndex,]

testPartition <- traindata[-trainIndex,]

--- 

## STEP 5 : Creating ML models with different 'methods'

set.seed(2434)

model_nb <- train(classe ~ .,  method="nb", data=trainPartition)

model_gbm <- train(classe ~ ., method = "gbm", data = trainPartition)

model_f <- train(classe ~ .,  method ="rf", data=trainPartition)

--- 

## STEP 6 : Analysing the accuracy of the models

print("Naive Bayes")

nb_accuracy <- predict(model_nb, testPartition)

print(confusionMatrix(nb_accuracy, testPartition$classe)) ### ---> IF you run it, it will give the accuracy = 0.7375


print("Stochastic Gradient Boosting")

gbm_accuracy <- predict(model_gbm, testPartition)

print(confusionMatrix(gbm_accuracy, testPartition$classe)) ### ---> IF you run it, it will give the accuracy = 0.961


print("Random Forest")

rf_accuracy <- predict(model_f, testPartition)

print(confusionMatrix(rf_accuracy, testPartition$classe))  ### ---> IF you run it, it will give the accuracy = 0.9897

--- 

## STEP 7 : Tuning and Cross Validation (CV)

set.seed(2345)
fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model_rf_CV <- train(classe ~ ., method="rf",  data=trainPartition, trControl = fitcontrol)

rf_CV_accuracy <- predict(model_rf_CV, testPartition)
print(confusionMatrix(rf_CV_accuracy, testPartition$classe)) ### ---> If you run it, it will give the accuracy = 0.9895

--- 

## STEP 8 : Predicting the 20 cases of the Test dataset

prediction <- predict(model_rf_CV, testdata)
print(prediction)

Result : [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E

--- 

## For Fast Computing the following packages could be helpful:


library(parallel)

library(doParallel)

registerDoParallel(makeCluster(detectCores()))

--- 

## Thank You For Watching my Presentation

Amir Abbas Darbanibasmanj

Data-Driven Marketer and Data Mining Specialist

amirabbas.darbani@gmail.com

Find me on:

Linkedin
&
Twitter

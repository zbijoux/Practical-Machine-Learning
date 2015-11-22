---
title: "Practical Machine Learning"
author: "zbijoux"
date: "November 21, 2015"
output: html_document
---

# Practical Machine Learning Project Report

## Introduction
A group of enthusiasts who take measurements about themselves regularly to improve their
health,to find patterns in their behavior.They regularly do quantify how much of a particular
activity they do, but they rarely quantify how well they do it. Using devices such as Jawbone
Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about
personal activity relatively inexpensively. These type of devices are part of the quantified
self movement.In this project, we will use data from accelerometers on the belt, forearm, arm,
and dumbell of6 participants to predict the manner in which they did the exercise.
The goal of your project is to predict the manner in which they did the exercise. 

# Loading packages
```{r echo=TRUE}
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(ggplot2)
library(lattice)
library(grid)
library(corrplot)
```

# Getting and Cleaning The Data
**1. Downloading Data and Reading Files**
```{r echo=TRUE}
if (!file.exists("data")) {dir.create("data")}

url1 <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url1,"./data/pml-training.csv")
download.file(url2,"./data/pml-testing.csv" )
dateDownloaded <- date()

datatr <- read.csv("./data/pml-training.csv", na.strings= c("NA",""," "))
datat <- read.csv("./data/pml-testing.csv", na.strings= c("NA",""," "))
dim(datatr)
dim(datat)
testing <- datat
```

**2. Clean the data by removing columns with NAs**
* Calculating the total number of missing values 

```{r echo=TRUE}
table(colSums(is.na(datatr)))

```

There are 100 columns with 19216 missing values(NAs) and 60 columns with zero missing values. Next removing missing values with more than 80% per column.

```{r echo=TRUE}
set.seed(2345)
# remove variables with more than 80% missing values
datana <- sapply(colnames(datatr), function(x) if(sum(is.na(datatr[, x])) > 0.8*nrow(datatr))
                          {return(T)}else{return(F)})
datr1 <- datatr[, !datana]
training <- datr1
dim(datr1)
```

Calculating the correlation between the response and the dependent variables using the spearman correlation since are response is class(factor) variable. The first seven columns were remove from the data since they do not have any relation to the objective of the data analysis.

```{r echo=TRUE}
# remove near zero covariates
nsv <- nearZeroVar(training,saveMetrics = TRUE)
training <- training[,!nsv$nzv]

 ## removing identifier columns such as name, timestamps etc
training = training[,-c(1:7)]
dim(training)

# calculate correlations
cor <- abs(sapply(colnames(training[, -ncol(training)]), function(x) 
  cor(as.numeric(training[, x]), as.numeric(training$classe), method = "spearman")))

summary(cor)

## plot a correlation matrix
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))

```

The final dataset retain for the training data had 19622 observations and 52 variables. From the correlation matrix there seems to be no strong correlation between the outcome variables classe and the predictor variables.

## Data Modelling
The Random forest method will be used predict this exercise since it is robust method to outliers and can handle categorical predictors naturally and missing values. Fit model with random forests algorithm and 10-fold cross validation to predict classe with all other predictors.

```{r echo=TRUE}
set.seed(12345)
controlRf <- trainControl(method="cv", 10)
modelRf <- train(classe ~ ., data=training, method="rf",trControl=controlRf,
                 importance=T,ntree=250)
modelRf
plot(modelRf)
```

The final value used for the model had accuracy close to 1.
```{r echo=TRUE}
print(modelRf$finalModel)

```

## Predicting the Test Data
Once we have trained the model on the training data, we can test the accuracy using the testing data we left out. Let's define accuracy as the percentage of correct predictions from the model (comparing the predictions from the model to the actual classe variable in the testing data).

```{r echo=TRUE}
# removing the first seven columns

testing <-testing[,-c(1:7)]

prediction <- as.character(predict(modelRf, testing))
prediction
result <- predict(modelRf, testing)
result

```

## Conclusion
The final random forests model contains 500 trees with 2 variables tried at each split. The estimated out of sample error rate for the random forests model is 0.45% as reported by the final model.

## Appendix
* plot a correlation matrix

```{r echo=TRUE}
# plot a correlation matrix
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))

plot(training[, names(which.max(cor))], training[, names(which.max(cor[-which.max(cor)]))], 
     col = training$classe, pch = 19, cex = 0.1, xlab = names(which.max(cor)),
     ylab = names(which.max(cor[-which.max(cor)])))

```


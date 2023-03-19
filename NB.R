#install.packages('naivebayes')
library(naivebayes)
library(dplyr)
library(datasets)
library(caTools)
library(e1071)

setwd("~/Exam1Things")

Record_DF <- read.csv('Decision_tree.csv', stringsAsFactors=TRUE) # all non-numeric columns should be factors
Record_DF <- Record_DF[,-c(1)]
head(Record_DF)

set.seed(123)
sample_data = sample.split(Record_DF, SplitRatio = 0.8)
train_data <- subset(Record_DF, sample_data == TRUE)
test_data <- subset(Record_DF, sample_data == FALSE)

# removing labels and storing them seperately for both training and testing data
TrainKnownLabels <- train_data$new_price
train_data <- train_data[ , -which(names(train_data) %in% c("new_price"))]
TestKnownLabels <- test_data$new_price
test_data <- test_data[ , -which(names(test_data) %in% c("new_price"))]

model_NB <- naiveBayes(train_data, TrainKnownLabels, laplace = 1)
predictions <- predict(model_NB, test_data)

tab_NB <- table(predictions,TestKnownLabels)
tab_NB

accuracy_Test <- sum(diag(tab_NB)) / sum(tab_NB)
accuracy_Test * 100

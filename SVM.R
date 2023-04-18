library(datasets)
library(caTools)
#install.packages("party")
library(party)
library(dplyr)
library(magrittr)
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(e1071)

setwd("C:/Users/megha/Downloads")

Record_DF <- read.csv('SVM.csv', stringsAsFactors=TRUE)
Record_DF <- Record_DF[,-c(1,2,4,5,6,7,8,10)]
str(Record_DF)

set.seed(100)
Record_DF_Around_50k <- sample_n(Record_DF[Record_DF$new_price == 'Around_50k',], 100)
Record_DF_More_than_60k <- sample_n(Record_DF[Record_DF$new_price == 'More_than_60k',], 100)
Record_DF_Less_than_40k <- sample_n(Record_DF[Record_DF$new_price == 'Less_than_40k',], 100)

Final_df <- rbind(Record_DF_Around_50k, Record_DF_Less_than_40k, Record_DF_More_than_60k) # combining data frames

head(Final_df)
str(Final_df)
table(Final_df$new_price)

sample_data = sample.split(Final_df, SplitRatio = 0.8)
train_data <- subset(Final_df, sample_data == TRUE)
test_data <- subset(Final_df, sample_data == FALSE)

# removing labels from test data and storing then in another variable 'TestKnownLabels'
TestKnownLabels <- test_data$new_price
test_data <- test_data[ , -which(names(test_data) %in% c("new_price"))]
head(test_data)

svm_fit <- svm(new_price ~ ., data = train_data, kernel = 'linear', cost = 85)
pred <- predict(svm_fit, test_data, type = 'new_price')
plot(svm_fit, data = train_data, screen_size ~ normalized_used_price )

table(pred, TestKnownLabels)

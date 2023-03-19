library(datasets)
library(caTools)
#install.packages("party")
library(party)
library(dplyr)
library(magrittr)
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

setwd("~/Exam1Things")

Final_df <- read.csv('Decision_tree.csv', stringsAsFactors=TRUE) # all non-numeric columns should be factors
Final_df <- Final_df[,-c(1)]


head(Final_df)
str(Final_df)

sample_data = sample.split(Final_df, SplitRatio = 0.8)
train_data <- subset(Final_df, sample_data == TRUE)
test_data <- subset(Final_df, sample_data == FALSE)

train_data
test_data

# removing labels from test data and storing then in another variable 'TestKnownLabels'
TestKnownLabels <- test_data$new_price
test_data <- test_data[ , -which(names(test_data) %in% c("release_year_binned"))]
head(test_data)

# checking the balance of categories in target labels in training data and testing data
table(train_data$new_price)
table(TestKnownLabels)

fit <- rpart(release_year_binned ~ days_used + internal_memory_binned + screen_size + X4g , data = train_data,maxdepth = 3, method = 'class') # all columns and all rows
#fit <- rpart(new_price ~., data = train_data, method = 'class', subset = sample(1:nrow(train_data), 20)) # all cols, 20 rows
#fit <- rpart(new_price ~ rain + UV, data = train_data, method = 'class') # only rain and UV columns

# In rpart function above -> default split is 'Gini', If we want to use information gain then
#fit <- rpart(new_price ~., data = train_data, method = 'class', parms = list(split = 'entropy'))
#fit <- rpart(new_price ~., data = train_data, method = 'class', parms = list(split = 'information'))

rpart.plot(fit, extra = 101) # extra...?rpart.plot

predict_unseen <-predict(fit, test_data, type = 'class')

table_mat <- table(TestKnownLabels, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test * 100

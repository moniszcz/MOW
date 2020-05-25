library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)

source("loadData.R")
#check for missing values
sapply(df,function(x)any(is.na(x)))
#Since we are going to be randomly sampling things,
#ets set the seed for the random number generator
#so that we can reproduce results
set.seed(42)
limit.rows <- 250000
df <- df[sample(nrow(df), limit.rows),]
table(df$Cover_Type)
#imbalanced classes

#I’ll use a standard 80/20 split, train the model, 
#then evaluate it and print the confusion matrix
train.fraction <- 0.8

train.ind <- sample(nrow(df), round(train.fraction*nrow(df)))
train <- df[train.ind,]
test <- df[-train.ind,]

rm(df)
#we want to predict Cover_Type using all of the other columns in dataset
model <- randomForest(factor(Cover_Type)~., data = train, ntree = 200, importance = TRUE)
predictions <- predict(model, test, type = "class")
confusionMatrix(factor(predictions), factor(test$Cover_Type))
importance <- varImp(model)
head(importance)

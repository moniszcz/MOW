library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)
# library(reprtree)

#build random forest
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   ntree - number of trees
#   importance - If True, the model will calculate the feature importance for further analysis. (default = False)
randomForestPcg <- function(train, test, targ, preds, ntree, importance){
  if(missing(preds)){
    model <- randomForest(train[[targ]]~., data = train, ntree = ntree, importance = importance)
  }else{
    model <- randomForest(train[[targ]]~., data = train[, preds], ntree = ntree, importance = importance)
  }
  
predictions <- predict(model, test, type = "class")
confMat <- table(factor(predictions), factor(test[[targ]]))
if(nrow(confMat) < ncol(confMat)){
  print(confMat)
  accuracy <- sum(diag(confMat))/sum(confMat)
  print(accuracy)
}else{
  confMat1 <- confusionMatrix(factor(predictions),factor(test[[targ]]))
  print(confMat1)
}
# reprtree:::plot.getTree(model)
importance <- varImp(model)
head(importance)
}
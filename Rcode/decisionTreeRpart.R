library(rpart)
library(rpart.plot)
library(caret)

#build decision tree
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   max_depth - set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
#               Values greater than 30 rpart will give nonsense results on 32-bit machines
rpartDT <- function(train, test, targ, preds, min_split, cp, max_depth){
if(max_depth > 30){
    max_depth <- 30
}
  
if(missing(preds)){
    model <- rpart(train[[targ]]~., data = train, method = "class", control = rpart.control(minsplit = min_split, cp = cp, maxdepth = max_depth))
}else{
    model <- rpart(train[[targ]]~., data = train[, preds], method = "class", control = rpart.control(minsplit = min_split, cp = cp, maxdepth = max_depth))
}

predictions <- predict(model, test, type = "class")
confMat <- table(factor(predictions), factor(test[[targ]]))
print(nrow(confMat))
if(nrow(confMat) < ncol(confMat)){
  print(confMat)
  accuracy <- sum(diag(confMat))/sum(confMat)
  print(accuracy)
}else{
  confMat1 <- confusionMatrix(factor(predictions),factor(test[[targ]]))
  print(confMat1)
}
if(missing(preds)){
  X11()
  rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE)
}
}

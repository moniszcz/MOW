library(cowplot)
library(randomForest)
library(caret)
source("helper.R")

#build random forest
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   ntree - number of trees
#   importance - If True, the model will calculate the feature importance for further analysis. (default = False)
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   cp - complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.

#return confusion matrix, TPR, FPR, FM table and means

randomForestPcg <- function(train, test, targ, preds, ntree, importance, min_split, cp){
  
  targFormula <- as.formula(paste0(targ, "~ .")) 
  
  #build randomForest
  if(missing(preds)){
    if(missing(min_split) && missing(cp)){
      model <- randomForest(targFormula, data = train, ntree = ntree, importance = importance)
    }else{
      model <- randomForest(targFormula, data = train, ntree = ntree, importance = importance, args=list( minsplit = min_split, cp= cp))
    }
  }else{
    if(missing(min_split) && missing(cp)){
      model <- randomForest(targFormula, data = train[, preds], ntree = ntree, importance = importance)
    }else{
      model <- randomForest(targFormula, data = train[, preds], ntree = ntree, importance = importance, args=list( minsplit = min_split, cp= cp))
    }
  }
  
  predictions <- predict(model, test, type = "class")
  
  #performance indicators
  confMat <- confusionMatrix(factor(predictions),factor(test[[targ]]))
 
  confMat01 <- confmat01(predictions, test[[targ]])
  
  tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
  tpfp <- round(tpfp, 3)
  
  means <- rowMeans(tpfp)
  means <- round(means, 3)

  ret_values <- list(confMat, tpfp, means)
  return(ret_values)
}
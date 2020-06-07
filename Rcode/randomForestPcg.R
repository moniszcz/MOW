library(cowplot)
library(randomForest)
library(caret)
source("helper.R")
# library(reprtree)

#build random forest
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   ntree - number of trees
#   importance - If True, the model will calculate the feature importance for further analysis. (default = False)
randomForestPcg <- function(train, test, targ, preds, ntree, importance, minsplit, cp){
  
  targFormula <- as.formula(paste0(targ, "~ .")) 
  if(missing(preds)){
    if(missing(minsplit) && missing(cp)){
      model <- randomForest(targFormula, data = train, ntree = ntree, importance = importance)
    }else{
      model <- randomForest(targFormula, data = train, ntree = ntree, importance = importance, args=list( minsplit = minsplit, cp= cp))
    }
  }else{
    if(missing(minsplit) && missing(cp)){
      model <- randomForest(targFormula, data = train[, preds], ntree = ntree, importance = importance)
    }else{
      model <- randomForest(targFormula, data = train[, preds], ntree = ntree, importance = importance, args=list( minsplit = minsplit, cp= cp))
    }
  }
  print(model)
  predictions <- predict(model, test, type = "class")
  confMat1 <- confusionMatrix(factor(predictions),factor(test[[targ]]))
  print(confMat1)
  confMat01 <- confmat01(predictions, test[[targ]])
  tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
  tpfp <- round(tpfp, 3)
  print(tpfp)
  means <- rowMeans(tpfp)
  means <- round(means, 3)
  print(means)
  
  importance <- varImp(model)
  varImpPlot(model)
  # print(head(importance))
}
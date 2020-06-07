library(rpart)
library(caret)
source("anova.R")

#build decision tree
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   min_bucket - he minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, 
#               the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate

rpartDT_split <- function(train, test, targ, preds, min_split, min_bucket, cp){
  
  alist <- list(init= anova_init, split=anova_split, eval=anova_eval)
  alist_test <- list(init= anova_init2, split=test_split, eval=anova_eval)
  
  targFormula <- as.formula(paste0(targ, "~ ."))
  
  if(missing(min_split)){
    t_control <- rpart.control(minbucket =  min_bucket, cp = cp)
  } else if (missing(min_bucket)){
    t_control <- rpart.control(minsplit = min_split, cp = cp)
  } else{
    t_control <- rpart.control(minsplit = min_split, minbucket =  min_bucket, cp = cp)
  }
  
  if(missing(preds)){
    model <- rpart(targFormula, data = train,  method = alist_test, control = t_control)
  }else{
    model <- rpart(targFormula, data = train[, preds], method = alist_test, control = t_control)
  }
  predictions <- round(predict(model, test))
  
  confMat1 <- confusionMatrix(factor(predictions),factor(test[[targ]]))
  print(confMat1)
  
  confMat01 <- confmat01(predictions, test[[targ]])
  
  tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
  tpfp <- round(tpfp, 3)
  print(tpfp)
  
  means <- rowMeans(tpfp)
  means <- round(means, 3)
  print(means)
  
  
}
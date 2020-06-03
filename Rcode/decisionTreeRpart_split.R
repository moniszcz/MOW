library(rpart)
library(rpart.plot)
library(caret)
source("logic_boost.R")
source("anova.R")

#build decision tree
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted

rpartDT_split <- function(train, test, targ, preds, min_split, cp){
  
  alist <- list(init= anova_init, split=anova_split, eval=anova_eval)
  alist_test <- list(init= anova_init2, split=anova_split, eval=anova_eval)
  
  targFormula <- as.formula(paste0(targ, "~ ."))
  if(missing(preds)){
    model <- rpart(targFormula, data = train,  method = alist_test, control = rpart.control(minsplit = min_split, cp = cp))
  }else{
    model <- rpart(targFormula, data = train[, preds], method = alist_test, control = rpart.control(minsplit = min_split, cp = cp))
  }
  predictions <- round(predict(model, test))
  
  confMat1 <- confusionMatrix(factor(predictions),factor(test[[targ]]))
  print(confMat1)
  conf <- table(factor(predictions),factor(test[[targ]]))
  print(conf)
  confMat01 <- confmat01(predictions, test[[targ]])
  tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
  tpfp <- round(tpfp, 3)
  print(tpfp)
  means <- rowMeans(tpfp)
  means <- round(means, 3)
  print(means)
  
  
}
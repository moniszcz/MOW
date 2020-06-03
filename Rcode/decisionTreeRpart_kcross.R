library(rpart)
library(rpart.plot)
library(caret)
source("helper.R")

#build decision tree
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   target - feature to predict
#   preds - features to build the tree
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   min_bucket - he minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, 
#               the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate

rpartDT_kcross <- function(k, data, targ, preds, min_split, min_bucket, cp){
  
  #confMat1 <- list()
  means_list <- list()
  
  targFormula <- as.formula(paste0(targ, "~ ."))
  
  if(missing(min_split)){
    t_control <- rpart.control(minbucket =  min_bucket, cp = cp)
  } else if (missing(min_bucket)){
    t_control <- rpart.control(minsplit = min_split, cp = cp)
  } else{
    t_control <- rpart.control(minsplit = min_split, minbucket =  min_bucket, cp = cp)
  }
  
  
  
  parts = split(data, sample(1:k, nrow(data), replace=T))

  for(i in 1:k) {
    test_data = parts[[i]]
    training_data = c();

    for(j in 1:k) {
      if(j != i) {
        training_data = rbind(training_data, parts[[j]])
      }
    }
    
    if(missing(preds)){
      model <- rpart(targFormula, data = training_data,  control = t_control)
    }else{
      model <- rpart(targFormula, data = training_data[, preds], control = t_control)
    }
    
    predictions <- predict(model, test_data, type = "class")
    
    confMat1 <- confusionMatrix(factor(predictions),factor(test_data[[targ]]))
    
    confMat01 <- confmat01(predictions, test_data[[targ]])
    
    tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
    tpfp <- round(tpfp, 3)
    #print(tpfp)
    
    means <- rowMeans(tpfp)
    means <- round(means, 3)
    #print(means)
    
    means_list <- rowMeans(tpfp)
    means_list <- round(means_list, 3)
  }
  

  #means_list <- round(means_list, 3)
  
  
  print(means_list)
  
  
  
}

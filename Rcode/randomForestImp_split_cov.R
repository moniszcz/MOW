library(rpart)
library(caret)
library(dplyr)
source("helper.R")
source("smoothed_anova.R")

#create random forest 
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   targ - feature to predict
#   predictors - features to build the tree
#   perc_predictors - specifies part of features to be used for building tree
#   ntree - number of trees
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   min_bucket - he minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, 
#               the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate
#   complex_param - complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.

#return confusion matrix, TPR, FPR, FM table and means

randomForestImp_test <- function(train, test, targ, predictors, perc_predictors, ntree, min_split, min_bucket, complex_param) {
  
  set.seed(42)
  # create an empty list for trees
  randForest <- list()
  predictions <- c()
  
  alist <- list(init= anova_init, split=anova_split_cov, eval=anova_eval)
  
  # build trees
  for(i in 1:ntree) {
    # choose perc_predictors sample of the predictors without replacing
    tree_predictors <- sample(predictors, length(predictors) * perc_predictors, replace = FALSE)
    # create a bootstrap dataset with possible replacement
    inbagDS <- train[sample(nrow(train), replace = TRUE),]
    
    inbagPreds <- inbagDS[, tree_predictors]
    
    if(!(targ %in% colnames(inbagPreds)))
    {
      inbagPreds <- bind_cols(inbagPreds, inbagDS[targ])
    }
    
    # set the rpart.control object
    if(missing(min_split)){
      t_control <- rpart.control(minbucket =  min_bucket, cp = complex_param)
    } else if (missing(min_bucket)){
      t_control <- rpart.control(minsplit = min_split, cp = complex_param)
    } else{
      t_control <- rpart.control(minsplit = min_split, minbucket =  min_bucket, cp = complex_param)
    }
    # define formula
    targFormula <- as.formula(paste0(targ, "~ ."))
    # build a tree
    tree <- rpart(targFormula, inbagPreds, method = alist, control = t_control) 
    # add our tree to the forest
    randForest[[i]] <- tree
    
    pred <- predict(tree, test)#, type = "class")
    predictions <- cbind(predictions, pred)
  }
  
  predicted_value <- apply(predictions, 1, function(x) names(which.max(table(x))))
  
  #performance indicators
  confMat <- confusionMatrix(factor(predicted_value), factor(test[[targ]]))
  
  confMat01 <- confmat01(predicted_value, test[[targ]])
  
  tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
  tpfp <- round(tpfp, 3)
  
  means <- rowMeans(tpfp)
  means <- round(means, 3)
  
  ret_values <- list(confMat, tpfp, means)
  return(ret_values)
}
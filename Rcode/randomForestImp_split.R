library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
source("logic_boost.R")
source("anova.R")


#create random forest 
#arguments:
#   train - dataset for training
#   test - dataset for testing
#   targ - feature to predict
#   predictors - features to build the tree
#   perc_predictors - specifies part of features to be used for building tree
#   perc_samples - specifies part of samples to be used for building tree
#   ntree - number of trees
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   min_bucket - he minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, 
#               the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate

randomForestImp_split <- function(train, test, targ, predictors, perc_predictors, perc_samples, ntree, min_split, min_bucket, complex_param) {
  
  # create an empty list for trees
  randForest <- list()
  predictions <- c()
  
  alist <- list(init= anova_init, split=anova_split, eval=anova_eval)
  alist_test <- list(init= anova_init2, split=anova_split, eval=anova_eval)
  
  # build trees
  for(i in 1:ntree) {
    # choose perc_predictors sample of the predictors without replacing
    tree_predictors <- sample(predictors, length(predictors) * perc_predictors, replace = FALSE)
    # take a sample of the observations using normal distribution
    in_bag <- apply(train, 1, function(v) ifelse(runif(n = 1, min = 0, max = 1) <= perc_samples, 1, 0))
    #make new dataset
    forestDS <- cbind(train, in_bag)
    #observations used for building tree
    inbagDS <- forestDS[which(forestDS$in_bag == 1), ]
    inbagPreds <- forestDS[which(forestDS$in_bag == 1), tree_predictors]
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
    targFormula <- as.formula(paste0(targ, "~ ."))
    # build a tree
    tree <- rpart(targFormula, inbagPreds, method = alist_test, control = t_control) 
    # add our tree to the forest
    randForest[[i]] <- tree
    
    pred <- predict(tree, test)#, type = "class")
    predictions <- cbind(predictions, pred)
  }
  predicted_value <- apply(predictions, 1, function(x) names(which.max(table(x))))
  data_to_return <- cbind(test, predicted_value)
  
  confMat1 <- confusionMatrix(factor(predicted_value), factor(test[[targ]]))
  print(confMat1)
  confMat01 <- confmat01(predicted_value, test[[targ]])
  tpfp <- sapply(confMat01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm)))
  tpfp <- round(tpfp, 3)
  print(tpfp)
  means <- rowMeans(tpfp)
  means <- round(means, 3)
  print(means)
  
}

#creates random forest 
#arguments:
#   train_dataset - dataset for training
#   test_dataset - dataset for testing
#   target - feature to predict
#   predictors - features to be used for building tree
#   perc_predictors - specifies part of features to be used for building tree
#   perc_samples - specifies part of samples to be used for building tree
#   ntrees - number of trees to grow. This should not be set to too small a number
#   min_split - the minimum number of observations that must exist in a node in order for a split to be attempted
#   min_bucket - he minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, 
#               the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate
#   max_depth - set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
#               Values greater than 30 rpart will give nonsense results on 32-bit machines
#   isClassification - if target is a class
#   factorLst - list of target values for classification
buildRandomForest <- function(train_dataset, test_dataset, target, predictors, perc_predictors, perc_samples, ntrees, min_split, min_bucket, complex_param, max_depth) {
  
  # create an empty list for trees
  randForest <- list()
  if(max_depth > 30){
    max_depth <- 30
  }
  predictionsRF <- c()
  # build trees
  for(i in 1:ntrees) {
    # choose perc_predictors sample of the predictors without replacing
    tree_predictors <- sample(predictors, length(predictors) * perc_predictors, replace = FALSE)
    # take a sample of the observations using normal distribution
    in_bag <- apply(train_dataset, 1, function(v) ifelse(runif(n = 1, min = 0, max = 1) <= perc_samples, 1, 0))
    #make new dataset
    forestDs <- cbind(train_dataset, in_bag)
    #observations used for building tree
    in_bag <- which(forestDs$in_bag == 1)
    # set the rpart.control object
    if(missing(min_split)){
      t_control <- rpart.control(minbucket =  min_bucket, cp = complex_param, maxdepth = max_depth)
    } else if (missing(min_bucket)){
      t_control <- rpart.control(minsplit = min_split, cp = complex_param, maxdepth = max_depth)
    } else{
      t_control <- rpart.control(minsplit = min_split, minbucket =  min_bucket, cp = complex_param, maxdepth = max_depth)
    }
    # build a tree
    tree <- rpart(forestDs[in_bag,target] ~ ., forestDs[in_bag,tree_predictors], control = t_control) 
    # add our tree to the forest
    randForest[[i]] <- tree

    prediction <- predict(tree, test_dataset, type = "class")
    predictionsRF <- cbind(predictionsRF, prediction)
  }
  predicted_value <- apply(predictionsRF, 1, function(x) names(which.max(table(x))))
  print(predicted_value)
  data_to_return <- cbind(test_dataset, predicted_value)

  #  
  return(predicted_value)
  
}

#Validate predicted value

# validateRandomForest <- function(data, target){
#   cnt_correct = 0
#   cnt_incorrect = 0
#   for(i in nrow(data)){
#     if(data[i, target] == data[i, ncol(data)]){
#       cnt_correct = cnt_correct + 1
#     }
#     else{
#       cnt_incorrect = cnt_incorrect + 1
#     }
#   }
#   list(cnt_correct, cnt_incorrect)
# }

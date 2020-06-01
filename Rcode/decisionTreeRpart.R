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

rpartDT <- function(train, test, targ, preds, min_split, cp){
  
targFormula <- as.formula(paste0(targ, "~ ."))
if(missing(preds)){
    model <- rpart(targFormula, data = train,  control = rpart.control(minsplit = min_split, cp = cp))
}else{
    model <- rpart(targFormula, data = train[, preds], control = rpart.control(minsplit = min_split, cp = cp))
}

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

# }
# if(missing(preds)){

 # rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# }

}

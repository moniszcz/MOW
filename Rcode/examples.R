source("loadDataCovType.R")
source("helper.R")

#Cover Type Dataset
#check for missing values
#sapply(df,function(x)any(is.na(x)))
#Since we are going to be randomly sampling things,
#set the seed for the random number generator
#so that we can reproduce results
set.seed(12)
#half as a training set, quater as validation set and remaining part as test set
limit.rows <- 250000
df <- df[sample(nrow(df), limit.rows),]

#I’ll use a standard 80/20 split, train the model,
#then evaluate it and print the confusion matrix
train.fraction <- 0.8
train.ind <- sample(nrow(df), round(train.fraction*nrow(df)))
train <- df[train.ind,]
test <- df[-train.ind,]

table(train$Cover_Type)
table(train$Cover_Type)/nrow(train)

targ <- "Cover_Type"
preds <- c("Elevation", "Aspect","Slope", "Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
           "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points", paste("Wilderness_Area", 1:4, sep = ""), paste("Soil_Type", 1:40, sep = ""), "Cover_Type")
rm(df)

# rpart package
source("decisionTreeRpart.R")
covTypeTree <- rpartDT(train = train, test = test, targ = targ, min_split = 2, cp = 0)


source("decisionTreeRpart_split.R")
covTypeTree_split <- rpartDT_split(train = train, test = test, targ = targ, preds = preds, min_split = 2, cp = 0)


# randomForest package
source("randomForestPcg.R")
CovTypeRF <- randomForestPcg(train = train, test = test, targ = targ, ntree = 50, importance = TRUE, min_split = 2, cp = 0)


# randomForest implementation
source("randomForestImp.R")
CovTypeImp <- randomForestImp(train = train, test = test, targ = targ, predictors = preds, perc_predictors = 0.8, ntree = 5, min_split = 2, complex_param = 0.0)


#randomForest implementation with user defined split function
source("randomForestImp_split.R")
CovTypeImp_split <- randomForestImp_split(train = train, test = test, targ = targ, predictors = preds, perc_predictors = 0.8, ntree = 5, min_split = 2, complex_param = 0.0)



source("loadDataCancer.R")
#Breast Cancer Dataset
#check for missing values
sapply(ds,function(x)any(is.na(x)))
#Since we are going to be randomly sampling things,
#set the seed for the random number generator
#so that we can reproduce results
set.seed(12)

#I’ll use a standard 80/20 split, train the model, 
#then evaluate it and print the confusion matrix
train.fraction_BC <- 0.8
train.ind_BC <- sample(nrow(ds), round(train.fraction_BC*nrow(ds)))
train_BC <- ds[train.ind_BC,]
test_BC <- ds[-train.ind_BC,]

table(train_BC$diagnosis)
table(train_BC$diagnosis)/nrow(train_BC)

targ_BC <- "diagnosis"
preds_BC <- c("diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean",
           "concave_points_mean", "symmetry_mean", "fractal_dimension_mean", "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se",
           "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se", "radius_worst", "texture_worst", "perimeter_worst",
           "area_worst", "smoothness_worst", "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", "fractal_dimension_worst")
rm(ds)

# # rpart package
# source("decisionTreeRpart.R")
# breastCancTree <- rpartDT(train = train_BC, test = test_BC, preds = preds_BC, targ = targ_BC, min_split = 2, cp = 0.0)
# 
source("decisionTreeRpart_split.R")
breastCancTree_split <- rpartDT_split(train = train_BC, test = test_BC, preds = preds_BC, targ = targ_BC, min_split = 2, cp = 0.0)

 
# randomForest package
source("randomForestPcg.R")
breastCancRF <- randomForestPcg(train = train_BC, test = test_BC, targ = targ_BC, ntree = 10, importance = TRUE, min_split = 2, cp = 0)


# randomForest implementation
source("randomForestImp.R")
breastCancImp <- randomForestImp(train = train_BC, test = test_BC, targ = targ_BC, predictors = preds_BC, perc_predictors = 0.4, ntree = 10, min_split = 5, complex_param = 0.001)


#randomForest implementation with user defined split function
source("randomForestImp_split.R")
breastCancImp_split <- randomForestImp_split(train = train_BC, test = test_BC, targ = targ_BC, predictors = preds_BC, perc_predictors = 0.8, ntree = 10, min_split = 5, complex_param = 0.001)

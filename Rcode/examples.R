source("loadDataCovType.R")

#Cover Type Dataset
#check for missing values
sapply(df,function(x)any(is.na(x)))
#Since we are going to be randomly sampling things,
#set the seed for the random number generator
#so that we can reproduce results
set.seed(42)
limit.rows <- 250000
df <- df[sample(nrow(df), limit.rows),]

#I’ll use a standard 80/20 split, train the model, 
#then evaluate it and print the confusion matrix
train.fraction <- 0.8
train.ind <- sample(nrow(df), round(train.fraction*nrow(df)))
train <- df[train.ind,]
test <- df[-train.ind,]

targ <- "Cover_Type"
preds <- c("Elevation", "Aspect","Slope", "Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
           "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4",
           "Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8","Soil_Type9","Soil_Type10","Soil_Type11", 
           "Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16", "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21", 
           "Soil_Type22","Soil_Type23","Soil_Type24","Soil_Type25","Soil_Type26", "Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31", 
           "Soil_Type32","Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36", "Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")
rm(df)

# rpart package
source("decisionTreeRpart.R")
covTypeTree <- rpartDT(train = train, test = test, targ = targ, preds = preds, min_split = 5, cp = 0.001, max_depth = 25)


# randomForest package
source("randomForestPcg.R")
CovTypeRF <- randomForestPcg(train = train, test = test, targ = targ, preds = preds, ntree = 10, importance = TRUE)


# randomForest implementation
source("randomForestImp.R")
CovTypeImp <- randomForestImp(train = train, test = test, targ = targ, predictors = preds, perc_predictors = 0.8, perc_samples = 0.8, ntree = 10, min_split = 5, complex_param = 0.001, max_depth = 25)


source("loadDataCancer.R")
#Breast Cancer Dataset
#check for missing values
sapply(ds,function(x)any(is.na(x)))
#Since we are going to be randomly sampling things,
#set the seed for the random number generator
#so that we can reproduce results
set.seed(42)

#I’ll use a standard 80/20 split, train the model, 
#then evaluate it and print the confusion matrix
train.fraction_BC <- 0.8
train.ind_BC <- sample(nrow(ds), round(train.fraction_BC*nrow(ds)))
train_BC <- ds[train.ind_BC,]
test_BC <- ds[-train.ind_BC,]

targ_BC <- "diagnosis"
preds_BC <- c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean",
           "concave_points_mean", "symmetry_mean", "fractal_dimension_mean", "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se",
           "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se", "radius_worst", "texture_worst", "perimeter_worst",
           "area_worst", "smoothness_worst", "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", "fractal_dimension_worst")
rm(ds)

# rpart package
source("decisionTreeRpart.R")
covTypeTree <- rpartDT(train = train_BC, test = test_BC, targ = targ_BC, preds = preds_BC, min_split = 5, cp = 0.001, max_depth = 25)


# randomForest package
source("randomForestPcg.R")
CovTypeRF <- randomForestPcg(train = train_BC, test = test_BC, targ = targ_BC, preds = preds_BC, ntree = 10, importance = TRUE)


# randomForest implementation
source("randomForestImp.R")
CovTypeImp <- randomForestImp(train = train_BC, test = test_BC, targ = targ_BC, predictors = preds_BC, perc_predictors = 0.8, perc_samples = 0.8, ntree = 10, min_split = 5, complex_param = 0.001, max_depth = 25)

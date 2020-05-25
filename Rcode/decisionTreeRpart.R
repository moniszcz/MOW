library(rpart)
library(rpart.plot)
library(caret)
source("loadDataCOvType.R")
set.seed(42)
limit.rows <- 250000
df <- df[sample(nrow(df), limit.rows),]
df$Cover_Type <- as.factor(df$Cover_Type)

train.fraction <- 0.8
train.ind <- sample(nrow(df), round(train.fraction*nrow(df)))
train <- df[train.ind,]
test <- df[-train.ind,]

targ <- "Cover_Type"
preds <- c("Elevation", 
           "Aspect",
           "Slope",
           "Horizontal_Distance_To_Hydrology",
           "Vertical_Distance_To_Hydrology",
           "Horizontal_Distance_To_Roadways",
           "Hillshade_9am",
           "Hillshade_Noon",
           "Hillshade_3pm",
           "Horizontal_Distance_To_Fire_Points",
           "Wilderness_Area1",
           "Wilderness_Area2",
           "Wilderness_Area3",
           "Wilderness_Area4",
           "Soil_Type1",                 
           "Soil_Type2",
           "Soil_Type3",
           "Soil_Type4",
           "Soil_Type5",
           "Soil_Type6",                        
           "Soil_Type7",
           "Soil_Type8",
           "Soil_Type9",
           "Soil_Type10",
           "Soil_Type11",                       
           "Soil_Type12",
           "Soil_Type13",
           "Soil_Type14",
           "Soil_Type15",
           "Soil_Type16",                       
           "Soil_Type17",
           "Soil_Type18",
           "Soil_Type19",
           "Soil_Type20",
           "Soil_Type21",                       
           "Soil_Type22",
           "Soil_Type23",
           "Soil_Type24",
           "Soil_Type25",
           "Soil_Type26",                       
           "Soil_Type27",
           "Soil_Type28",
           "Soil_Type29",
           "Soil_Type30",
           "Soil_Type31",                       
           "Soil_Type32",
           "Soil_Type33",
           "Soil_Type34",
           "Soil_Type35",
           "Soil_Type36",                       
           "Soil_Type37",
           "Soil_Type38",
           "Soil_Type39",
           "Soil_Type40")
rm(df)
str(train)
#model <- rpart(factor(Cover_Type)~., data = train[, preds], control = rpart.control(cp = 0.0001))
model <- rpart(train[,targ]~., data = train[, preds], control = rpart.control(cp = 0.0001))
predictions <- predict(model, test, type = "class")
print(predictions)
confusionMatrix(factor(predictions), factor(test$Cover_Type))

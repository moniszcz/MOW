con <- gzcon(url(paste0("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/",
                        "covtype.data.gz", sep="")))
txt <- readLines(con)
df <- read.csv(textConnection(txt))
colnames(df) <- c("Elevation", "Aspect","Slope", "Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                  "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points",
                  paste("Wilderness_Area", 1:4, sep = ""),
                  paste("Soil_Type", 1:40, sep = ""), "Cover_Type")

cs.input.attrs <- setdiff(names(df), "Cover_Type")
df$Cover_Type <- as.factor(df$Cover_Type)

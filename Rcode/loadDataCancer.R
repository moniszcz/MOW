df <-  read.table(url(paste0("https://archive.ics.uci.edu/ml/machine-learning-databases/",
                                  "breast-cancer-wisconsin/wdbc.data")),header=FALSE,sep=",")
head(df)
df$V2 <- df$V2 == 'M'

colnames(df) <- c("id", "diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean",
                  "concave points_mean", "symmetry_mean", "fractal_dimension_mean", "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se",
                  "compactness_se", "concavity_se", "concave points_se", "symmetry_se", "fractal_dimension_se", "radius_worst", "texture_worst", "perimeter_worst",
                  "area_worst", "smoothness_worst", "compactness_worst", "concavity_worst", "concave points_worst", "symmetry_worst", "fractal_dimension_worst")
df$diagnosis <- as.factor(df$diagnosis)

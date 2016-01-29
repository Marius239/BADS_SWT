setwd("C:/Users/User/Desktop/RandomForest")

ranking <- read.csv2("RF_Selection.csv", stringsAsFactors = FALSE)


idx <- 20:40
plot(idx, ranking[idx,2])
abline(v = 31)
abline(v = 36)
abline(v = 39)

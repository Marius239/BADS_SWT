rm(list = ls())

# install.packages("party")
# install.packages("tree")
# install.packages("verification")
# install.packages("dplyr")
# install.packages("AUC")
# install.packages("party")
# install.packages("gbm")
# install.packages("dplyr")
# install.packages("caret")
# install.packages("verification")
# install.packages("ROCR")
# install.packages("randomForest")

library(foreign)
library(randomForest)
library(tree)
library("AUC")
library("party")
library("gbm")
library("dplyr")
library("caret")
library("verification")
library("ROCR")
library(doParallel)

setwd("C:/Users/luxmariu.hub/Desktop/RandomForest")

load("DataPredictTest.RData")
load("DataPredictTrain.RData")

train.data <- DataPredictTrain
ranking <- read.csv2("RF_Selection.csv", stringsAsFactors = FALSE)

vars_train <- c("churn", ranking$Variable[1:31])

sum(vars_train %in% names(train.data))  #Check if all variables are in trainData

train.data <- train.data[,which(names(train.data) %in% vars_train)]

vars_test <- c(ranking$Variable[1:31])
test.data <- DataPredictTest[,which(names(DataPredictTest) %in% vars_test)]

#------------------------------------------------------------------------------
#Model

trees <- 3000

churn_num_train <- as.numeric(train.data$churn) - 1

fit = gbm.fit(x = train.data[,which(names(train.data) != "churn")], 
              y = churn_num_train, 
              n.trees = trees, verbose=FALSE, shrinkage=0.005, 
              interaction.depth=20, n.minobsinnode=5, distribution="bernoulli")   

prediction = predict(fit, newdata = test.data, n.trees = trees ,type="response")


FinalPredictions <- data.frame(Customer_ID = DataPredictTest$Customer_ID, 
                               EstimatedChurnProbability = prediction)

head(FinalPredictions)

write.csv(FinalPredictions, file = "Churn_Predictions.csv",
          quote = FALSE, row.names = FALSE)

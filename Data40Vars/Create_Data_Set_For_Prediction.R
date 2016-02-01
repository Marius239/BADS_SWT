rm(list = ls())

setwd("C:/Users/User/Documents/GitHub/BADS_SWT/Data40Vars")

load("TestData.RData")
load("TrainingData.RData")

head(trainingSetFinal$churn)

fullData <- rbind(trainingSetFinal, testSetFinal)


data <- fullData

# Create Data Set with all numeric Variables

numerics <- which(sapply(data, is.numeric)) 

#------------------------------------------------------------------------------
#Median Imputation

for(i in numerics){
  
  med <- median(data[,i], na.rm=TRUE)
  idx <- is.na(data[,i])
  data[idx,i] <- med 
  
}

DataPredict <- data[1:50000,]

DataPredict$churn <- factor(DataPredict$churn)
DataPredictTrain <- DataPredict

save(DataPredictTrain, file = "DataPredictTrain.RData")

DataPredictTest <- data[50001:100000,]

table(DataPredictTest$churn)
save(DataPredictTest, file = "DataPredictTest.RData")


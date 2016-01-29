rm(list = ls())

setwd("C:/Users/User/Desktop/RandomForest")

#----------------------------------------------------------------------------
# Load Data Set

load("TrainingData40Var.RData")
data <- trainingSetFinal

# Create Data Set with all numeric Variables

numerics <- which(sapply(data, is.numeric)) 

#------------------------------------------------------------------------------
#Median Imputation
 
# for(i in numerics){
#   
#   med <- median(data[,i], na.rm=TRUE)
#   idx <- is.na(data[,i])
#   data[idx,i] <- med 
#   
# }

#--------------------------------------------------------------------------------
#Mean Imputation

for(i in numerics){
  
  mu <- mean(data[,i], na.rm=TRUE)
  idx <- is.na(data[,i])
  data[idx,i] <- mu 
  
}


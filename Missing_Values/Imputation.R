rm(list = ls())

setwd("C:/Users/User/Desktop")

#----------------------------------------------------------------------------
# Load Data Set

load("dnew.RData") 
load("dnew2.RData")
load("dnew3.RData")

data <- 

# Create Data Set with all numeric Variables

numerics <- which(sapply(data, is.numeric)) 

#------------------------------------------------------------------------------
#Median Imputation
 
for(i in numerics){
  
  med <- median(data[,i], na.rm=TRUE)
  idx <- is.na(data[,i])
  data[idx,i] <- med 
  
}

#--------------------------------------------------------------------------------
#Mean Imputation

data <- dnew2

for(i in numerics){
  
  mu <- mean(data[,i], na.rm=TRUE)
  idx <- is.na(data[,i])
  data[idx,i] <- med 
  
}


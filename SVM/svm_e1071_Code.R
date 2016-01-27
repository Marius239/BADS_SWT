#######################################################################################
########## Project BADS - Support Vector Machines with "e1071" package ################
#######################################################################################

#################################################
# The "e1071" package supports factor variables #
#################################################

rm(list=ls())

setwd("C:/Users/Josephine/Documents/Studium/Master Statistik/Business Analytics and Data Science/Special Working Task")

#install.packages("e1071")
library(e1071)

#-------------------------------------------------------------------------------------
# Prepare Datasets

# Load Datasets
load("C:/Users/Josephine/Documents/BADS_SWT/Missing_Values/Data/trainData.RData")
load("C:/Users/Josephine/Documents/BADS_SWT/Missing_Values/Data/testData.RData")

ranking <- read.csv2("RF_Selection.csv", stringsAsFactors = FALSE)
vars <- c("churn", ranking$Variable[1:30])

sum(vars %in% names(trainData))  #Check if all variables are in trainData

#------------------------------------------------------------------------------
#

# Dataset with all observations, best 30 variables + churn + csa, NAs omitted
data_na <- na.omit(trainData[,which(names(trainData) %in% vars)])
data_test_na <- na.omit(testData[,which(names(trainData) %in% vars)])

# Make churn a factor
data_na$churn <- factor(data_na$churn, labels = c("Y", "N"))
data_test_na$churn <- factor(data_test_na$churn, labels = c("Y", "N"))

# Delete all factor variables
# factors <- names(data_na[, sapply(data_na, is.factor)])
# data_na <- data_na[1:200,-which(names(data_na) %in% factors[1:4])]
# data_test_na <- data_test_na[1:200,-which(names(data_test_na) %in% factors[1:4])]

#-------------------------------------------------------------------------------------------------
# Model 1: Use all variables for the model

start1 <- Sys.time()

# Create model
model1 <- svm(churn ~ ., data=data_na, probability=TRUE)
summary(model1)

# Create predictions
#test <- as.matrix(data_test2_csa)
prediction1 <- predict(model1, newdata=data_test_na, type="prob") 

end1 <- Sys.time()
end1-start1

#------------------------------------------------------------------------------------------------
# Model 1: Result

hit.rate1 <- sum(prediction1==data_test_na$churn)/length(data_test_na$churn)
hit.rate1

#-------------------------------------------------------------------------------------------------
# Model 2: Prepare Datasets without "csa"

# Prepare Dataset
vars_new <- c("churn", ranking$Variable[1:30])
data_na_csa <- na.omit(trainData[,which(names(trainData) %in% vars_new)])
data_test_na_csa <- na.omit(testData[,which(names(trainData) %in% vars_new)])

# Make churn a factor
data_na_csa$churn <- factor(data_na$churn, labels = c("Y", "N"))
data_test_na_csa$churn <- factor(data_test_na$churn, labels = c("Y", "N"))

#--------------------------------------------------------------------------------------------------
# Create Model 2

start2 <- Sys.time()

# Create model
model2 <- svm(churn ~ ., data=data_na_csa, probability=TRUE)
summary(model2)

# Create predictions
#test <- as.matrix(data_test2_csa)
prediction2 <- predict(model2, newdata=data_test_na_csa, type="prob") 

end2 <- Sys.time()
end2-start2

#--------------------------------------------------------------------------------------------------
# Model 2: Results

hit.rate2 <- sum(prediction2==data_test_na_csa$churn)/length(data_test_na_csa$churn)
hit.rate2
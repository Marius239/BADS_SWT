#######################################################################################
########## Project BADS - Support Vector Machines with "e1071" package ################
#######################################################################################

#################################################
# The "e1071" package supports factor variables #
#################################################

rm(list=ls())

setwd("C:/Users/luxmariu.hub/Desktop/BADS-Project")

#install.packages("e1071")
library(e1071)

#-------------------------------------------------------------------------------------
# Prepare Datasets


# Load Data
data <- read.csv2("TrainingDataSet.csv", sep=";")
data_test <- read.csv2("TestDataSet.csv", sep=";")

#factors <- names(data[, sapply(data, is.factor)])
data_na <- na.omit(data[,-which(names(data) %in% c("Customer_ID", "phones", "csa", "hnd_price"))])
data_na$churn <- factor(data_na$churn, labels = c("Y", "N"))

data_test_na <- na.omit(data_test[,-which(names(data) %in% c("Customer_ID"))])
data_test_na$churn <- factor(data_test_na$churn, labels = c("Y", "N"))



### CHECK: Haben alle Variablen den richtigen Typ?
#sapply(data_train, class)
#data_train$churn <- as.factor(data_train$churn)

# Only 15 most important variables - with csa
#data_train2_csa <- data_train[1:200,is.element(colnames(data_train),c("churn", "eqpdays", "months", "mou_Mean", "totmrc_Mean", "last_swap", "hnd_price", "adjrev", "change_mou", "mou_cvce_Mean", "avg3mou", "totrev", "mou_Range", "mou_opkv_Mean", "totcalls", "phones", "csa"))]
#data_test2_csa <- data_test[1:200,is.element(colnames(data_train),c("churn","eqpdays", "months", "mou_Mean", "totmrc_Mean", "last_swap", "hnd_price", "adjrev", "change_mou", "mou_cvce_Mean", "avg3mou", "totrev", "mou_Range", "mou_opkv_Mean", "totcalls", "phones", "csa"))]

# # Only 15 most important variables - without csa
# data_train2 <- data_train[,is.element(colnames(data_train),c("churn", "eqpdays", "months", "mou_Mean", "totmrc_Mean", "last_swap", "hnd_price", "adjrev", "change_mou", "mou_cvce_Mean", "avg3mou", "totrev", "mou_Range", "mou_opkv_Mean", "totcalls", "phones"))]
# data_test2 <- data_test[,is.element(colnames(data_train),c("churn", "eqpdays", "months", "mou_Mean", "totmrc_Mean", "last_swap", "hnd_price", "adjrev", "change_mou", "mou_cvce_Mean", "avg3mou", "totrev", "mou_Range", "mou_opkv_Mean", "totcalls", "phones"))]


#--------------------------------------------------------------------------------------
# Reduce number of observations for test purposes

# # With csa
# data_train2_csa500 <- data_train2_csa[1:500,]
# data_test2_csa500 <- data_test2_csa[1:500,]
# 
# # Without csa
# data_train2_500 <- data_train2[1:500,]
# data_test2_500 <- data_test2[1:500,]

#--------------------------------------------------------------------------------------
# Use all variables for the model

start <- Sys.time()

# Create model
model <- svm(churn ~ ., data=data_na, probability=TRUE)
#summary(model)

# Create predictions
#test <- as.matrix(data_test2_csa)
prediction <- predict(model, newdata=data_test_na, type="prob") 
# Fehlermeldung: Error: cannot allocate vector of size 1.3 Gb
#summary(model)

end <- Sys.time()

end-start
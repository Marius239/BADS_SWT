#######################################################################################
########## Project BADS - Support Vector Machines with "caret" package ################
#######################################################################################

rm(list=ls())

setwd("C:/Users/Josephine/Documents/Studium/Master Statistik/Business Analytics and Data Science/Special Working Task")

library(caret)
library(kernlab)
library(doParallel)

#-----------------------------------------------------------------------------------------------------------
### Prepare Datasets ###

start <- Sys.time()

# Load Datasets
load("C:/Users/Josephine/Documents/BADS_SWT/Missing_Values/Data/trainData.RData")
load("C:/Users/Josephine/Documents/BADS_SWT/Missing_Values/Data/testData.RData")

ranking <- read.csv2("RF_Selection.csv", stringsAsFactors = FALSE)
vars <- c("churn", "csa", ranking$Variable[1:30])

sum(vars %in% names(trainData))  #Check if all variables are in trainData

#------------------------------------------------------------------------------
#

data_na <- na.omit(trainData[,which(names(trainData) %in% vars)])
data_test_na <- na.omit(testData[,which(names(trainData) %in% vars)])
data_na$churn <- factor(data_na$churn, labels = c("Y", "N"))
data_test_na$churn <- factor(data_test_na$churn, labels = c("Y", "N"))

factors <- names(data_na[, sapply(data_na, is.factor)])
data_na <- data_na[1:200,-which(names(data_na) %in% factors[1:4])]
data_test_na <- data_test_na[1:200,-which(names(data_test_na) %in% factors[1:4])]

#------------------------------------------------------------------------------------------------
# Support Vector Machines with caret: Top 26 numeric variables, all observations, NAs omitted

set.seed(1492)
ctrl   <- trainControl(method = "cv", number = 5, classProbs = TRUE)

svm  <-    train(churn~., 
                 data = data_na,
                 method = "svmRadial",
                 tuneLength = 9,
                 preProc = c("center","scale"),
                 trControl = ctrl, 
                 trace = FALSE
)

prediction <- predict(svm, newdata=data_test_na, type = "prob")

#----------------------------------------------------------------------------------------------------
# First Result

svm.pred <- 1 - round(prediction$N)
svm.pred <- factor(svm.pred, labels = c("N","Y"))

hit.rate <- sum(svm.pred==data_test_na$churn)/length(data_test_na$churn) 

#------------------------------------------------------------------------------
# Model Tuning: Results Step 1

svm
max_acc <- max(svm$results[,3])
sigma <- svm$results[svm$results[,3]==max_acc, 1]
C <- svm$results[svm$results[,3]==max_acc, 2]

#-----------------------------------------------------------------------------
# Model Tuning: Refine Parameters

# Use expand.grid to specify the search space

cores <- 4

cl <- makeCluster(cores)
registerDoParallel(cl)

tune <- function(x){

  library(caret)
  library(kernlab)
  
  ctrl   <- trainControl(method = "cv", number = 2, classProbs = TRUE)
  
  sigma_new = seq(sigma-.05, sigma+.05, by=.01)
  set.seed(1492)
  grid <- expand.grid(C = seq(max(c(0,C-1)),C+1, by= .1), sigma = sigma_new[x])

  # Tuning
  svm.tune <- train(churn~., 
                  data = data_na,
                  method = "svmRadial",
                  preProc = c("center","scale"),
                  tuneGrid = grid,
                  trControl = ctrl, 
                  trace = FALSE
)

}

result <- foreach(i=1:cores) %dopar% tune(i)

stopCluster(cl)
registerDoSEQ()

#------------------------------------------------------------------------------------------------
#Save results

results_complete <- result[[1]]$results[,1:3]

for(i in 2:cores){
  
  results_complete <- rbind(results_complete, result[[i]]$results[,1:3])
  
}

#-----------------------------------------------------------------------------------------------------
#Find best model
acc_lo <- numeric(cores)
for(i in 1:cores) acc_lo[i] <- max(result[[i]]$results[,3])
model <- result[[which(acc_lo == max(acc_lo))]]
max_acc_new <- max(results_complete[,3])

#---------------------------------------------------------------------------------------------------
# Predict

new_prediction <- predict(model, newdata=data_test_na, type = "prob")

#-------------------------------------------------------------------------------------------------------------------------
# Ergebnis

new_svm.pred <- 1 - round(new_prediction$N)
new_svm.pred <- factor(new_svm.pred, labels = c("N","Y"))

new_hit.rate <- sum(new_svm.pred==data_test_na$churn)/length(data_test_na$churn) 

end <- Sys.time()

end-start

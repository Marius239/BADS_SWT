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
#------------------------Additionaal functions


#-------------------------------------------
setwd("C:/Users/luxmariu.hub/Desktop/RandomForest")

anzahl <- c(31)

for(i in 1:length(anzahl)){

nvar <- anzahl[i]
t    <- 3000

load("TrainingDataMean40Var.RData")
train.data <- trainingSetMean
ranking <- read.csv2("RF_Selection.csv", stringsAsFactors = FALSE)


vars <- c("churn", ranking$Variable[1:nvar])

sum(vars %in% names(train.data))  #Check if all variables are in trainData

data <- na.omit(train.data[,which(names(train.data) %in% vars)])

#------------------------------------------------------------------------------
#Data management

# data$phones <- as.numeric(data$phones)
# data$phones[data$phones>10] <- 10
# data$phones <- as.factor(data$phones)


#------------------------------------------------------------------------------
#Function that is used in parallelized code

rf <- function(x, trees){
  
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
  
  k         = 10
  n         = floor(nrow(data)/k)
  #err.vect  = rep(NA, k)
  #lift.vect = rep(NA, k)
  
  s1     = ((x - 1)*n+1)
  s2     = (x*n)
  subset = s1:s2
  
  cv.train = data[-subset,]
  cv.test  = data[subset,]
  
  fit        = randomForest(x = cv.train[,which(names(cv.train) != "churn")], 
                            y = cv.train$churn, 
                            ntree = trees) 
  
  prediction = predict(fit, newdata = cv.test[,which(names(cv.train) != "churn")], type="prob")[,2]
  
  
  churn_num  = as.numeric(cv.test$churn) - 1
  auc        = roc.area(churn_num, prediction)$A
  pred       = prediction(prediction,cv.test$churn )
  perf       = performance(pred, measure="lift", x.measure="rpp")
  lift       = cbind(perf@x.values[[1]], perf@y.values[[1]])
  lift_score = lift[which.min(abs(lift[,1] - 0.1)) ,2]
  
  result     = data.frame(Fold = x, AUC = auc, Lift = lift_score)
  return(result)
}




#------------------------------------------------------------------------------
#Run Model

#set.seed(20)

start <- Sys.time()
print(start)

cores <- 10

cl <- makeCluster(cores)
registerDoParallel(cl)

estimate <- foreach(i = 1:10) %dopar% rf(i, trees = t)


end <- Sys.time()
dur <- end - start
print(dur)

stopCluster(cl)
registerDoSEQ()

#------------------------------------------------------------------------------
#Arrange Results

n <- length(estimate)

result <- estimate[[1]]
for(i in 2:n) result <- rbind(result, estimate[[i]])

summary(result$Lift)
summary(result$AUC)

filename <- paste("Results/RF_", t, "Tree_", nvar,"Vars_Phone_Median_NoSeed.RData", sep = "")

save(result, file = filename)
}
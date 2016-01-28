rm(list = ls())
graphics.off()

library(caret)
library(nnet)
library(beepr)
library(doParallel)
library(ROCR)
library(AUC)


#load("C:/Users/User/Desktop/Parallel/Data/trainData.Rdata")
#load("C:/Users/User/Desktop/Server_Parallel_NN/Data/testData.Rdata")
load("C:/Users/User/Desktop/Server_Parallel_NN/Data/fullData.Rdata")
fullData <- dnew2
ranking <- read.csv2("C:/Users/User/Desktop/Parallel/Data/RF_Selection.csv", stringsAsFactors = FALSE)

vars <- c("churn", ranking$Variable[1:13])

sum(vars %in% names(fullData))  #Check if all variables are in trainData

#------------------------------------------------------------------------------
#

data_na <- na.omit(fullData[,which(names(fullData) %in% vars)])
data_na$churn <- factor(data_na$churn, labels = c("N", "Y"))

factors    <- names(data_na[, sapply(data_na, is.factor)])
data_scale <- data_na[,-which(names(data_na) %in% factors)]

maxs <- sapply(data_scale, max) 
mins <- sapply(data_scale, min)

scaled <- as.data.frame(scale(data_scale, center = mins, scale = maxs - mins))

data_na <- data.frame(scaled, data_na[factors])

#----------------------------------------------------------
#Test Data

set.seed(13)
# Division in two uses 70% for training and 30% for testint
ind <- createDataPartition(dnew2$churn, p = .7, list = FALSE)

trainData  <- data_na[ind,]
testData   <- data_na[-ind,]

#trainData  <- trainData[1:500,]

#------------------------------------------------------------------------------
#Set Parameters and Function for Parallelizing

tune <- function(grid, train, test){
  
  result <- list()
  
  library(caret)
  library(ROCR)
  library(AUC)
  
  for(i in 1:nrow(grid)){
    
    #------------------------------------------------------------------------------
    #Estimate Model
    
    nn.tune <- train(train$churn~., data = train, 
                     method = "nnet",
                     trControl = ctrl, 
                     tuneGrid = grid[i,],
                     trace = FALSE)
    
    #------------------------------------------------------------------------------
    #Predict
    
    yhat <- predict(nn.tune, newdata = test, type = "prob")[,2] 
    y    <- test$churn
    
    #AUC
    predictions_test <- yhat
    roc_stats_test   <- roc(predictions_test, test$churn)
    auc <- auc(roc_stats_test)
    
    #Lift Score
    pred <- prediction(predictions_test, test$churn)
    perf <- performance(pred, measure = "lift", x.measure = "rpp")
    lift       <- cbind(perf@x.values[[1]], perf@y.values[[1]])
    lift_score <- lift[which.min(abs(lift[,1] - 0.1)) ,2]
   
    
    output <- list(list(measures = (cbind(grid[i,], auc, lift_score)), model = nn.tune) )
    
    result <- c(result,output)
    
    
  }
  return(result)
}

#------------------------------------------------------------------------------
#Test

ctrl <- trainControl(method = "cv", number = 2, classProbs = TRUE)
grid <- expand.grid(size = 1:3, decay = seq(0.5,50, by = 0.5))
grid_split <- split(grid, rep(1:15, each = 20))

#estimate <- tune(grid = grid, train = trainData, test = testData)

#------------------------------------------------------------------------------
#Run Code Parallelized

start <- Sys.time()

cores <- 3

cl <- makeCluster(cores)
registerDoParallel(cl)

estimate <- foreach(i=1:cores) %dopar% tune(grid = grid_split[[i]],
                                          train = trainData, 
                                          test = testData) 
end <- Sys.time()
end - start

stopCluster(cl)
registerDoSEQ()


#Get  Results
n <- nrow(grid_split[[1]])
m <- length(estimate)

result <- matrix(0, nrow = 1, ncol = 4)
colnames(result) <- names(estimate[[1]][[1]]$measures)
  
for(j in 1:m){
  
  for(i in 1:n) result <- rbind(result, estimate[[j]][[i]]$measures)
}
  
result <- result[-1,]


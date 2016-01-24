############################################################
#### Data Partition into Training and test set #############
############################################################

##### Status - I wanted to 


# Start with dnew2 as data frame

#### Create division into two and into 5 different data sets 
set.seed(13)

library("caret")
# Division in two uses 70% for training and 30% for testint
ind <- createDataPartition(dnew2$churn, p = .7, list = FALSE)

trainData  <- dnew2[ind2,]
testData <- dnew2[-ind2,]
  


data5CV1 <- dnew2[ind5,]
  

ind <- sample(1:length(dnew2$churn), size = length(dnew2$churn), replace = FALSE)

data2Train <- dnew2[ind[1:floor(length(dnew2$churn)*.70)],]
data2Train2  <- dnew2[ind[1:floor(length(dnew2$churn)*.50)],]

data10Train_1 <- dnew2
  
  ceiling(length(dnew2$churn)/2)

test <- createDataPartition(dnew2, times = 2)  
  
   
### Division into two
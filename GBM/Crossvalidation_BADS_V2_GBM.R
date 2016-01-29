rm(list = ls())

library(foreign)
library(randomForest)
library(tree)
library(beepr)
library("AUC")
library("party")
library("gbm")
library("dplyr")
library("caret")
library("verification")
library("ROCR")
#------------------------Additionaal functions



#-------------------------------------------
setwd("C:/Users/Marco/Desktop/Uni_projects/WiSe201516/BADS")


load("TrainingData.RData")
load("TestData.RData")

train.data <- trainingSetFinal
test.data <- testSetFinal
#------------------------------------------Data management
#train.data$phones1<- as.numeric(train.data$phones)
#train.data$phones1[train.data$phones1>10]<- 10
#train.data$phones1<- as.factor(train.data$phones1)


 #test.data$phones1<- as.numeric(test.data$phones)
 #test.data$phones1[test.data$phones1>10]<- 10
 #test.data$phones1<- as.factor(test.data$phones1)

#Handset Price

#train.data$hnd_price1<- as.numeric(as.character(train.data$hnd_price))
#train.data$hnd_price1[train.data$hnd_price1<50]<- 1
#train.data$hnd_price1[train.data$hnd_price1>50 & train.data$hnd_price1<150 ]<- 2
#train.data$hnd_price1[train.data$hnd_price1>150 & train.data$hnd_price1<200]<- 3
#train.data$hnd_price1[train.data$hnd_price1>200]<- 4


#test.data$hnd_price1<- as.numeric(as.character(test.data$hnd_price))
#test.data$hnd_price1[test.data$hnd_price1<50]<- 1
#test.data$hnd_price1[test.data$hnd_price1>50 & test.data$hnd_price1<150 ]<- 2
#test.data$hnd_price1[test.data$hnd_price1>150 & test.data$hnd_price1<200]<- 3
#test.data$hnd_price1[test.data$hnd_price1>200]<- 4

#---------------------------------------------------------







#------------------------------------------



#Reduced dataset:
#test<- test.data[,-c(8, 17, 33)]

train<- train.data
test<-test.data
test=na.omit(test)



#Merge all data to see if it works better:
set.seed(20)

data<-train




# Cross validation GBM:
k=10
n=floor(nrow(data)/k)
err.vect=rep(NA, k)
lift.vect= rep(NA, k)
ntrees=2000

for(i in 1:k){
  s1=((i-1)*n+1)
  s2=(i*n)
  subset=s1:s2
  
  cv.train=data[-subset,]
  cv.test= data[subset,]
  
  churn_num_train <- as.numeric(cv.train$churn) - 1
  
  fit=gbm.fit(x=cv.train[, -c(1,30)], y=churn_num_train, 
              n.trees=ntrees, verbose=FALSE, shrinkage=0.005, 
              interaction.depth=20, n.minobsinnode=5, distribution="bernoulli")      #include your prediction model
  prediction=predict(fit, newdata=cv.test[,-c(1,30)], n.trees=ntrees, type="response")
  
  
  churn_num_test <- as.numeric(cv.test$churn) - 1
  err.vect[i]=roc.area(churn_num_test, prediction)$A
  pred<-prediction(prediction,cv.test$churn )
  perf<-performance(pred, measure="lift", x.measure="rpp" )
  lift       <- cbind(perf@x.values[[1]], perf@y.values[[1]])
  lift.vect[i] <- lift[which.min(abs(lift[,1] - 0.1)) ,2]
  
  
  
  print(paste("AUC for fold", i, ":", err.vect[i],lift.vect[i]))
}

print(paste("Average AUC", mean(err.vect),mean(lift.vect)))

######final forecast#########
churn_num <- as.numeric(data$churn) - 1
fit=gbm.fit(x=data[,-c(1,30)], y=churn_num, 
            n.trees=ntrees, verbose=FALSE, shrinkage=0.005, 
            interaction.depth=20, n.minobsinnode=5, distribution="bernoulli")      #include your prediction model
prediction_final=predict(fit, newdata=test[,-c(1,30)], n.trees=ntrees, type="response")
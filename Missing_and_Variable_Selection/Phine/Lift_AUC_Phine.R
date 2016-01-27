library(ROCR)
library(AUC)
library(e1071)

load("C:/Users/User/Desktop/Phine/MiMittag.RData")

#------------------------------------------------------------------------------
#Predict

yhat <- predict(model1, newdata = data_test_na[1:2000,], probability = TRUE) #Wie krieg ich hier Wahrscheinlichkeiten?
y    <- data_test_na[1:2000,]$churn

#AUC
predictions_test <- yhat
roc_stats_test   <- roc(predictions_test, data_test_na$churn)
auc <- auc(roc_stats_test)

#Lift Score
pred <- prediction(predictions_test, data_test_na$churn)
perf <- performance(pred, measure = "lift", x.measure = "rpp")
lift       <- cbind(perf@x.values[[1]], perf@y.values[[1]])
lift_score <- lift[which.min(abs(lift[,1] - 0.1)) ,2]


auc
lift_score

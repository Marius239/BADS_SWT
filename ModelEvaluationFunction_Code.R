########## FUNCTION TO EVALUATE MODELS #############

library("klaR")
library("gmodels")
library("AUC")

###



## Must be run before using the function
testDataForModelSelection <- subset(testData, select = c(Customer_ID, churn)) 
# testData is the dataframe made to use predictions.
# IF YOUR TEST DATA IS NOT CALLED "testData" just change the name in the function call




#### FUNCTION ###

ModelMetricsFunction <- function (predictionsData, decile, pi, ...) {
  ## PredictionsData: dataframe with first column Customer_ID and second column the predicted Probability calculated from the model
  ## decile - decile wished for the lift measure
  ## pi - probability used as cutoff value for confusion Matrix and classification Error
  
  # Merge predictions with actual churn or not churn
  colnames(predictionsData) <- c("Customer_ID", "predictedProbability") # to be able to subset and manipulate columns by name
  
  data <- merge(predictionsData, testDataForModelSelection,
                by.x = "Customer_ID",
                by.y = "Customer_ID")
  colnames(data) <- c("Customer_Id", "predictedProbability", "churn")
  ###### Accuracy Metrics #####
  ### 1. Summary Statistics ###
  summaryPredictedProba <- summary(data$predictedProbability)
  countNAsPredictedProba <- sum(is.na(data$predictedProbability))
  countActualChurners <- nrow(data[data$churn == 1,])
  proportionOfChurners <- countActualChurners/nrow(data)
  
  ## Confusion Matrix and Classification Error
  data$predictedClass <- factor(data$predictedProbability >= pi, 
                                labels = c("pred_not_churn", "pred_churn"))  ## 
  confMatrix <- table(data$churn, data$predictedClass)  ## Confusion Matrix
  classificationError <- 1-sum(diag(confMatrix))/sum(confMatrix)
  
  ## ROC
  rocObject <- roc(data$predictedProbability , data$churn)   
  rocCurve <- plot(roc(data$predictedProbability , data$churn)) 
  AUCRoc <- auc(roc(data$predictedProbability , data$churn))
  
  ## Calculate lift measure
  topQuantile <- quantile(data$predictedProbability, probs = .9, na.rm = TRUE)
  dataForLift <- na.omit(data) ## remove NAs to be able to make the calculations
  dataForLift <- dataForLift[dataForLift$predictedProbability >= topQuantile, ]
  countChurnersInTopQuantile <- sum(dataForLift$churn == 1)
  lift <- countChurnersInTopQuantile/(nrow(dataForLift)*.4956) # assumption pi equal to proportion of churners in training data
  
  # Generate a list with the output
  output <- list(
    summaryPredictedProb = summaryPredictedProba,
    countNAs = countNAsPredictedProba,
    dataForLift = dataForLift,
    confusionMatrix = confMatrix,
    classificationError = classificationError,
    rocObject = rocObject,
    rocCurve = rocCurve, 
    AUCRoc = AUCRoc,
    countChurnersInWholeDataset = countActualChurners,
    countChurnersInTopQuantile = countChurnersInTopQuantile,
    lift = lift,
    cutValueQuantileForLift = topQuantile
  )
  return(output)
}

# Test the Function ######


# Test the function with random Numbers####
dataRandom <- data.frame(testData$Customer_ID, runif(nrow(testData), min = 0, max = 1))
colnames(dataRandom) <- c("Customer_ID", "randomProbas")  

## HOW TO CALL THE FUNCTION ####
# First parameter is the name of the data frame with two columns: 1) Customer_id 2)Predicted Proba
# Second parameter is 1 - the decile we want for lift (.1) --> .9
# Third column is the prior proba of churning - I used here the proportion of churners in the
  # training data set

functionTest <- ModelMetricsFunction(dataRandom, .9,.4956)
check2<- functionTest$dataForLift
functionTest$lift
functionTest$countChurnersInTopQuantile
functionTest$countChurnersInWholeDataset
functionTest$confusionMatrix
functionTest$summaryPredictedProb
functionTest$classificationError
functionTest$countNAs
functionTest$rocCurve
functionTest$AUCRoc

plot(functionTest$rocObject, col = "red") 


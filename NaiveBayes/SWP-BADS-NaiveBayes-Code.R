
##########################################################
############# Naive Bayes Classifier #####################
##########################################################
# install.packages("gmodels")


# install.packages("AUC")

library("klaR")
library("gmodels")
library("AUC")

## Read in the data
#trainData2 <- read.csv2("~/Google Drive/BADS_SWT/TrainingDataSet.csv", sep=";") 
#testData2 <- read.csv2("~/Google Drive/BADS_SWT/TestDataSet.csv", sep=";") 
#data$churn <- factor(data$churn,
 #                    labels = c("not churn", "churn"))

#testData$churn <- factor(testData$churn,
  #                       labels = c("not churn", "churn"))

# trainData2$Customer_ID == trainData$Customer_ID

#levels(testData$hnd_price) <- levels(data$hnd_price)


## Create data frame just with the variables that go into the model





# testData <- subset(testData, select = -c(Customer_ID, csa))

trainData2 <- na.omit(trainDataNew)

#dRed$churn <- factor(dRed$churn, labels = c("not churn", "churn"))


variablesInTheModel <- c("eqpdays", "months", "mou_Mean", "totmrc_Mean", "last_swap", "hnd_price", "adjrev",
                           "change_mou", "mou_cvce_Mean", "avg3mou", "totrev", "mou_Range", "mou_opkv_Mean", "totcalls" ,
                           "phones", "avg3qty", "complete_Mean", "peak_vce_Mean", "comp_vce_Mean", "totmou", "opk_vce_Mean",
                           "mou_peav_Mean", "adjqty", "avgqty", "adjmou", "rev_Mean", "ovrmou_Range", "rev_Range", "ovrmou_Mean",
                           "plcd_vce_Mean")

## Naive Bayes Model
nbLong <- NaiveBayes(churn ~ eqpdays + months + mou_Mean + totmrc_Mean + last_swap + hnd_price + adjrev +
                 change_mou + mou_cvce_Mean + avg3mou + totrev + mou_Range + mou_opkv_Mean + totcalls +
                 phones + avg3qty + complete_Mean + peak_vce_Mean + comp_vce_Mean + totmou + opk_vce_Mean +
                 mou_peav_Mean + adjqty + avgqty + adjmou + rev_Mean + ovrmou_Range + rev_Range + ovrmou_Mean +
                 plcd_vce_Mean, data = trainData2)

nb <- NaiveBayes(churn ~ eqpdays + months + mou_Mean + totmrc_Mean + last_swap + hnd_price + adjrev +
                   change_mou + mou_cvce_Mean + avg3mou + totrev + mou_Range + mou_opkv_Mean + totcalls +
                   phones, data = trainData2)





## Make the predictions on the test data


nbResults <- predict(nbLong, newdata = testDataNew)
nbPostProb <- data.frame(nbResults$posterior)


nbPredictionsFinal <- predict(nb, newdata = testSetFinal)


predictionsNB <- data.frame(testData$Customer_ID, nbPostProb$X1)
colnames(predictionsNB) <- c("Customer_ID", "predictedProbabilityChurn")




### Build the accuracy parameter calculations - Idea - everyone with their models imput a data frame just with the following
# info : customer number, predicted probability of churning and the actual churn or not churn. 

###FUNCTION TO ASSESS ACCURACY, ERROR, AREA UNDER ROC CURVE, LIFT OF MODELS #####

## Arguments of the function
# predictionsData : Data frame with 2 columns: Customer ID and estimated probabity of churn
# decile : which decile of the whole data is used to calculate the lift methode
# pi : probability that at random, a person churns


plot(roc(dummyPredictions, dataTest$churn))
plot(roc(dataRandom$randomProbas, testData$churn))


## Must be run before using the function
testDataForModelSelection <- subset(testData, select = c(Customer_ID, churn)) # testData is the model made to use predictions

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

plot(functionTest$rocObject)



functionNB <- ModelMetricsFunction(predictionsNB, .1,.5)

check <- functionNB$dataForLift
functionNB$lift
functionNB$countChurnersInTopQuantile
functionNB$countChurnersInWholeDataset
functionNB$confusionMatrix
functionNB$summaryPredictedProb
functionNB$classificationError
functionNB$countNAs
functionNB$AUCRoc

nbROCPlot <- plot(functionNB$rocObject)

plot(functionTest$rocObject, col = "red") 
plot(functionNB$rocObject, col = "blue")

quantile(predictionsNB$predictedProbabilityChurn, probs = c(0,.1,.5,.9,1), na.rm = TRUE)


### Plot all ROC Curves together






testOrder <- predictionsNB[with(predictionsNB, order(-predictedProbabilityChurn)),]
top10quantile <- quantile(predictionsNB$predictedProbabilityChurn, probs = .9, na.rm = TRUE)
testOrder2 <- na.omit(predictionsNB)
testOrder2<- testOrder2[testOrder2$predictedProbabilityChurn>= top10quantile,]

#testOrder <- predictionsNB[seq(from = 1, to =ceiling(.1*nrow(testOrder)), by= 1),]

class(predictionsNB$predictedProbabilityChurn)
?seq
head(testOrder)
# head(test)

# Confusion matrix ###
CrossTable(testData$churn, nbPostProb$nbPredClasses,
           prop.t = FALSE)
table(testData$churn, nbPostProb$nbPredClasses)



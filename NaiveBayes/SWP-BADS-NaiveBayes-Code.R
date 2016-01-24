
##########################################################
############# Naive Bayes Classifier #####################
##########################################################
# install.packages("gmodels")

#rm(list=ls())

library("klaR")
library("gmodels")

## Read in the data
# data <- read.csv("~/Google Drive/BADS_SWT/Missing_and_Variable_Selection/DataWithoutNAs.csv", sep=";") 
data <- dataWithoutNas


## Create data frame just with the variables that go into the model
dRed <- subset(data, select = c(churn, eqpdays , months , mou_Mean , hnd_price , totmrc_Mean ,
                                  change_mou , adjrev , totrev , avg3mou , mou_opkv_Mean , last_swap ,
                                  mou_Range , mou_cvce_Mean , totcalls , mou_peav_Mean , ovrrev_Mean , avgqty ,
                                  totmou , adjqty , comp_vce_Mean , complete_Mean , opk_vce_Mean , rev_Mean ,
                                  rev_Range , attempt_Mean))

#dRed$churn <- factor(dRed$churn, labels = c("not churn", "churn"))

## Naive Bayes Model
nb <- NaiveBayes(churn ~ ., data = dRed)

## Make the predictions on the test data
?predict

nbResults <- predict(nb, newdata = testData)
nbPostProb <- data.frame(nbResults$posterior)
cutoff <- 0.5
length(nbPostProb[,2])

head(nbPostProb)

nbPostProb$nbPredClasses <- factor(nbPostProb[,2] >= cutoff, 
                        labels = c("pred.churn", "pred.not churn"))

# Confusion matrix ###
CrossTable(testData$churn, nbPostProb$nbPredClasses,
           prop.t = FALSE)
table(testData$churn, nbPostProb$nbPredClasses)


### Build the accuracy parameter calculations - Idea - everyone with their models imput a data frame just with the following
# info : customer number, predicted probability of churning and the actual churn or not churn. 


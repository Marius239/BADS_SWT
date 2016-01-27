

######################################################
#### PREPARATION OF DATA OF THE TEST SET #####
######################################################

# install.packages("gtools")
library("gtools")

### Import Test Set and prepare it for prediction ####
d <- read.csv("~/Google Drive/Uni/1. WISE 1516/Business Analytics/Special Working Task/BADS - SWT - Trainingset - 2015-10-27/BADS-Trainingset.csv", stringsAsFactors=FALSE)
d2 <- read.csv("~/Google Drive/BADS_SWT/BADS-TestsetFinal.csv", stringsAsFactors=FALSE)

#dtest <- smartbind(d, d2, fill = "Missing")
#?smartbind
vars <- read.csv("~/Google Drive/BADS_SWT/Missing_Values/Variables_V2.csv", sep=";", stringsAsFactors=FALSE)

#Store data to ensure original set is not changed
dnew <- smartbind(d, d2, fill = "Missing")

#summary(dnew)


#-------------------------------------------------------------------------------
#Convert variable types of data to type in Excel Description and 
#Merge blanks and NAs of factor variables to level "Missing"

for(i in 1:ncol(dnew)){
  
  var  <- colnames(dnew)[i]  #Store variable name
  
  #Find position of 'var' in 'vars'
  ind  <- which(vars$Variable == colnames(dnew)[i])  #Position  
  type <- vars[ind,2]  #Get variable type
  
  #Ensure continuous variables are class numeric
  if(type == "Continuous")  dnew[,i] <- as.numeric(dnew[,i])
  
  #Ensure Categorical variables are class factor
  if(type == "Categorical"){
    
    #Convert to factor and keep NAs as level
    dnew[,i] <- factor(dnew[,i], exclude = NULL)
    
    #Set <NA> level as "Missing" (converts values as well)
    if(is.na(levels(dnew[,i])[length(levels(dnew[,i]))])){
      
      levels(dnew[,i])[length(levels(dnew[,i]))] <- "Missing"
    }
    
    #Merge blank level and <NA> level as jointly "Missing"
    if(levels(dnew[,i])[1] == "") levels(dnew[,i])[1] <- "Missing"
    
    
  }
}

#dnew <- dnew[, -172]  #Delete 'Customer_ID'



#Handle factor label 'Unknown' manually
levels(dnew$dualband)[4] <- "Missing"
levels(dnew$ethnic)[16]  <- "Missing"
levels(dnew$kid0_2)[2]   <- "Missing"
levels(dnew$kid3_5)[2]   <- "Missing"
levels(dnew$kid6_10)[2]  <- "Missing"
levels(dnew$kid11_15)[2] <- "Missing"
levels(dnew$kid16_17)[2] <- "Missing"
levels(dnew$marital)[6]  <- "Missing"
levels(dnew$new_cell)[2] <- "Missing"

#create binary age1/age2 variable and to data - to not miss information of people that 
age1_bool <- factor(dnew$age1, exclude = NULL)
levels(age1_bool)[length(levels(age1_bool))] <- 0
levels(age1_bool)[levels(age1_bool) != 0] <- 1

age2_bool <- factor(dnew$age2, exclude = NULL)
levels(age2_bool)[length(levels(age2_bool))] <- 0
levels(age2_bool)[levels(age2_bool) != 0] <- 1


dnew$age1_bool <- age1_bool  #Add age1_bool to data
dnew$age2_bool <- age2_bool  #Add age2_bool to data



# Age variables - bin it in ca. 10 year intervals + "Missing". Note zeros are labeled as "Missing"

agebreaks <- c(0,30,40,50,60,max(dnew$age1, dnew$age2, na.rm = TRUE))

dnew$age1 <- cut(dnew$age1, breaks = agebreaks)
levels(dnew$age1) <- c(levels(dnew$age1), "Missing")
dnew$age1[is.na(dnew$age1)] <- "Missing"

dnew$age2 <- cut(dnew$age2, breaks = agebreaks)
levels(dnew$age2) <- c(levels(dnew$age2), "Missing")
dnew$age2[is.na(dnew$age2)] <- "Missing"




#Convert last_swap to time difference (is given as date)
swap_char                    <- as.character(dnew$last_swap) 
swap_date                    <- as.Date(swap_char[swap_char != "Missing"],
                                        format = "%m/%d/%Y")
swap_diff                    <- as.Date("2002-01-31") - swap_date
swap                         <- swap_char 
swap[swap_char != "Missing"] <- swap_diff
swap_numeric                 <- as.numeric(swap)
# dnew$swap_numeric            <- swap_numeric introduces too many NAs - just keep the factor variable


#Bin original swap and take as factor (replace original variable)
swap_breaks            <- quantile(na.omit(swap_numeric), c(0, 0.25, 0.5, 0.75, 1))
swap_factor            <- factor(cut(swap_numeric, breaks = swap_breaks), exclude = NULL)
levels(swap_factor)[5] <- "No Swap" 
dnew$last_swap         <- swap_factor



#ref_qty to binary: Referral or no Referral
levels(dnew$REF_QTY)[length(levels(dnew$REF_QTY))] <- 0  #'Missing' indicates no referral

ref_qty_bool <- dnew$REF_QTY
levels(ref_qty_bool)[levels(ref_qty_bool) != "0"] <- "Y"
dnew$REF_QTY_bool <- ref_qty_bool

#Create binary variable for retention call days
retdays_bool <- factor(dnew$retdays, exclude = NULL)
levels(retdays_bool)[length(levels(retdays_bool))] <- "Not Called"
levels(retdays_bool)[levels(retdays_bool) != "Not Called"] <- "Called"
# add it to the dnew dataframe
dnew$retdays_bool <- retdays_bool

# Binning of retdays to avoid having so many missing values
#Bin original swap and take as factor (replace original variable)
retdays_numeric <- dnew$retdays


retdays_breaks            <- quantile(na.omit(retdays_numeric), c(0, 0.25, 0.5, 0.75, 1))
retdays_factor            <- factor(cut(retdays_numeric, breaks = retdays_breaks), exclude = NULL)
levels(retdays_factor) <- c(levels(retdays_factor), "Missing")
retdays_factor[is.na(retdays_factor)] <- "Missing"
dnew$retdays_factor         <- retdays_factor


# Redefine label "Missing" for variables total calls and total accepted offers from retention team that replaces missing by no call (to differentiate from the ones who we know that really didn't accept)
# Total calls
levels(dnew$tot_ret)[levels(dnew$tot_ret) == "Missing"] <- "Not Called"

# Total accepted offers
levels(dnew$tot_acpt)[levels(dnew$tot_acpt) == "Missing"] <- "Not Called"

# Create a numeric version of churn
dnew$churnNumeric <- as.numeric(dnew$churn) - 1

# Create dummies for rmcalls = NA, rmmou = NA, rmrev = NA 
dnew$rmcallsNA <- is.na(dnew$rmcalls)

dnew$rmmouNA <- is.na(dnew$rmmou)

dnew$rmrevNA <- is.na(dnew$rmrev)

# deal with csa - It denotes different cities in combination with area codes

dnew$csaCities <- factor(substr(dnew$csa, 1, 3)) 
levels(dnew$csaCities)[levels(dnew$csaCities) == "Mis"] <- "Missing"
levels(dnew$csaCities)[levels(dnew$csaCities) == "SLU" |
                         levels(dnew$csaCities) == "LAW" |
                         levels(dnew$csaCities) == "HOP" |
                         levels(dnew$csaCities) == "INU" |
                         levels(dnew$csaCities) == "SFU"] <- "Missing"

nlevels(dnew$csaCities)
summary(as.factor(dnew$csaCities))
# Total amount of levels 58 - we need max 56 to be able to perform the random forest approach. There were
# 3 cities with just one observation - I changed them to be missing


# Split info into two data frames with i) all categorical variables and ii) all numeric variables

is_factor <- sapply(dnew, is.factor) # Logical vector that tells which variables are categorical
is_numeric <- sapply(dnew, is.numeric)
dnewCategorical <- dnew[, is_factor] # Data frame just with categorical variables
dnewNumeric <- dnew[, is_numeric]





#-------------------------------------------------------------------------------
#Count function to count 'Missing' in Factors and NAs in numericals



count<- function(x){
  
  if(class(x) == "factor"){ 
    z <- sum(x == "Missing")/length(x)
    return(z)
  }
  
  if(class(x) == "numeric"){
    z <- 1-length(na.omit(x))/length(x)
    return(z)
  }
  
}

#-------------------------------------------------------------------------------
#Compute proportion of missing values for all variables

missing_var <- sapply(dnew, count)
missing_var_categorical <- sapply(dnewCategorical, count)
missing_var_numeric <- sapply(dnewNumeric, count)

sort(missing_var_categorical, decreasing = TRUE) # check percentages 
sort(missing_var_numeric, decreasing = TRUE)

# Idea behind this different check division into categorical and numeric: NAs ("Missing") in Categorical variables
# can be used as a category, whereas for numeric variables they truly are NAs (some models cannot like 
# random forests cannot run with NAs, so we want to minimize them)


# After checking the percentages for the numeric variables We eliminate retdays, rmcalls, rmmou 
# The rest of the variables have less than 3% NAs this shouldn't change dramatically the results
#dnew2 <- subset(dnew, select = -c(retdays, rmcalls, rmmou, rmrev))

dnew$dummyFinalTestSet <- is.element(dnew$Customer_ID, d2$Customer_ID) # TRUE if the Customer_ID is in the Final Test Dataset

#### Re-divide the data sets into training and test set ####
trainingSetFinal <- dnew[dnew$dummyFinalTestSet == FALSE,]
testSetFinal <- dnew[dnew$dummyFinalTestSet == TRUE,]
trainingSetFinal$churn <- droplevels(trainingSetFinal$churn)

trainingSetFinal <- subset(trainingSetFinal, select = c(Customer_ID, eqpdays, months, mou_Mean, totmrc_Mean,last_swap,
                                             hnd_price,adjrev, change_mou, mou_cvce_Mean, avg3mou,
                                             totrev,mou_Range, mou_opkv_Mean,totcalls,phones, avg3qty,
                                             complete_Mean, peak_vce_Mean,comp_vce_Mean, totmou, opk_vce_Mean,
                                             mou_peav_Mean,adjqty, avgqty,adjmou,rev_Mean, ovrmou_Range,
                                             rev_Range, ovrmou_Mean, plcd_vce_Mean, churn))




testSetFinal <- subset(testSetFinal, select = c(Customer_ID, eqpdays, months, mou_Mean, totmrc_Mean,last_swap,
                                                   hnd_price,adjrev, change_mou, mou_cvce_Mean, avg3mou,
                                                   totrev,mou_Range, mou_opkv_Mean,totcalls,phones, avg3qty,
                                                   complete_Mean, peak_vce_Mean,comp_vce_Mean, totmou, opk_vce_Mean,
                                                   mou_peav_Mean,adjqty, avgqty,adjmou,rev_Mean, ovrmou_Range,
                                                   rev_Range, ovrmou_Mean, plcd_vce_Mean, churn))


save(trainingSetFinal, file = "~/Google Drive/BADS_SWT/TrainingData.RData")
save(testSetFinal, file = "~/Google Drive/BADS_SWT/TestData.RData")

set.seed(13)
# Division in two uses 70% for training and 30% for testint
ind <- createDataPartition(trainingSetFinal$churn, p = .7, list = FALSE)


trainDataNew  <- trainingSetFinal[ind,]
testDataNew <- trainingSetFinal[-ind,]






#summary(dnew2$dummyFinalTestSet)

#testDataSetFinal <- dnew[dnew$dummyFinalTestSet == TRUE,]
#trainingDataSetOriginal <- dnew[dnew$dummyFinalTestSet == FALSE,]
######
#droplevels(trainingDataSetOriginal$churn)



  

  
  


# summary(dnew2$hnd_price)



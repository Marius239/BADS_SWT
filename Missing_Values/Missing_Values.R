#Clear working space and close windows
rm(list = ls())
#graphics.off()

#Set working directory to place where files are located
setwd("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/")

#Read training data set
d <- read.csv("BADS - SWT - Trainingset - 2015-10-27/BADS - SWT - Trainingset - 2015-10-27.csv", 
              stringsAsFactors = FALSE)

#Read file with variable types
vars <- read.csv2("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/Variables.csv",
                  stringsAsFactors = FALSE)

#Store data to ensure original set is not changed
dnew <- d


#-------------------------------------------------------------------------------
#Convert variable types of data to type in Excel Dexcription and 
#Merge blanks and NAs of factor variables to level "Missing"

for(i in 1:ncol(dnew)){
  
  var  <- colnames(dnew)[i]  #Store variable name
  
  #Find osition of 'var' in 'vars'
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


#-------------------------------------------------------------------------------
# Delete all Variables with Missing Values >= 50%

five     <- missing_var[missing_var>=0.5]  # Contains variables where more than 50% are missing
length(five) #22 Variables with at least 50% NAs

idx <- missing_var < 0.5  #Index for variables that have more than 50% of observations
dnew2 <- dnew[,idx]  # Dataset without Variables having more thant 50% Missings 


#-------------------------------------------------------------------------------
#Check for horizontal NAs, i.e. NAs per individual (DELETE IN NEXT SECTION)

#Create data set to count NAs faster but use information to subset 'dnew'
dnew_hoz <- dnew2

#Coerce 'Missing' in factors to NA
factors <- which(sapply(dnew_hoz, is.factor))  #Find position of factors

for(i in factors){
  
  miss_idx <- levels(dnew_hoz[,i]) == "Missing"  #Find where 'Missing' is
  
  #If 'Missing' is contained, coerce to NA
  if(sum(miss_idx) == 1){
    id <- which(miss_idx)  #Position of 'Missing'
    levels(dnew_hoz[,i])[id] <- NA  #Coerce to NA
  }
  
}

#Count horizontal missings
n     <- nrow(dnew_hoz)  #Number of individuals
n_var <- ncol(dnew_hoz)  #Number of variables
missing_ind <- rep(NA, n)  #Vector to safe result
for(i in 1:n){
  
  num_na         <- sum(is.na(dnew_hoz[i,]))  #Sum of NAs
  missing_ind[i] <- num_na/n_var  #Share of NAs
}


#------------------------------------------------------------------------------------
# Delete Observations with too many Missing Values

ten <- missing_ind[missing_ind>=0.1]
length(ten)  #1080 individuals with at least 10% NAs

idx <- missing_ind < 0.1
dnew3 <- dnew2[idx,]  # Dataset without Observations with at least 10% Missings 


#-------------------------------------------------------------------------------------
# Dataset without Missing Values in Continuous Variables

data <- na.omit(dnew3)

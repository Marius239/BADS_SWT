#Clear working space and close windows
rm(list = ls())
#graphics.off()

#Set working directory to place where files are located
setwd("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/")

#Read training data set
d <- read.csv("BADS - SWT - Trainingset - 2015-10-27/BADS - SWT - Trainingset - 2015-10-27.csv", 
              stringsAsFactors = FALSE)

#Read file with variable types
vars <- read.csv2("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/Variables_V2.csv",
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

dnew <- dnew[, -172]  #Delete 'Customer_ID'

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

#Convert 0 in age1 and age2 to NA
dnew$age1[dnew$age1 == 0] <- NA
dnew$age2[dnew$age2 == 0] <- NA

#create binary age1/age2 variable and to data
age1_bool <- factor(dnew$age1, exclude = NULL)
levels(age1_bool)[length(levels(age1_bool))] <- 0
levels(age1_bool)[levels(age1_bool) != 0] <- 1

age2_bool <- factor(dnew$age2, exclude = NULL)
levels(age2_bool)[length(levels(age2_bool))] <- 0
levels(age2_bool)[levels(age2_bool) != 0] <- 1

dnew$age1_bool <- age1_bool  #Add age1_bool to data
dnew$age2_bool <- age2_bool  #Add age2_bool to data

#Convert last_swap to time difference (is given as date)
swap_char                    <- as.character(dnew$last_swap) 
swap_date                    <- as.Date(swap_char[swap_char != "Missing"],
                                        format = "%m/%d/%Y")
swap_diff                    <- as.Date("2002-01-31") - swap_date
swap                         <- swap_char 
swap[swap_char != "Missing"] <- swap_diff
swap_numeric                 <- as.numeric(swap)
dnew$swap_numeric            <- swap_numeric

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
length(ten)  #1081 individuals with at least 10% NAs

idx <- missing_ind < 0.1
dnew3 <- dnew2[idx,]  # Dataset without Observations with at least 10% Missings 


#-------------------------------------------------------------------------------------
# Dataset without Missing Values in Continuous Variables

data <- na.omit(dnew3)

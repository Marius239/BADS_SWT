#Clear working space and close windows
rm(list = ls())
#graphics.off()

#Set working directory to place where files are located
setwd("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/")

#Read training data set
d <- read.csv("BADS - SWT - Trainingset - 2015-10-27/BADS - SWT - Trainingset - 2015-10-27.csv", 
              stringsAsFactors = FALSE)

#Read variable type
vars <- read.csv2("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/Variables.csv",
                  stringsAsFactors = FALSE)

#Store data to ensure original set is not altered
dnew <- d

#---------------------------------------------------------------------
#Convert variable type

for(i in 1:ncol(dnew)){
  
  var  <- colnames(dnew)[i]
  ind  <- which(vars$Variable == colnames(dnew)[i])
  type <- vars[ind,2]
  
  if(type == "Continuous")  dnew[,i] <- as.numeric(dnew[,i])
  
  if(type == "Categorical"){
    
    dnew[,i] <- factor(dnew[,i], exclude = NULL)
    if(is.na(levels(dnew[,i])[length(levels(dnew[,i]))])){
      
      levels(dnew[,i])[length(levels(dnew[,i]))] <- "Missing"
    }
    if(levels(dnew[,i])[1] == "") levels(dnew[,i])[1] <- "Missing"
    
    
  }
}

#---------------------------------------------------------------------
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
  
  #Count rows
  if(typeof(x) == "character"){
    num_na      <- sum(is.na(dnew[i,])) 
    z <- num_na/length(x)
    return(z)
  }
  
}


#-------------------------------------------------------------------------------
#Check for vertical NAs

missing_var <- sapply(dnew, count)


#-------------------------------------------------------------------------------
#Check for horizontal NAs

#Create data set count NAs faster but use information to subset 'dnew'
dnew_hoz <- dnew

#Coerce 'Missing' in factors to NA
factors <- which(sapply(dnew_hoz, is.factor))

#sapply(dnew_hoz[,factors], levels)

for(i in factors){
  
  miss_idx <- levels(dnew_hoz[,i]) == "Missing"
  
  if(sum(miss_idx) == 1){
    id <- which(miss_idx)
    levels(dnew_hoz[,i])[id] <- NA
  }
  
}

#Count
n <- nrow(dnew_hoz)
missing_ind <- rep(NA, n)
for(i in 1:n){
  
  num_na      <- sum(is.na(dnew_hoz[i,]))
  missing_ind[i] <-num_na/n  
}

#Clear working space and close windows
rm(list = ls())
graphics.off()

#Set working directory to place where files are located
setwd("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/")

#Read training data set
d <- read.csv("BADS - SWT - Trainingset - 2015-10-27/BADS - SWT - Trainingset - 2015-10-27.csv", 
              stringsAsFactors = FALSE)

#Convert target variable to factor
d$churn <- factor(d$churn, labels = c("retain", "churn"))


##################
# Missing Values #
##################

#Convert missings in string variables to NA and then the string to a factor
na <- function(x){
  
  x[x==""]<-NA
  if(is.character(x)) x <- as.factor(x)
  
  return(x)
}

#Apply na to all variables
dnew <- as.data.frame(apply(d,2,na))  

#Compute proportion of NA to check with excel file
count <- function(x){
  
  z <- 1-length( na.omit(x))/length(x)
  return(z)
  
}

missing  <- data.frame(t(apply(dnew,2,count)))
five     <- subset(missings, select = missings[1,]>0.5)  #Show variables where more than 50% are missing
sort(five)

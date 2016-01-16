
#Clear working space and close windows
rm(list = ls())
graphics.off()

#Set working directory to place where files are located
setwd("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/")

#Read training data set
d <- read.csv("BADS - SWT - Trainingset - 2015-10-27/BADS - SWT - Trainingset - 2015-10-27.csv", 
              stringsAsFactors = FALSE)


##################
# Missing Values #
##################

#Convert missings in string variables to NA and then the string to a factor
na <- function(x){

  if(class(x)!="numeric") {
    x[x==""]<-NA    
    x <- as.factor(x)
  }
  
  return(x)
}

dnew <- d

for (i in 1:173){
  dnew[,i]<-na(dnew[,i])
  
}

levels(dnew$churn)<-c("retain","churn")

# Apply na to all variables
# dnew <- as.data.frame(apply(d,2,na))  

#Compute proportion of NA to check with excel file
count <- function(x){
  
  z <- 1-length( na.omit(x))/length(x)
  return(z)
  
}

missing  <- data.frame(t(apply(dnew,2,count)))

# Delete all variables with missing >=0.5

five     <- subset(missing, select = missing[1,]>=0.5)  #Show variables where more than 50% are missing
five
length(five)
sort(five)

idx <- missing[1,]>=0.5
idx <- !idx[1,]
dnew2 <- dnew[,idx]

#Check: lenght must be 0
miss_check  <- data.frame(t(apply(dnew2,2,count)))
five_check <- subset(miss_check, select = miss_check[1,]>=0.5)  #Show variables where more than 50% are missing
length(five_check)
hist(as.numeric(miss_check[1,]))

#edit(missing)
#Outlier Detection and Data Cleaning

#Check Variables with missings \in [10%, 50%)
ten_check <- subset(miss_check, select = miss_check[1,]>=0.1)  #Show variables where more than 50% are missing
length(ten_check)

ls(ten_check)


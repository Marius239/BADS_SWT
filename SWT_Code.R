rm(list = ls())
graphics.off()


setwd("C:/Users/User/Desktop/Master/5th_Semester/Business_Analytics/SWT/")
d <- read.csv("BADS - SWT - Trainingset - 2015-10-27/BADS - SWT - Trainingset - 2015-10-27.csv")

d$churn <- factor(d$churn, labels = c("retain", "churn"))


# Missing Values 

na <- function(x){
  
  x[x==""]<-NA
  return(x)
}

dnew <- apply(d,2,na)

count <- function(x){
  
  z <- 1-length( na.omit(x))/length(x)
  return(z)
  
}

missings <- apply(dnew,2,count)
missings[missings>0.5]


#das ist ein test

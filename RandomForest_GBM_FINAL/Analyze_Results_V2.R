rm(list = ls())

setwd("C:/Users/User/Desktop/RandomForest/Results")

#Go to last code section to specify output
k <- 3  #2: AUC; 3: Lift Score

#------------------------------------------------------------------------------
#Data Manipulation

files   <- list.files()
files   <- files[files != "Archive" & files != "ReadMe.txt"]  #Drop Archive and ReadMe
df_File <- gsub(".RData", "",files)
df_File <- gsub("RF_", "",df_File)

load(files[1])

s <- data.frame(File = df_File[1], stringsAsFactors = FALSE)
s$Min    <- summary(result[,k])[1]
s$Quart1 <- summary(result[,k])[2]  
s$Median <- summary(result[,k])[3]  
s$Mean   <- summary(result[,k])[4]   
s$Quart3 <- summary(result[,k])[5]  
s$Max    <- summary(result[,k])[6]   
  
for(i in 2:length(files)){
  
  load(files[i])
  
  t <- data.frame(File = df_File[i])
  t$Min    <- summary(result[,k])[1]
  t$Quart1 <- summary(result[,k])[2]  
  t$Median <- summary(result[,k])[3]  
  t$Mean   <- summary(result[,k])[4]   
  t$Quart3 <- summary(result[,k])[5]  
  t$Max    <- summary(result[,k])[6]  
  
  s <- rbind(s, t)
  
} 

#Add Median and Mean Rank to 's'
p <- order(s$Mean, decreasing = TRUE)  #Specify Sort Estimator
c <-which(names(s) %in% c("File", "Median", "Mean"))  #Specify Estimators to be shown

rankMean <- data.frame(File = s[p, c]$File, MeanRank = 1:length(files))

p <- order(s$Median, decreasing = TRUE)  #Specify Sort Estimator
c <-which(names(s) %in% c("File", "Median", "Mean"))  #Specify Estimators to be shown

rankMed <- data.frame(File = s[p, c]$File, MedianRank = 1:length(files))

s <- merge(s, rankMed, by = "File")
s <- merge(s, rankMean, by = "File")

p <- order(s$Median, decreasing = TRUE)

s$AvRank <- (s$MeanRank + s$MedianRank)/2

#------------------------------------------------------------------------------
#Output

#Specify Sort Estimator (For sort by AvRank set decreasing to FALSE, else TRUE)
estimator_sort     <- order(s$AvRank, decreasing = F)  

#Specify Estimators to be shown
estimators_display <- which(names(s) %in% c("File", 
                                            "Median", 
                                            "Mean",
                                            "MedianRank", 
                                            "MeanRank",
                                            "AvRank", "Min", "Max")) 

z <- s[estimator_sort, estimators_display]
z[,]



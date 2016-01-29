rm(list = ls())

setwd("C:/Users/User/Desktop/RandomForest/Results")


files   <- list.files()
df_File <- gsub(".RData", "",files)
df_File <- gsub("RF_", "",df_File)

load(files[1])

s <- data.frame(File = df_File[1])
s$Min    <- summary(result[,3])[1]
s$Quart1 <- summary(result[,3])[2]  
s$Median <- summary(result[,3])[3]  
s$Mean   <- summary(result[,3])[4]   
s$Quart3 <- summary(result[,3])[5]  
s$Max    <- summary(result[,3])[6]   
  
for(i in 2:length(files)){
  
  load(files[i])
  
  t <- data.frame(File = df_File[i])
  t$Min    <- summary(result[,3])[1]
  t$Quart1 <- summary(result[,3])[2]  
  t$Median <- summary(result[,3])[3]  
  t$Mean   <- summary(result[,3])[4]   
  t$Quart3 <- summary(result[,3])[5]  
  t$Max    <- summary(result[,3])[6]  
  
  s <- rbind(s, t)
  
} 

  


#Load Data with Results for Binned 'phone'

load("RF_2000Tree_30Vars.RData")
v30t2k <- result

load("RF_3000Tree_30Vars.RData")
v30t3k <- result

load("RF_2000Tree_27Vars.RData")
v27t2k <- result

load("RF_3000Tree_27Vars.RData")
v27t3k <- result

load("RF_2000Tree_25Vars.RData")
v25t2k <- result

load("RF_3000Tree_25Vars.RData")
v25t3k <- result

load("RF_2000Tree_13Vars.RData")
v13t2k <- result

load("RF_3000Tree_13Vars.RData")
v13t3k <- result

#Load Data with Results with original'phone'

load("RF_3000Tree_30Vars_Phone.RData")
v30t3k_phone <- result

load("RF_3000Tree_27Vars_Phone.RData")
v27t3k_phone <- result

load("RF_3000Tree_25Vars_Phone.RData")
v25t3k_phone <- result



#------------------------------------------------------------------------------
#Summary Statistics

summary(v13t2k[,3])
summary(v13t3k[,3])

summary(v25t2k[,3])
summary(v25t3k[,3])

summary(v27t2k[,3])
summary(v27t3k[,3])

summary(v30t2k[,3])
summary(v30t3k[,3])


summary(v30t3k_phone[,3])
summary(v27t3k_phone[,3])
summary(v25t3k_phone[,3])

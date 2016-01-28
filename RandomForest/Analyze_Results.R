rm(list = ls())

setwd("C:/Users/User/Desktop/RandomForest/Results")

load("RF_2000Tree_30Vars.RData")
v30t2k <- result

load("RF_2000Tree_13Vars.RData")
v13t2k <- result

load("RF_3000Tree_13Vars.RData")
v13t3k <- result

summary(v30t2k[,3])
summary(v13t2k[,3])
summary(v13t3k[,3])

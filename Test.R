a <- c("a","b","","")
na(a)

b <- d$mailflag
b
na(b)

dnew[,155]

test <- dnew[1:1000,]

tdf <- data.frame(test, stringsAsFactors = FALSE)
class(tdf$rmrev)
?as.data.frame
tdf$rmrev <- as.numeric(tdf$rmrev)
tdf$mailflag <- as.factor(tdf$mailflag)
tdf$rmrev

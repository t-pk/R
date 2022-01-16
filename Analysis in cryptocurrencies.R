#install.packages("reshape2")
#install.packages("quadprog")
#install.packages("corrplot")
#install.packages("xts")


#GUIDE: https://www.kaggle.com/hrkzszk/analysis-in-cryptocurrencies-using-r/script
#1.1 Import train data
install.packages('TTR', dependencies=TRUE, repos='http://cran.rstudio.com/')

data <- read.csv("..\\R\\input\\100coin.csv", skip = 0) #config path.

#1.2 Summary of the data
head(data)
#print(file.choose())

#2.1 Close price data
library(reshape2)
#Change data frame with row=date and col=close.price
close.raw <- reshape(data[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
close.raw[,"Close.Currency"] <- NULL

#Change values into numeric and get it as the dataframe style
close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
#Change names of cols
colnames(close) <- sub("Close.", "", colnames(close))

#Change the date column into POSIXct style
library(lubridate)
dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y", locale = "eng")
close$Date<- dates

#Delete the last row since it has no information
close <- close[-nrow(close),]

dim(close)

#2.2 Number of days past after the coin was issued
length.col <- colSums(!is.na(close[,-1]))
#Newest cryptocurrency in this dataset
sort(length.col)[1]

#Summary table about how long the cryptocurrencies are. Unit is day.
options("scipen"=100, "digits"=4)
table(cut(length.col, c(0, 180, 365), right = T))
#Percentage
table(cut(length.col, c(0, 180, 365), right = T))/100*100

#3.0 Correlation analysis

#Take cols with more than 200
close.180 <- close[,colSums(!is.na(close)) >= 180]

##Correlation between all of cryptocurrencies
corr <- cor(close.180[,-1], use = "pairwise.complete")

#3.1
 library(corrplot)
 corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 1,
          title = "Correlation matrix (ordered by hierarchical clustering)",
          mar = c(0,1,2,0))


#3.2
#Make correlation matrix between bitcoin and all of alt coins
print(1:ncol(corr))
corr.bit <- corr[1:ncol(corr),"BTC", drop=FALSE]
corrplot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.5, mar = c(0,1,2,0))

corr.bit.dec.order <- corr.bit[order(corr.bit, decreasing=T),,drop=F]
data.frame(name=corr.bit.dec.order[2:6,0], cor=corr.bit.dec.order[2:6,1])

#3.2.1
corr.bit.dec.order <- corr.bit[order(corr.bit, decreasing=T),,drop=F]
data.frame(name=corr.bit.dec.order[2:6,0], cor=corr.bit.dec.order[2:6,1])

#3.2.2
corr.bit.inc.order <- corr.bit[order(corr.bit, decreasing=F),,drop=F]
data.frame(name=corr.bit.inc.order[1:5,0], cor=corr.bit.inc.order[1:5,1])

#4.1
markcap <- data[c(1,2,8)]
markcap$Market.Cap[markcap$Market.Cap == "-"] <- NA

#Change data frame with row=date and col=close.price
markcap.raw <- reshape(markcap, timevar= "Currency", idvar = "Date", direction = "wide")
markcap.raw[,"Market.Cap.Currency"] <- NULL

#Change values into numeric
markcap <- sapply(markcap.raw, function(z){as.numeric(gsub(",","", z))})
#Make data data.frame while deleting the last row because this has no info
markcap <- data.frame(markcap[-nrow(markcap),])
#Take cols with more than 200
markcap <- markcap[,colSums(!is.na(markcap)) >= 200]
#Change names of cols
colnames(markcap) <- sub("Market.Cap.", "", colnames(markcap))

#4.2 Average market cap
mean.cap <- data.frame(mean.cap=colMeans(markcap, na.rm = T))
mean.cap.10.name <- rownames(mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F])[1:10]
mean.cap.10.value <- mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F][1:10,]
mean.cap.10 <- data.frame(name=mean.cap.10.name, mean.market.cap=mean.cap.10.value)
mean.cap.10
barplot(mean.cap.10[,2], names.arg = mean.cap.10[,1],las=0.9 , cex.names=0.9,
        main="Average market capital in top 10 Cryptocurrencies")


#5.1
library(xts)
rownames(close) <- close$Date
close.xts <- as.xts(close)

#5.2

price10 <- as.xts(close[ ,mean.cap.10.name])
plot.xts(price10, main="Price")


#5.3

library(PerformanceAnalytics)
ret10.xts <- CalculateReturns(price10, method="log")
ret10 <- data.frame(ret10.xts)

#5.4

options(digits = 3)
data.frame(mean.percent = sort(apply(ret10[,1:ncol(ret10)], 2,
                                     function(x) mean(x, na.rm=TRUE)), decreasing = T))*100


#5.5
options(digits = 3)
data.frame(variance.percent = sort(apply(ret10[,1:ncol(ret10)],
                                         2, function(x) sd(x, na.rm=TRUE)), decreasing = T))*100

#5.6
options(digits = 3)
data.frame(variance.percent = sqrt(sort(apply(ret10[,1:ncol(ret10)],
                                              2, function(x) sd(x, na.rm=TRUE)), decreasing = T)))*100

#5.7

library(PerformanceAnalytics)
CVaR(ret10)



#5.8

library(TTR)
#5.9

vol30 <- xts(apply(ret10.xts, 2, runSD,n=30), index(ret10.xts))*sqrt(252)
plot.xts(vol30)


par(mfrow=c(2,1))
for(i in 1:ncol(vol30)){
  print(plot(vol30[,i], main=colnames(vol30)[i]))
}

#5.10

chart.Correlation(ret10)

#6.1

par(mfrow=c(2,1));
for(i in 1:ncol(ret10)){
  plot(density(ret10[,i], na.rm = T), main=colnames(ret10)[i])
}

#7.1

ret10.CF <- na.omit(ret10)
mean_vect <- colMeans(ret10.CF)
cov_mat <- cov(ret10.CF)
sd_vect <- sqrt(diag(cov_mat))

M <- length(mean_vect)

library(quadprog)
Amat <- cbind(rep(1,M), mean_vect) # set the constraints matrix
muP <- seq(0.0, 0.3, length=300) # set of 300 possible target values
sdP <- muP # set up storage for std dev's of portfolio returns
weights <- matrix(0, nrow=300, ncol=M) # set up storage for weights
for (i in 1:length(muP)){
  bvec <- c(1, muP[i]) # constraint vector
  result <- solve.QP(Dmat = cov_mat, dvec=rep(0,M), Amat=Amat, bvec = bvec, meq=2)
  sdP[i] <- sqrt(2*result$value)
  weights[i,] <- result$solution
}

plot(100*sdP, 100*muP, type="l", xlim=c(0,250), ylim=c(0,35),
     main="Efficient Frontier", ylab="Expected Return(%)", xlab="Standard deviation(%)")

ind.min <- which.min(sdP)
options(digits = 3)

#Expected return
muP[ind.min]

#Expected standard deviation
sdP[ind.min]

#Proportions
weight <- data.frame(proportion = weights[ind.min,], row.names = colnames(ret10.CF))
weight


#5.X Log-return
library(xts)
rownames(close) <- close$Date
close.xts <- as.xts(close)
price10 <- as.xts(close[ ,mean.cap.10.name])
plot.xts(price10, main="Price")

library(PerformanceAnalytics)
ret10 <- CalculateReturns(price10, method="log")
sort(apply(ret10[,1:ncol(ret10)], 2, function(x) mean(x, na.rm=TRUE)), decreasing = T)
sort(apply(ret10[,1:ncol(ret10)], 2, function(x) sd(x, na.rm=TRUE)), decreasing = T)
plot.xts(ret10, main="log-return", ylim = c(-3,2))

par(mfrow=c(2,1));
for(i in 1:ncol(ret10)){
  print(plot(ret10[,i], main=colnames(ret10)[i]))
}



setwd("/Users/li/Downloads/")
#install.packages("fBasics")
library(fBasics)
n2f4=read.csv(file="movementFeatures_n2nf4.csv", header=TRUE, sep=",")
par(mfrow=c(1,1))


# speed
plot(n2f4$TimeElapsed, n2f4$Speed,type='l')

basicStats(n2f4$Speed)
# histogram of speed
par(mfcol=c(2,2)) 
hist(n2f4$Speed, xlab="n2nf4 Speed", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(n2f4$Speed),max(n2f4$Speed),length=40)
yfit<-dnorm(xfit,mean=mean(n2f4$Speed),sd=sd(n2f4$Speed))
lines(xfit, yfit, col="blue", lwd=2) 

qqnorm(n2f4$Speed)
qqline(n2f4$Speed, col = 2) 
# little skewed, but acceptable.

normalTest(n2f4$Speed,method=c("jb"))  


# Acceleration
plot(n2f4$TimeElapsed, n2f4$Acceleration,type='l')

basicStats(n2f4$Acceleration)
# histogram of Acceleration
par(mfcol=c(2,2)) 
hist(n2f4$Acceleration, xlab="n2nf4 Acceleration", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(n2f4$Acceleration),max(n2f4$Acceleration),length=40)
yfit<-dnorm(xfit,mean=mean(n2f4$Acceleration),sd=sd(n2f4$Acceleration))
lines(xfit, yfit, col="blue", lwd=2) 

qqnorm(n2f4$Acceleration)
qqline(n2f4$Acceleration, col = 2) 
# little skewed, but acceptable.

normalTest(n2f4$Acceleration,method=c("jb"))  





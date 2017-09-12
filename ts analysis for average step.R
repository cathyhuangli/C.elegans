library(tseries)
library(fBasics)
library(zoo)
library(forecast)
library(lmtest)

####### egl19_f2
df<-steptime[steptime$video_name=="egl19_f2",]
df$xrange <- cut(df$Time.Elapsed, breaks=seq(0, 36000, 60))
library(plyr)
dt<-ddply(df, .(xrange), summarise, m = mean(Step.length.in.mm))
d1<-data.frame(dt)
d1$time<-seq.int(nrow(d1))
#plot(d1$time,d1$m)
#lines(d1$time,d1$m)
spts<-ts(d1$m,start=1,frequency=1)
plot(spts)
lspts<-log(spts)
dspts<-diff(spts)
plot(lspts)
plot(dspts)

library(fUnitRoots)
adfTest(spts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(spts,lags=6,type="nc")
adfTest(lspts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(lspts,lags=6,type="nc")

acf(coredata(dspts),lag=30)
pacf(coredata(dspts))

plot(lspts)
acf(lspts)
pacf(lspts)
auto.arima(lspts, max.p = 5, max.q = 5,stationary = F, ic =c("bic"), trace=TRUE, allowdrift=TRUE) 
m1=Arima(lspts, order=c(0,1,1), method='ML')
coeftest(m1)
Box.test(m1$residuals,lag=6,type='Ljung-Box', fitdf=length(m1$coef))# fitdf is the number of parameters in model
Box.test(m1$residuals,lag=12,type='Ljung-Box', fitdf=length(m1$coef))
acf(m1$residuals)
source("backtest.R")
ntrain=round(0.9*length(lspts))
backtest(m1,lspts,ntrain,1)
f1=forecast(m1,h=8)
plot(f1, include=1000)
lines(ts(c(f1$fitted, f1$mean)),col="blue")

####### N2_f2
df<-steptime[steptime$video_name=="N2_f2",]
df$xrange <- cut(df$Time.Elapsed, breaks=seq(0, 36000, 60))
library(plyr)
dt<-ddply(df, .(xrange), summarise, m = mean(Step.length.in.mm))
d1<-data.frame(dt)
d1$time<-seq.int(nrow(d1))
plot(d1$time,d1$m)
lines(d1$time,d1$m)
spts<-ts(d1$m,start=1,frequency=1)
plot(spts)
lspts<-log(spts)
dspts<-diff(spts)
plot(spts)
plot(dspts)
acf(dspts)
pacf(dspts)

plot(lspts)
acf(lspts)
pacf(lspts)
auto.arima(lspts, max.p = 5, max.q = 5,stationary = F, ic =c("bic"), trace=TRUE, allowdrift=TRUE) 
m1=Arima(lspts, order=c(1,1,1), method='ML')
coeftest(m1)
Box.test(m1$residuals,lag=6,type='Ljung-Box', fitdf=length(m1$coef))# fitdf is the number of parameters in model
Box.test(m1$residuals,lag=12,type='Ljung-Box', fitdf=length(m1$coef))
acf(m1$residuals)
source("backtest.R")
ntrain=round(0.9*length(lspts))
backtest(m1,lspts,ntrain,1)
f1=forecast(m1,h=8)
plot(f1, include=1000)
lines(ts(c(f1$fitted, f1$mean)),col="blue")

######## multivariate time series with egl19_f2
library(tseries)
library(fBasics)
library(lmtest)
library(forecast)
df<-steptime[steptime$video_name=="egl19_f2",]
cor(df$Angle.between.2.step.lengths,df$Step.length.in.mm)
cor(df$Step.length.in.mm,df$Speed.in.mm.per.sec)
cor(df$Step.length.in.mm,df$Duration)
df$xrange <- cut(df$Time.Elapsed, breaks=seq(0, 36000, 60))
library(plyr)
dt<-ddply(df, .(xrange), summarise, step = mean(Step.length.in.mm))
ds<-ddply(df, .(xrange), summarise, angle = mean(Angle.between.2.step.lengths))
dp<-ddply(df, .(xrange), summarise, speed = mean(Speed.in.mm.per.sec))
dd<-ddply(df, .(xrange), summarise, duration = mean(Duration))
d1<-data.frame(dt,ds,dd,dp)
d1$time<-seq.int(nrow(d1))
cor(d1[,c(2,4,6,8)])

#plot(d1$time,d1$m)
#lines(d1$time,d1$m)
spts<-ts(d1$step,start=1,frequency=1)
agts<-ts(d1$angle,start=1,frequency=1)
drts<-ts(d1$duration,start=1,frequency=1)
sdts<-ts(d1$speed,start=1,frequency=1)
plot(spts)
plot(agts)
plot(sdts)
plot(drts)
lspts<-log(spts)
lagts<-log(agts)
lsdts<-log(sdts)
ldrts<-log(drts)
plot(lspts)
plot(lagts)
plot(lsdts)
plot(ldrts)

# test for stationarity
library(fUnitRoots)
adfTest(spts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(spts,lags=6,type="nc")
adfTest(agts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(agts,lags=6,type="nc")
adfTest(lspts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(lspts,lags=6,type="nc")
adfTest(ldrts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(ldrts,lags=6,type="nc")
adfTest(lsdts,lags=3,type="nc")#non-stationarity as null hypothesis
adfTest(lsdts,lags=6,type="nc")

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(d1$time,log(d1$step), type="l",ylim=c(min(log(d1$step)), max(log(d1$angle))),main="time plot of log(step.length),log(angle),log(speed),log(duration)")
lines(d1$time,log(d1$angle), col="red")
lines(d1$time,log(d1$duration), col="blue")
lines(d1$time,log(d1$speed), col="green")
legend("topright", c("step.length","angle","duration","speed"),#   
       lty=c(1,1), inset=c(-0.45,0),# gives the legend appropriate symbols (lines)
       lwd=c(2.0,2.0),col=c("black","red","blue","green")) # gives the legend lines the correct color and width
par(mfrow = c(1,1))
acf(lspts)
pacf(lspts)
m1=arima(as.vector(lspts), order=c(0,1,1), xreg=data.frame(lsdts, ldrts, lagts), method="ML")
coeftest(m1)
m2=arima(as.vector(lspts), order=c(0,1,1), xreg=data.frame(lsdts, ldrts), method="ML")
coeftest(m2)
# test model efficiency
acf(coredata(m2$resid), na.action = na.pass, lag=40)
Box.test(m2$resid, lag=5)
Box.test(m2$resid, lag=10)

source("backtest.R")
backtest(m2,lspts,xre=data.frame(lsdts, ldrts),129,1,inc.mean=F)


m3=arima(as.vector(lspts), order=c(0,1,1), xreg=data.frame(lsdts, ldrts,tslag(lsdts),tslag(ldrts)), method="ML")
coeftest(m3)
# test model efficiency

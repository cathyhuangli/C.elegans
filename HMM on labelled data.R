####### hidden markov model try out
library(car)
data=N2_nf4
#data=N2_nf5
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow','Forward-Sharp')='Forward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
data$Kim<-recode(data$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")

# Use HMM
# Create model, 2 states: either going up or going down
library(depmixS4)
ret=data$SkewerAngle
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), nstates=2)
f <- fit(mod)
summary(f)
########################
## MULTIVARIATE HMM
library(depmixS4)
ret=data$SkewerAngle
ret1=data$Area
ret2=data$ComptFactor
ret3=data$ComptFactor
mod <- depmix(list(ret ~ 1,ret1 ~ 1,ret2 ~ 1), data = data.frame(ret=ret,ret1=ret1,ret1=ret2), nstates = 3,
              family = list(gaussian(), gaussian(),gaussian()),instart = runif(3))
f <- fit(mod)
#########################
# Get the estimated state for each timestep 
esttrans <- posterior(f)
unique(esttrans[,1])
# Plot
par(mfrow=c(3,1))
plot(data$ElapsedTime, data$Kim, type='l', main='Trajectory')
plot(data$ElapsedTime, esttrans[,1], type='l', main='Estimated state')
plot(data$ElapsedTime, ret, type='l', main='Returns')


s=data$ElapsedTime
t=data$ElapsedTime[-nrow(data)]
########### multivariate HMM
mod <- depmix(list(rt ~ 1,corr ~ 1), data = speed, nstates = 2,
              + family = list(gaussian(), multinomial("identity")),
              + transition = ~ scale(Pacc), instart = runif(2))
###################### try PCA then HMM##################
library(car)
data=N2_nf4
#data=N2_nf5
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow','Forward-Sharp')='Forward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
data$Kim<-recode(data$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")

data=data[,which(names(data) %in% c('Kim','ElapsedTime',  'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle'))]
### PCA
ds <- data[, -7]


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ds.pca <- prcomp(ds,
                 center = TRUE,
                 scale. = TRUE) 
ds.pca$x
print(ds.pca)
plot(ds.pca, type = "l")
loadings = ds.pca$x[,1:3]
df1 <- as.data.frame(loadings)
newdf1=cbind(data$Kim,df1)
colnames(newdf1)[1] <- "Kim"
### now run HMM on the PCA component
### one componeng
library(depmixS4)
ret=newdf1$PC1
ret1=newdf1$PC2
ret2=newdf1$PC3
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), nstates=3)
### three component
mod <- depmix(list(ret ~ 1,ret1 ~ 1,ret2 ~ 1),data = data.frame(ret=ret,ret1=ret1,ret1=ret2), nstates = 3,
              ,family = list(gaussian(), gaussian(),gaussian()), instart = runif(3))



#############
f <- fit(mod)
summary(f)
esttrans <- posterior(f)
unique(esttrans[,1])
# Plot
par(mfrow=c(3,1))
plot(data$ElapsedTime, data$Kim, type='l', main='Trajectory')
plot(data$ElapsedTime, esttrans[,1], type='l', main='Estimated state')
plot(data$ElapsedTime, ret, type='l', main='Returns')


# build hmm from n2nf4
summary(n2nf4allf$FrameNum)
summary(movementFeatures_n2nf4$Frame)
n2nf4allf$ElapsedTime=format(round(n2nf4allf$ElapsedTime, 3), nsmall = 3)
n2nf4allf$ElapsedTime=as.numeric(n2nf4allf$ElapsedTime)
#newds=n2nf4allf[n2nf4allf$ElapsedTime %in% movementFeatures_n2nf4$TimeElapsed,]
#newds1=movementFeatures_n2nf4[movementFeatures_n2nf4$TimeElapsed %in% n2nf4allf$ElapsedTime,]

n2nf4allf[n2nf4allf$FrameNum==80790,]
movementFeatures_n2nf4[movementFeatures_n2nf4$Frame==80790,]
# tried: Ixx','ComptFactor','Length','Area',
df=n2nf4allf[,c('ElapsedTime','Elongation','Perimeter','MinorAxisLength','RectBigSide','MajorAxisLength','Kim'), drop=FALSE]
#df=n2nf4allf[n2nf4allf$SkewerAngle<=270,c('ElapsedTime','SktAmpRatio','TrackAmplitude','MajorAxisLength','SkewerAngle','Kim'), drop=FALSE]

df[df==""] <- NA
df=na.omit(df)
library(car)
df$Kim<-recode(df$Kim,"c('Forward-NTD','Forward-Shallow')='Forward'")
df$Kim<-recode(df$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
summary(df$Posture)
unique(df$Kim)
summary(df$ElapsedTime)
#df$ElapsedTime=as.numeric(df$ElapsedTime)
df$sapl=1:nrow(df)
vec=1
for(i in 1:2185){
  c_inx <- which(abs(df[,1]-i) == min(abs(df[,1]-i)))
  if (length(c_inx)>1){c_inx=c_inx[1]}
  vec<- c(vec,c_inx)
}
sapl_df <- df[df$sapl %in% vec, ]
sapl_df$time<-seq.int(nrow(sapl_df))
plot(df$Posture,df$Kim)
plot(sapl_df$Posture,sapl_df$Kim)

# add speed from movementfeature, note the frame problem
newds=movementFeatures_n2nf4[movementFeatures_n2nf4$TimeElapsed %in% sapl_df$ElapsedTime,]
#newds$MaxWidth=as.numeric(newds$MaxWidth)
summary(newds)
library(modeest)
#MaxWidth','Heywood','Hydraulic','GblCentroidRow

mlv(newds$Speed, method = "mfv") # mode for speed is 169.236(=mean)
mlv(newds$Angle, method = "mfv") # mode for angle is 0
mlv(newds$Angular_Velocity, method = "mfv")
a=sapl_df$ElapsedTime
b=newds$TimeElapsed
#a[a %in% b]
sub1=sapl_df[!(a %in% b),]
#newds[!(b %in% a),]
sub1$speed=176.200 # use median
sub1$angle=0
sub1$Angular_Velocity=1.534075 
sub2=sapl_df[(a %in% b),]
#sub2[with(sub2, order(sub2$ElapsedTime)), ]==sub2
#newds<-newds[!(newds$Frame==20453),]
sub2$speed=newds$Speed
sub2$angle=newds$Angle
sub2$Angular_Velocity=newds$Angular_Velocity
sapl_df_new=rbind(sub1,sub2)
sapl_df_new=sapl_df_new[with(sapl_df_new, order(sapl_df_new$ElapsedTime)), ]

#############
# new sequence of observation from n2nf5
dfs=n2nf5allf[,c('ElapsedTime','Posture','Kim'), drop=FALSE]
dfs[dfs==""] <- NA
dfs=na.omit(dfs)
library(car)
dfs$Kim<-recode(dfs$Kim,"c('Forward-NTD','Forward-Shallow')='Forward'")
dfs$Kim<-recode(dfs$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
summary(dfs$Posture)
unique(dfs$Kim)
summary(dfs$ElapsedTime)
dfs$sapl=1:nrow(dfs)
vec=1
for(i in 1:1137){
  c_inx <- which(abs(dfs[,1]-i) == min(abs(dfs[,1]-i)))
  if (length(c_inx)>1){c_inx=c_inx[1]}
  vec<- c(vec,c_inx)
}
sapl_dfs <- dfs[dfs$sapl %in% vec, ]
sapl_dfs$time<-seq.int(nrow(sapl_dfs))
plot(dfs$Posture,dfs$Kim)
plot(sapl_dfs$Posture,sapl_dfs$Kim)



library(HMM)
inihmm=initHMM(sapl_df$Kim, sapl_df$Posture, startProbs=NULL, transProbs=NULL, emissionProbs=NULL)
logForwardProbabilities = forward(inihmm,sapl_dfs$Posture)
print(exp(logForwardProbabilities))



head(dfs)
ret=sapl_df$MajorAxisLength
#ret1=sapl_df$Posture
ret1=sapl_df$MaxWidth
ret2=sapl_df$Heywood
#ret2=sapl_df$Posture
ret3=sapl_df$Hydraulic
ret4=sapl_df$GblCentroidRow
summary(ret)
summary(ret1)
library(depmixS4)
set.seed(1)
# for 1 varaible
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), nstates=5,trstart=runif(25))
f <- fit(mod)
esttrans <- posterior(f)
table(esttrans[,1],sapl_df$Kim)

# for 3 variable  family=list(gaussian(),multinomial())
set.seed(1)
mod <- depmix(list(ret~1,ret1~1,ret2~1), data=sapl_df, nstates=2,
              family=list(gaussian(),multinomial(),gaussian()),
              trstart=runif(4))
# for 2 variables
set.seed(1)
mod <- depmix(list(ret~1,ret1~1,ret3~1,ret4~1), data=sapl_df, nstates=5,
              family=list(gaussian(),gaussian(),gaussian(),gaussian()),trstart=runif(25))
f <- fit(mod)
esttrans <- posterior(f)
table(esttrans[,1],sapl_df$Kim)

### for sapl_df_new ################################
head(dfs)
ret=sapl_df_new$MajorAxisLength
#ret1=sapl_df_new$Posture
#ret=sapl_df_new$angle
#'Elongation','Perimeter','MinorAxisLength','RectBigSide', Ixx, Length, ComptFactor, Area, 'MaxWidth','Heywood','Hydraulic','GblCentroidRow'
ret1=sapl_df_new$MinorAxisLength
ret2=sapl_df_new$speed
#ret=sapl_df_new$RectBigSide
#ret3=sapl_df_new$SkewerAngle
#ret4=sapl_df_new$TrackAmplitude
summary(ret)
summary(ret1)
library(depmixS4)
set.seed(1)
# for 1 varaible
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), nstates=5,trstart=runif(25))
f <- fit(mod)
esttrans <- posterior(f)
table(esttrans[,1],sapl_df_new$Kim)

# for 3 variable  family=list(gaussian(),multinomial())

set.seed(1)
mod <- depmix(list(ret~1,ret1~1,ret2~1), data=sapl_df_new, nstates=5,
              family=list(gaussian(),gaussian(),gaussian()),
              trstart=runif(25))
f <- fit(mod)
esttrans <- posterior(f)
table(esttrans[,1],sapl_df_new$Kim)
# for 2 variables
set.seed(1)
mod <- depmix(list(ret~1,ret2~1), data=sapl_df_new, nstates=5,
              family=list(gaussian(),gaussian()),trstart=runif(25))
f <- fit(mod)
esttrans <- posterior(f)
table(esttrans[,1],sapl_df_new$Kim)
summary(f)








library(depmixS4)
data(speed)
set.seed(1)
mod <- depmix(rt~1, data=speed, nstates=2, trstart=runif(4))
fm <- fit(mod)
fm
set.seed(1)
mod <- depmix(list(rt~1,corr~1), data=speed, nstates=2,
                family=list(gaussian(),multinomial()),
                transition=~scale(Pacc),instart=runif(2))
fm <- fit(mod)


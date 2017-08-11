

#df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('Kim','ElapsedTime','ComptFactor','Hydraulic','SkewerAngle','Length','CurvHead','CurvTail')]

# TRY one variable first:
df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('SkewerAngle'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),ds.q1=numeric(0),ds.q3=numeric(0),start_row=numeric(0))

for (i in 1:98){
  #i=1
  start=25*(i-1)+1
  end=25*i
  ds=df[start:end,, drop=FALSE]
  ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.q1=quantile(ds$SkewerAngle, 0.25)
  ds.q3=quantile(ds$SkewerAngle, 0.75)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  newdf[nrow(newdf) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.q1,ds.q3,i)
}
newdf$Label='n2nf4_Rshort'


###### now process the testing one:N2_nf5
df1=N2_nf5[,c('SkewerAngle','Kim'), drop=FALSE]
summary(N2_nf5$Kim)
df1=na.omit(df1)
#df=df[complete.cases(df),]
##### one way is to literate throught table by moving window one row only***************
newdf1 <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),ds.q1=numeric(0),ds.q3=numeric(0),start_row=numeric(0))

for (i in 1:1668){
  #i=1
  start=(i-1)*25+1
  end=i*25
  ds=df1[start:end,, drop=FALSE]
  ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.q1=quantile(ds$SkewerAngle, 0.25)
  ds.q3=quantile(ds$SkewerAngle, 0.75)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  newdf1[nrow(newdf1) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.q1,ds.q3,i)
}
newdf1$Label='n2nf5'


normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}


ts=rbind(newdf,newdf1)
ts.norm=normalized(ts[,1:6])
k.f = 2 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.ts<-kmeans(ts.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(ts,cluster=factor(km.ts$cluster))
ss1=df.f.labels[df.f.labels$cluster==1, ]
unique(ss1$Label)
ss2=df.f.labels[df.f.labels$cluster==2, ]
unique(ss2$Label)
ss1sub=ss1[ss1$Label=='n2nf5',]
summary(ss1sub$start_row)


##### anotherway is to literate throught table by every 25 frames***************
df1=N2_nf5[,c('SkewerAngle','Kim'), drop=FALSE]
#summary(N2_nf5$Kim)
df1=na.omit(df1)
#df=df[complete.cases(df),]
newdf1 <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),ds.q1=numeric(0),ds.q3=numeric(0),start_row=numeric(0))

for (i in 1:1668){
  #i=1
  start=(i-1)*25+1
  end=i*25
  ds=df1[start:end,, drop=FALSE]
  ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.q1=quantile(ds$SkewerAngle, 0.25)
  ds.q3=quantile(ds$SkewerAngle, 0.75)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  newdf1[nrow(newdf1) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.q1,ds.q3,i)
}
newdf1$Label='n2nf5'


normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}


ts=rbind(newdf,newdf1)
ts.norm=normalized(ts[,1:6])
k.f = 2 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.ts<-kmeans(ts.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(ts,cluster=factor(km.ts$cluster))
ss1=df.f.labels[df.f.labels$cluster==1, ]
unique(ss1$Label)
ss2=df.f.labels[df.f.labels$cluster==2, ]
unique(ss2$Label)
ss2sub=ss2[ss2$Label=='n2nf5',]
summary(ss2sub$start_row)

write.csv(df.f.labels,file=outfile)


######################## it seems using skewrangle alone is not enough to distinguish. add other varibles inside
df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('Kim','ElapsedTime','ComptFactor','Hydraulic','SkewerAngle','Length','CurvHead','CurvTail')]

# TRY two variable now:
df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('SkewerAngle','Hydraulic'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),ds.q1=numeric(0),ds.q3=numeric(0),
                    ds.min1=numeric(0),ds.max1=numeric(0),ds.mean1=numeric(0),ds.var1=numeric(0),ds.q11=numeric(0),ds.q31=numeric(0)
                    ,start_row=numeric(0))

for (i in 1:98){
  i=1
  start=25*(i-1)+1
  end=25*i
  ds=df[start:end,, drop=FALSE]
  #ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.q1=quantile(ds$SkewerAngle, 0.25)
  ds.q3=quantile(ds$SkewerAngle, 0.75)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  ds.min1=min(ds$Hydraulic)
  ds.q11=quantile(ds$Hydraulic, 0.25)
  ds.q31=quantile(ds$Hydraulic, 0.75)
  ds.max1=max(ds$Hydraulic)
  ds.mean1=mean(ds$Hydraulic)
  ds.var1=var(ds$Hydraulic)
  newdf[nrow(newdf) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.q1,ds.q3,ds.min1,ds.max1,
                              ds.mean1,ds.var1,ds.q11,ds.q31,i)
}
newdf$Label='n2nf4_Rshort'

####### use the same two variables for testing data n2nf5###################

# TRY two variable now:
df=N2_nf5[,c('SkewerAngle','Hydraulic'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf1 <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),ds.q1=numeric(0),ds.q3=numeric(0),
                    ds.min1=numeric(0),ds.max1=numeric(0),ds.mean1=numeric(0),ds.var1=numeric(0),ds.q11=numeric(0),ds.q31=numeric(0)
                    ,start_row=numeric(0))

for (i in 1:1668){
 i=1
 start=25*(i-1)+1
 end=25*i
#for (i in 1:41690){
    #i=1
#  start=i
#  end=i+24
  ds=df[start:end,, drop=FALSE]
  #ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.q1=quantile(ds$SkewerAngle, 0.25)
  ds.q3=quantile(ds$SkewerAngle, 0.75)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  ds.min1=min(ds$Hydraulic)
  ds.q11=quantile(ds$Hydraulic, 0.25)
  ds.q31=quantile(ds$Hydraulic, 0.75)
  ds.max1=max(ds$Hydraulic)
  ds.mean1=mean(ds$Hydraulic)
  ds.var1=var(ds$Hydraulic)
  newdf1[nrow(newdf1) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.q1,ds.q3,ds.min1,ds.max1,
                              ds.mean1,ds.var1,ds.q11,ds.q31,i)
}
newdf1$Label='n2nf5'

normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}


ts=rbind(newdf,newdf1)
ts.norm=normalized(ts[,1:12])
k.f = 2 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.ts<-kmeans(ts.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(ts,cluster=factor(km.ts$cluster))
ss1=df.f.labels[df.f.labels$cluster==1, ]
unique(ss1$Label)
ss2=df.f.labels[df.f.labels$cluster==2, ]
unique(ss2$Label)
ss1sub=ss1[ss1$Label=='n2nf5',]
summary(ss1sub$start_row)
ss2sub=ss2[ss2$Label=='n2nf5',]
summary(ss2sub$start_row)

############# it seems two variables is not eonough, but do we really need to generate so many variables?
### q1, q3 has high correlation with 
df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('Kim','ElapsedTime','ComptFactor','Hydraulic','SkewerAngle','Length','CurvHead','CurvTail')]

# TRY three variable now and delete 2 generated variables: q1, q3
df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('SkewerAngle','Hydraulic','ElapsedTime'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),
                    ds.min1=numeric(0),ds.max1=numeric(0),ds.mean1=numeric(0),ds.var1=numeric(0),
                    ds.min2=numeric(0),ds.max2=numeric(0),ds.mean2=numeric(0),ds.var2=numeric(0),
                     start_row=numeric(0))

for (i in 1:98){
  start=25*(i-1)+1
  end=25*i
  ds=df[start:end,, drop=FALSE]
  #ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  ds.min1=min(ds$Hydraulic)
  ds.max1=max(ds$Hydraulic)
  ds.mean1=mean(ds$Hydraulic)
  ds.var1=var(ds$Hydraulic)
  ds.min2=min(ds$ElapsedTime)
  ds.max2=max(ds$ElapsedTime)
  ds.mean2=mean(ds$ElapsedTime)
  ds.var2=var(ds$ElapsedTime)
  newdf[nrow(newdf) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.min1,ds.max1,
                              ds.mean1,ds.var1,ds.min2,ds.max2,
                              ds.mean2,ds.var2,i)
}
newdf$Label='n2nf4_Rshort'
#cor(newdf[,1:12])
#df=N2_nf5[N2_nf5$Kim=='Backward-ReverseShort',c('Kim','ElapsedTime','ComptFactor','Hydraulic','SkewerAngle','Length','CurvHead','CurvTail')]

# TRY three variable now and delete 2 generated variables: q1, q3
df=N2_nf5[,c('SkewerAngle','Hydraulic','ElapsedTime'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf1 <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),
                    ds.min1=numeric(0),ds.max1=numeric(0),ds.mean1=numeric(0),ds.var1=numeric(0),
                    ds.min2=numeric(0),ds.max2=numeric(0),ds.mean2=numeric(0),ds.var2=numeric(0),
                    start_row=numeric(0))

for (i in 1:1668){
  #i=2
  start=25*(i-1)+1
  end=25*i
  ds=df[start:end,, drop=FALSE]
  #ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  ds.min1=min(ds$Hydraulic)
  ds.max1=max(ds$Hydraulic)
  ds.mean1=mean(ds$Hydraulic)
  ds.var1=var(ds$Hydraulic)
  ds.min2=min(ds$ElapsedTime)
  ds.max2=max(ds$ElapsedTime)
  ds.mean2=mean(ds$ElapsedTime)
  ds.var2=var(ds$ElapsedTime)
  newdf1[nrow(newdf1) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.min1,ds.max1,
                              ds.mean1,ds.var1,ds.min2,ds.max2,
                              ds.mean2,ds.var2,i)
}
newdf1$Label='n2nf5'
#cor(newdf[,1:12])

##### combine, cluster and compare

ts=rbind(newdf,newdf1)
ts.norm=normalized(ts[,1:12])
k.f = 2 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.ts<-kmeans(ts.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(ts,cluster=factor(km.ts$cluster))
ss1=df.f.labels[df.f.labels$cluster==1, ]
unique(ss1$Label)
ss2=df.f.labels[df.f.labels$cluster==2, ]
unique(ss2$Label)
ss1sub=ss1[ss1$Label=='n2nf5',]
summary(ss1sub$start_row)
ss2sub=ss2[ss2$Label=='n2nf5',]
summary(ss2sub$start_row)


############# try selected two variables without q1, q3:#####################
#####****************************
df=N2_nf4[N2_nf4$Kim=='Backward-ReverseShort',c('SkewerAngle','Hydraulic'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),
                    ds.min1=numeric(0),ds.max1=numeric(0),ds.mean1=numeric(0),ds.var1=numeric(0),
                    start_row=numeric(0))

for (i in 1:98){
  start=25*(i-1)+1
  end=25*i
  ds=df[start:end,, drop=FALSE]
  #ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  ds.min1=min(ds$Hydraulic)
  ds.max1=max(ds$Hydraulic)
  ds.mean1=mean(ds$Hydraulic)
  ds.var1=var(ds$Hydraulic)

  newdf[nrow(newdf) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.min1,ds.max1,
                              ds.mean1,ds.var1,i)
}
newdf$Label='n2nf4_Rshort'
#cor(newdf[,1:12])
#df=N2_nf5[N2_nf5$Kim=='Backward-ReverseShort',c('Kim','ElapsedTime','ComptFactor','Hydraulic','SkewerAngle','Length','CurvHead','CurvTail')]

# TRY three variable now and delete 2 generated variables: q1, q3
df=N2_nf5[,c('SkewerAngle','Hydraulic','ElapsedTime'), drop=FALSE]
df=na.omit(df)
#df=df[complete.cases(df),]

newdf1 <- data.frame(ds.min=numeric(0),ds.max=numeric(0),ds.mean=numeric(0),ds.var=numeric(0),
                     ds.min1=numeric(0),ds.max1=numeric(0),ds.mean1=numeric(0),ds.var1=numeric(0),
                     start_row=numeric(0))

for (i in 1:1668){
  #i=2
  start=25*(i-1)+1
  end=25*i
  ds=df[start:end,, drop=FALSE]
  #ds.fivenum=summary(ds$SkewerAngle)
  ds.min=min(ds$SkewerAngle)
  ds.max=max(ds$SkewerAngle)
  ds.mean=mean(ds$SkewerAngle)
  ds.var=var(ds$SkewerAngle)
  ds.min1=min(ds$Hydraulic)
  ds.max1=max(ds$Hydraulic)
  ds.mean1=mean(ds$Hydraulic)
  ds.var1=var(ds$Hydraulic)
  newdf1[nrow(newdf1) + 1,] = c(ds.min,ds.max,ds.mean,ds.var,ds.min1,ds.max1,
                                ds.mean1,ds.var1,i)
}
newdf1$Label='n2nf5'
#cor(newdf[,1:12])

##### combine, cluster and compare

ts=rbind(newdf,newdf1)
ts.norm=normalized(ts[,1:8])
k.f = 2 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.ts<-kmeans(ts.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(ts,cluster=factor(km.ts$cluster))
ss1=df.f.labels[df.f.labels$cluster==1, ]
unique(ss1$Label)
ss2=df.f.labels[df.f.labels$cluster==2, ]
unique(ss2$Label)
ss1sub=ss1[ss1$Label=='n2nf5',]
summary(ss1sub$start_row)
ss2sub=ss2[ss2$Label=='n2nf5',]
summary(ss2sub$start_row)


check=N2_nf5[N2_nf5$Kim=='Backward-ReverseShort',c('ElapsedTime','Hydraulic','SkewerAngle')]

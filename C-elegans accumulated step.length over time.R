ds=all_data_20160217
plot(ds$Step.length.in.mm,ds$Time.Elapsed)
with(ds,summary(v_name))
#n2_f=4; n2_nf=33; n2_nnf=2; tph1_f=13; tph1_nf=5
######################################
########CUMULATE STEP LENGTH WITHIN EACH VIDEO
### N2_f
par(mfrow = c(3, 2))
newds1 <- ds[0,]
for (i in 1:4){
  vname=sub("^", "N2_f", i )
  #print(vname)
  dssub=ds[ds$v_name==vname,]
  dssub[,12] <- cumsum(dssub[,4])
  newds1=rbind(newds1,dssub)
}
colnames(newds1)[12] <- "accumulated_step"
with(newds1, plot(Time.Elapsed,accumulated_step,main='N2_f',col=v_name,xlim=c(0,5000),ylim=c(0,800)))
### N2_nf
newds2 <- ds[0,]
for (i in 1:33){
  vname=sub("^", "N2_nf", i )
  #print(vname)
  dssub=ds[ds$v_name==vname,]
  dssub[,12] <- cumsum(dssub[,4])
  newds2=rbind(newds2,dssub)
}

colnames(newds2)[12] <- "accumulated_step"
with(newds2, plot(Time.Elapsed,accumulated_step,main='N2_nf',col=v_name,xlim=c(0,5000),ylim=c(0,800)))
###N2_nnf
#newds <- ds[0,]
#for (i in 1:2){
#  vname=sub("^", "N2_nnf", i )
#  #print(vname)
#  dssub=ds[ds$v_name==vname,]
#  dssub[,12] <- cumsum(dssub[,4])
#  newds=rbind(newds,dssub)
#}
#colnames(newds)[12] <- "accumulated_step"
#with(newds, plot(Time.Elapsed,accumulated_step,main='N2_nnf',col=v_name,xlim=c(0,5000),ylim=c(0,800)))


### tph1_f
newds3 <- ds[0,]
for (i in 1:13){
  vname=sub("^", "tph1_f", i )
  #print(vname)
  dssub=ds[ds$v_name==vname,]
  dssub[,12] <- cumsum(dssub[,4])
  newds3=rbind(newds3,dssub)
}
colnames(newds3)[12] <- "accumulated_step"
with(newds3, plot(Time.Elapsed,accumulated_step,main='tph1_f',col=v_name,xlim=c(0,5000),ylim=c(0,800)))
### tph1_nf
newds4 <- ds[0,]
for (i in 1:5){
  vname=sub("^", "tph1_nf", i )
  #print(vname)
  dssub=ds[ds$v_name==vname,]
  #with(dssub,plot(Time.Elapsed,Speed.in.mm.per.sec,main=vname))
  dssub[,12] <- cumsum(dssub[,4])
  newds4=rbind(newds4,dssub)
}
colnames(newds4)[12] <- "accumulated_step"
with(newds4, plot(Time.Elapsed,accumulated_step,main='tph1_nf',col=v_name,xlim=c(0,5000),ylim=c(0,800)))

###egl19_f=3
newds5 <- ds[0,]
for (i in 1:3){
  vname=sub("^", "egl19_f", i )
  #print(vname)
  dssub=ds[ds$v_name==vname,]
  #with(dssub,plot(Time.Elapsed,Speed.in.mm.per.sec,main=vname))
  dssub[,12] <- cumsum(dssub[,4])
  newds5=rbind(newds5,dssub)
}
colnames(newds5)[12] <- "accumulated_step"
with(newds5, plot(Time.Elapsed,accumulated_step,main='egl19_f',col=v_name,xlim=c(0,5000),ylim=c(0,800)))


###egl19_nf=2
newds6 <- ds[0,]
for (i in 1:2){
  vname=sub("^", "egl19_nf", i )
  #print(vname)
  dssub=ds[ds$v_name==vname,]
  #with(dssub,plot(Time.Elapsed,Speed.in.mm.per.sec,main=vname))
  dssub[,12] <- cumsum(dssub[,4])
  newds6=rbind(newds6,dssub)
}
colnames(newds6)[12] <- "accumulated_step"
with(newds6, plot(Time.Elapsed,accumulated_step,main='egl19_nf',col=v_name,xlim=c(0,5000),ylim=c(0,800)))
################ acculated all videos now
par(mfrow = c(1, 1))
accuds=rbind(newds1,newds2,newds3,newds4,newds5,newds6)
with(accuds[accuds$type=="N2_f",], plot(Time.Elapsed,accumulated_step, type="l", col="red",xlim = c(0, 5000)))
par(new=TRUE)
with(accuds[accuds$type=="N2_nf",], plot(Time.Elapsed,accumulated_step, type="l", col="green",xlim = c(0, 5000)))
par(new=TRUE)
with(accuds[accuds$type=="tph1_f",], plot(Time.Elapsed,accumulated_step, type="l", col="purple",xlim = c(0, 5000)))
par(new=TRUE)
with(accuds[accuds$type=="tph1_nf",], plot(Time.Elapsed,accumulated_step, type="l", col="black",xlim = c(0, 5000)))



with(ds[ds$type=="tph1_f",], plot(Time.Elapsed,Step.length.in.mm))
with(ds[ds$type=="tph1_nf",], plot(Time.Elapsed,Step.length.in.mm))
with(ds[ds$type=="N2_f",], plot(Time.Elapsed,Step.length.in.mm))
with(ds[ds$type=="N2_nf",], plot(Time.Elapsed,Step.length.in.mm))

#########################################################

with(ds[ds$type=="tph1_f",], plot(Time.Elapsed,Step.length.in.mm))
with(ds[ds$type=="tph1_nf",], plot(Time.Elapsed,Step.length.in.mm))
with(ds[ds$type=="N2_f",], plot(Time.Elapsed,Step.length.in.mm))
with(ds[ds$type=="N2_nf",], plot(Time.Elapsed,Step.length.in.mm))

ds[,12] <- cumsum(ds[,4])
ds1=ds[ds$type=="tph1_f",]
ds2=ds[ds$type=="N2_f",]

with(ds[ds$type=="tph1_f",], plot(Time.Elapsed/10000,V12/10000,type="l", col="red"))
par(new=TRUE)
with(ds[ds$type=="N2_nf",], plot(Time.Elapsed/10000,V12/10000,type="l", col="green"))
par(new=TRUE)
with(ds[ds$type=="tph1_nf",], plot(Time.Elapsed,V12,type="l", col="black"))
par(new=TRUE)
with(ds[ds$type=="N2_f",], plot(Time.Elapsed,V12,type="l", col="purple"))

#get max 4737
install.packages("ggplot2")
library(ggplot2)
tapply(ds$Time.Elapsed, ds$type, summary)
tapply(ds$V12, ds$type, summary)
with(ds[ds$type=="tph1_f1",], plot(V12,Time.Elapsed, type="l", col="red"))
par(new=TRUE)
with(ds[ds$type=="N2_nf",], plot(V12,Time.Elapsed, type="l", col="green"))
par(new=TRUE)
with(ds[ds$type=="N2_f",], plot(Time.Elapsed,V12, type="l", col="purple"))
par(new=TRUE)
with(ds[ds$type=="tph1_nf",], plot(Time.Elapsed,V12, type="l", col="black"))


with(ds[ds$type=="tph1_f",], plot(Time.Elapsed,V12, type="l", col="red",xlim = c(0, 5000)))
par(new=TRUE)
with(ds[ds$type=="N2_nf",], plot(Time.Elapsed,V12, type="l", col="green",xlim = c(0, 5000)))
par(new=TRUE)
with(ds[ds$type=="N2_f",], plot(Time.Elapsed,V12, type="l", col="purple",xlim = c(0, 5000)))
par(new=TRUE)
with(ds[ds$type=="tph1_nf",], plot(Time.Elapsed,V12, type="l", col="black",xlim = c(0, 5000)))


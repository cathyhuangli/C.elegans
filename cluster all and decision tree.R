#First, for the redo the on/off food all file, adding the "time elapsed" as one attribute
# later add cluster back to this file, easy for the encoding part.


# add on food all & off food all into one file: "allofdata.csv"





#define cluster number for all as 2:5 and add back to the file.
s<-cbind(normalized(allofdata$Angle.between.2.step.lengths),normalized(allofdata$Step.length.in.mm))
t<-cbind(s,normalized(allofdata$Speed.in.mm.per.sec))
allofdata.norm<-data.frame(t)
library(reshape)
allofdata.norm<-rename(allofdata.norm,c(X1="Angle.between.2.step.lengths", X2="Step.length.in.mm",X3="Speed.in.mm.per.sec"))

for (i in 2:5){
  k.f = i # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
  km.allofdata<-kmeans(allofdata.norm[,1:3],k.f,iter.max = 1000,nstart = 25)
  df.f.labels = cbind(allofdata,cluster=factor(km.allofdata$cluster))
  outfile=paste("allofdata.assigned", i, ".csv",sep = "")
  write.csv(turningAngletph1_f1,file=outfile)
}


##go through the four files, and use decision tree to decide which cluster number is the best:
#install.packages("tree")
path = "allofdata.csv"
file.names <- dir(path, pattern =".csv")#same as list.files()
for(i in 1:length(file.names)){
  t <- read.table(file.names[i],header=T)
  library(tree)
  attach(t)
  set.seed(2)
  train=sample(1:nrow(t),nrow(t)/2)#using half of dataset for training
  test=-train
  training_data=t[train,]
  testing_data=t[test,]
  testing_cluster=cluster[test]
  #fit the tree model using training data
  tree_model=tree(cluster~.,training_data)
  plot(tree_model)
  text(tree_model,pretty=0)#pretty=0 means categorical value
  #check how the tree doing using testing data
  tree_pred=predict(tree_model,testing_data,type="class")
  st=mean(tree_pred==testing_cluster)
  # write to file
  k=length(unique(cluster))
  print("The presion of prediction for cluster",k,"is: ", st )
  detach(t)
}


#sort the data according to first by v-name, then by time-elapsed.
#eg. smallData[order(smallData$value, smallData$name),]#change order
#eg. smallData[with(smallData, order(value, name)),]#rewrite the table.

allofdata[with(allofdata,order(v_name,Time.Elapsed))]#rewrite the data

#run-length encoding:
#http://www.r-bloggers.com/r-function-of-the-day-rle/
allofdata.rle <- rle(allofdata)
sort(allofdata.rle$lengths, decreasing = TRUE)

video<-list(unique(allofdata$v_name))
for v in video:
  df<-allofdata[allofdata$v_name==v,]
  df.rle<-rle(df)
  #ls <- list(value=df.rle$lengths, length=df.rle$values) 
  #df <- as.data.frame(ls) 
  pos<-cumsum(df.rle$lengths)
  df_pos<-[unlist(pos),]#take the ones marked at lengths.
  df_pos$value<-df.rle$values
  df_pos$value<-df.rle$lengths
  write.csv(df_pos,file=str(v+"rln")
#as.numeric(rownames(df))=ROW NUMBER

# then make the list of datasets into one.





































install.packages("tree")
library(tree)
attach(carseat)
set.seed(2)
train=sample(1:nrow(carseat),nrow(carseat)/2)#using half of dataset for training
test=-train
training_data=carseat[train,]
testing_data=carseat[test,]
testing_cluster=cluster[test]
#fit the tree model using training data
tree_model=tree(cluster~.,training_data)
plot(tree_model)
text(tree_model,pretty=0)#pretty=0 means categorical value
#check how the tree doing using testing data
tree_pred=predict(tree_model,testing_data,type="class")
#table(tree.pred,testing_cluster)
mean(tree_pred==testing_cluster)#give precision of prediction
#mean(tree_pred!=testing_cluster)#give misprediction









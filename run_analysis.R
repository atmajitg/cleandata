#######################################################
### Load the necessary Library
#######################################################
library(dplyr)
#######################################################
### Get the train data and label it
#######################################################
train.data = read.table("./data/train/X_train.txt", header=FALSE) # stats data
features = read.table("./data/features.txt", header = FALSE) # column labels
colnames(train.data)= t(features[,2])
activity.train = read.table("./data/train/y_train.txt", header=FALSE) # activity labels
activity.label = read.table("./data/activity_labels.txt", header=FALSE)
colnames(activity.train)="activity.nbr"
colnames(activity.label)=c("acivity.nbr","activity.name")
subject.train = read.table("./data/train/subject_train.txt", header=FALSE)
colnames(subject.train)="subjects"
train.data.all = cbind(activity.train,subject.train,train.data)
#######################################################
### Get the test data and label it
#######################################################
test.data = read.table("./data/test/X_test.txt", header=FALSE)
activity.test = read.table("./data/test/y_test.txt", header=FALSE)
colnames(test.data)= t(features[,2])
colnames(activity.test)="activity.nbr"
subject.test = read.table("./data/test/subject_test.txt", header=FALSE)
colnames(subject.test)="subjects"
test.data.all = cbind(activity.test,subject.test,test.data)

#######################################################
### Merging the data
#######################################################
all = rbind(train.data.all, test.data.all)

#######################################################
### extracting the labels the data
#######################################################
lbl.mn = grepl("mean..", features[,2])
ftr= features[lbl.mn,]
lbl.sd = grepl("std..", features[,2])
ftr2= features[lbl.sd,]
lbl.mn.pstn = (ftr[,1])+2
lbl.sd.pstn=(ftr2[,1])+2
select.all1 = all[,1:2]
select.all2 = all[,lbl.mn.pstn[]]
select.all3 = all[,lbl.sd.pstn[]]
select.all = cbind(select.all1,select.all2,select.all3)
#######################################################
### Adding descriptive names for activity
#######################################################

activity= c(rep(NA, nrow(select.all)))
for(i in 1:nrow(select.all)){
if(select.all[i,1]==1){
  activity[i] = "WALKING"
}else if(select.all[i,1]==2){
  activity[i] = "WALKING_UPSTAIRS"
}else if(select.all[i,1]==3){
  activity[i]= "WALKING_DOWNSTAIRS"
}else if(select.all[i,1]==4){
  activity[i] = "SITTING"
}else if(select.all[i,1]==5){
 activity[i] = "STANDING" 
}else{
  activity[i] ="LAYING"
}
}
names(activity)=c("activity.name")
select.all=cbind(activity,select.all)

#######################################################
### Adding descriptive labels for activity
#######################################################
edit = colnames(select.all)
edit = gsub("[\\(\\)]", "",edit)
edit = gsub("-", ".", edit)
edit = sub("std", "stddvtn",edit)
edit = sub("Acc", "Accelarationlrtn",edit)
edit = sub("Gyro", "Gyroscope",edit)
edit = sub("^t", "Time",edit)
edit = sub("^f", "frequency",edit)
edit = sub("X$", "Xcordinate",edit)
edit = sub("Y$", "Ycordinate",edit)
edit = sub("Z$", "Zcordinate",edit)
edit = sub("Mag", "Magnitude",edit)
edit = sub("meanFreq", "meanfrequency",edit)
edit = sub("BodyBody", "Body",edit)
colnames(select.all)= t(edit)

#######################################################
### tidy data
#######################################################
grpd.data = group_by(select.all,subjects, activity)
tidy.data =summarise_each(grpd.data,funs(mean))
write.table(tidy.data, "tidy_data.txt", sep=",", col.names = TRUE, row.names = FALSE)

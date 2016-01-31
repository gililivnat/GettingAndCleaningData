#######################################################################################

## Getting and Cleaning Data Course Project
## Gilad Livnat
## 31-01-2016

# Description:
# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#######################################################################################

#Clean workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.

# set working directory
setwd("C:/Users/glivnat/Google Drive/Courses/Getting and cleaning data/project/UCI HAR Dataset")

# read data
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
head(features)
head(activityType)

#read data - train
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
head(subjectTrain)
head(xTrain)

#Assign column names to data
colnames(activityType)  = c('activityId','activityType')

#Assign columns - train
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

#read data - test
subjectTest = read.table('./test/subject_test.txt',header=FALSE) #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE) #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE) #imports y_test.txt

#Assign columns - test
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"

#Merge
trainingData = cbind(yTrain,subjectTrain,xTrain)
testData = cbind(yTest,subjectTest,xTest)
finalData = rbind(trainingData,testData)
head(finalData)


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

#Extract only the relevant data
colNames  = colnames(finalData); 
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData = finalData[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

#Merge with activityType
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)
colNames  = colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names. 

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
head(colNames)
#Assign to finalData
colnames(finalData) = colNames;

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Create data without activityType
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

#create tidy data
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)
head(tidyData)

#Merge tidy data with activity
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)
head(tidyData)

#Export data
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')






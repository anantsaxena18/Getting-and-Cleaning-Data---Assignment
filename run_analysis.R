## 1. Merge the training and the test sets to create one data set ##

##Reading files##

features     = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/features.txt',header=FALSE)
activityType = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt',header=FALSE)
subject_Train = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
x_Train       = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt',header=FALSE)
y_Train       = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt',header=FALSE)

subject_Test = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
x_Test       = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt',header=FALSE)
y_Test       = read.table('D:/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt',header=FALSE)


## Assigning column names ##

colnames(activityType)  = c('activityId','activityType')
colnames(subject_Train)  = "subjectId"
colnames(x_Train)        = features[,2] 
colnames(y_Train)        = "activityId"

colnames(subject_Test) = "subjectId"
colnames(x_Test)       = features[,2] 
colnames(y_Test)       = "activityId"


## Creating mergeddatasets##

training_data = cbind(x_Train, y_Train,subject_Train)
test_Data = cbind(x_Test, y_Test, subject_Test)


## Combine training and test data to create a final data set ##

merged_data = rbind(training_data,test_Data)

## Creating vector for column names for merged_data ##

colNames  = colnames(merged_data)

## 2. Extract only the measurements on the mean and standard deviation for each measurement. ##

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

merged_data = merged_data[logicalVector==TRUE]

## 3. Use descriptive activity names to name the activities in the data set ##

merged_data = merge(merged_data,activityType,by='activityId',all.x=TRUE)

colNames  = colnames(merged_data)

## 4. Appropriately label the data set with descriptive activity names. ##

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

colnames(merged_data) = colNames

## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. ##

predata  = merged_data[,names(merged_data) != 'activityType']

tidy_Data = aggregate(predata[,names(predata) != c('activityId','subjectId')],by=list(activityId=predata$activityId,subjectId = predata$subjectId),mean)

tidy_Data = merge(tidy_Data,activityType,by='activityId',all.x=TRUE)

write.table(tidy_Data, 'D:/Coursera/Getting and Cleaning Data/tidy_Data.txt',row.names=TRUE,sep='\t')

View(tidy_Data)
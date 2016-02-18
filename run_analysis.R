library(httr)
library(data.table)
library(tidyr)
library(dplyr)



#Download the Data
filesPath <- "C:\\Coursera_Data Scientist\\data.csv\\UCI HAR Dataset"
setwd(filesPath)
if(!file.exists(".\\data")){dir.create(".\\data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile=".\\data\\Dataset.zip")

###Unzip DataSet to /data directory
unzip(zipfile=".\\data/Dataset.zip",exdir=".\\data")

#list of data files
filesPath <- "C:\\Coursera_Data Scientist\\data.csv\\UCI HAR Dataset\\data\\UCI HAR Dataset"
setwd(filesPath)          
list.files(getwd (), recursive=TRUE)


# Read subject files
dataSubjTrain <- read.table(file.path(filesPath, "train", "subject_train.txt"))
dataSubjTest  <- read.table(file.path(filesPath, "test" , "subject_test.txt" ))

# Read activity files
dataActTrain <- read.table(file.path(filesPath, "train", "Y_train.txt"))
dataActTest  <- read.table(file.path(filesPath, "test" , "Y_test.txt" ))

#Read data files.
dataTrain <- read.table(file.path(filesPath, "train", "X_train.txt" ))
dataTest  <- read.table(file.path(filesPath, "test" , "X_test.txt" ))
str(dataTrain)
str(dataTest)

#Merges the training and the test sets to create one data set
# for both Activity and Subject files this will merge the training and the test sets by row binding 
#and rename variables "subject" and "activityNum"
alldataSubj <- rbind(dataSubjTrain, dataSubjTest)
setnames(alldataSubj, "V1", "subject")

alldataAct<- rbind(dataActTrain, dataActTest)
setnames(alldataAct, "V1", "activityNum")

#combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)


# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- read.table(file.path(filesPath, "features.txt"))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- read.table(file.path(filesPath, "activity_labels.txt"))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubj, alldataAct)
dataTable <- cbind(alldataSubjAct, dataTable)


#2. Extracts only the measurements on the mean and standard deviation for each measurement
# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

#3. Uses descriptive activity names to name the activities in the data set
##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- arrange(dataAggr,subject,activityName)


#4. Appropriately labels the data set with descriptive variable names
#Names before
head(str(dataTable),2)

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
##write to text file on disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)

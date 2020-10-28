library(dplyr)

my_file <- "Getting_Cleaning_Assignment.zip"

# Checking for existance 
if (!file.exists(my_file)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, my_file, method="curl")
}  

# Checking for folder existance
if (!file.exists("UCI HAR Dataset")) { 
  unzip(my_file) 
}

#Assign each data to variables
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Merges the training and the test sets to create one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

#Extracts only the measurements on the mean and standard deviation for each measurement
My_TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

My_TidyData$code <- activities[My_TidyData$code, 2]

#Appropriately labels the data set with descriptive variable names

names(My_TidyData)[2] = "activity"
names(My_TidyData)<-gsub("Acc", "Accelerometer", names(My_TidyData))
names(My_TidyData)<-gsub("Gyro", "Gyroscope", names(My_TidyData))
names(My_TidyData)<-gsub("BodyBody", "Body", names(My_TidyData))
names(My_TidyData)<-gsub("Mag", "Magnitude", names(My_TidyData))
names(My_TidyData)<-gsub("^t", "Time", names(My_TidyData))
names(My_TidyData)<-gsub("^f", "Frequency", names(My_TidyData))
names(My_TidyData)<-gsub("tBody", "TimeBody", names(My_TidyData))
names(My_TidyData)<-gsub("-mean()", "Mean", names(My_TidyData), ignore.case = TRUE)
names(My_TidyData)<-gsub("-std()", "STD", names(My_TidyData), ignore.case = TRUE)
names(My_TidyData)<-gsub("-freq()", "Frequency", names(My_TidyData), ignore.case = TRUE)
names(My_TidyData)<-gsub("angle", "Angle", names(My_TidyData))
names(My_TidyData)<-gsub("gravity", "Gravity", names(My_TidyData))

#Final Data
finaldata <- My_TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(finaldata, "Final_Data.txt", row.name=FALSE)
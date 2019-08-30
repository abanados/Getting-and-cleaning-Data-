library(dplyr)

#Loading files
filename <- "Getting and Cleaning Data Course Project.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  


if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}
#Reading features
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))

#Reading activities
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

#Reading test data
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

#Reading train data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

#Step 1: Merges the training and the test sets to create one data set
xdata <- rbind(x_train, x_test)
ydata <- rbind(y_train, y_test)
Subjectdata <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subjectdata, ydata, xdata)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
tidydata <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
dim(tidydata)

#Step 3: Uses descriptive activity names to name the activities in the data set.
tidydata$code <- activities[tidydata$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.
names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

datafinal <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(datafinal, "datafinal.txt", row.name=FALSE)

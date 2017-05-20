'This script does the following:
1 Merges the training and the test sets to create one data set.
2 Extracts only the measurements on the mean and standard deviation for each measurement.
3 Uses descriptive activity names to name the activities in the data set
4 Appropriately labels the data set with descriptive variable names.
5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.'
library(dplyr) #used for manipulating data
library(stringr) # used for manipulating strings

#creating separate vectors out of each txt file for later binding
subj_test <- read.table("UCI_HAR_Dataset/test/subject_test.txt", col.names = "subjects")
x_test <- read.table("UCI_HAR_Dataset/test/X_test.txt")
y_test <- read.table("UCI_HAR_Dataset/test/y_test.txt", col.names = "activity")
subj_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt", col.names = "subjects")
x_train <- read.table("UCI_HAR_Dataset/train/X_train.txt")
y_train <- read.table("UCI_HAR_Dataset/train/y_train.txt", col.names = "activity")

#using the features file as columns for the variables (except subjects and activity)
features <- read.table("UCI_HAR_Dataset/features.txt")
colnames(x_test) <- as.character(t(features[,2]))
colnames(x_train) <- as.character(t(features[,2]))

#binding stuff together....
test <- cbind(subj_test,y_test, x_test)
train <- cbind(subj_train, y_train, x_train)
test_train <- rbind(test, train)

#selecting only mean and std (and subject/acitivity labels)
test_train_meanstd <- test_train[ , grepl( "subjects|activity|mean|std" , names( test_train ) ) ]

#labeling the activities in a readable way
test_train_meanstd$activity <- as.character(test_train_meanstd$activity)
test_train_meanstd$activity[test_train_meanstd$activity == "1"] <- "WALKING"
test_train_meanstd$activity[test_train_meanstd$activity == "2"] <- "WALKING_UPSTAIRS"
test_train_meanstd$activity[test_train_meanstd$activity == "3"] <- "WALKING_DOWNSTAIRS"
test_train_meanstd$activity[test_train_meanstd$activity == "4"] <- "SITTING"
test_train_meanstd$activity[test_train_meanstd$activity == "5"] <- "STANDING"
test_train_meanstd$activity[test_train_meanstd$activity == "6"] <- "LAYING"
test_train_meanstd$activity <- as.factor(test_train_meanstd$activity)

#summarizing the data into means for the 30 subjects and the 6 types of activity with the appropriate labels... and done!
test_train_meanstd <- group_by(test_train_meanstd, subjects, activity)
mean_table <- aggregate(test_train_meanstd[, 3:81], list(test_train_meanstd$subjects, test_train_meanstd$activity), mean)
mean_table <- rename(mean_table, subject = Group.1, activity = Group.2)
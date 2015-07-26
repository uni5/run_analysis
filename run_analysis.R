
### This program:
### 1. Merges the training and the test sets to create one data set.
### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
### 3. Uses descriptive activity names to name the activities in the data set
### 4. Appropriately labels the data set with descriptive variable names. 
### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)

######################################################################
### 1.Merges the training and the test sets to create one data set.###
######################################################################
# 'train/X_train.txt': Training set.
# 'test/X_test.txt': Test set.

# 'train/y_train.txt': Training labels.
# 'test/y_test.txt': Test labels.

# Reading a train set - created Xtrain.
Xtrain <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")

# Reading a test set - created Xtest.
Xtest <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")

# Combining Xtrain and Xtest into a new XTrainTest dataframe.
XTrainTest <- rbind(Xtrain, Xtest, deparse.level = 1, make.row.names=FALSE)

################################################################################################
### 2.Extracts only the measurements on the mean and standard deviation for each measurement.###
################################################################################################

features <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")

# Extracting positions of mean and standard deviation variables.
# Using grep function to search for variable names contaiting "mean" or "std" string.
# The function returns position (index) of matching variables within the features.txt file.

mean_positions <- grep("mean", features$V2, ignore.case=FALSE, value=FALSE, fixed=FALSE)
std_positions <- grep("std", features$V2, ignore.case=TRUE, value=FALSE, fixed=FALSE)

# Combining the mean_positions and std_positions into positions variable in an ascending order.
positions <- sort(c(mean_positions, std_positions))

# Subsetting XTrainTest dataframe - keeping only the mean and standard deviation variables 
# (using 'positions' variable to choose the appropriate column names within 'XTrainTest' dataframe).
mean_and_std_XTrainTest <- XTrainTest[, positions]

################################################################################
### 3.Uses descriptive activity names to name the activities in the data set.###
################################################################################

# 'activity_labels.txt': Activity names (2x6).
# 'train/y_train.txt': Training labels.
# 'test/y_test.txt': Test labels.

# Adding a subject column to the main data set "mean_and_std_XTrainTest", where each row corresponds to the subject' ID.
train_subject <-  read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
test_subject <-  read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
subject <- rbind(train_subject, test_subject, deparse.level = 1, make.row.names=FALSE)

colnames(subject) <- "Subject_ID"
mean_and_std_XTrainTest <- cbind(mean_and_std_XTrainTest, subject)

# Reading both files containing labels.
train_label <-  read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
test_label <- read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

# Combining them.
TrainTest_label <- rbind(train_label, test_label, deparse.level = 1, make.row.names=FALSE)

# Reading activity names file.
activity_labels <-  read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

# Adding a column to mean_and_std_XTrainTest.
mean_and_std_XTrainTest["Activity_Names"] <- "NA"

# Combining activity names with 2 label files above so that each number is assigned an activity.
# The result variable will be added as a column to the 'mean_and_std_XTrainTest' dataframe.
for (i in 1:10299) {
  mean_and_std_XTrainTest$Activity_Names[i] <- as.character(activity_labels$V2[match(TrainTest_label$V1[i], activity_labels$V1)])
}

###########################################################################
### 4.Appropriately labels the data set with descriptive variable names.###
###########################################################################

# Creating a new variable "var_names" to store the names of the variables.
var_names <- features$V2[ positions]

# Then, taking those variable names and creating neat labels. The labels are stored in "new_var_names".
new_var_names <- gsub( "[()]", "", as.character(var_names))
new_var_names <- gsub( "-", "_", new_var_names)
new_var_names <- sub( "tBody", "Time_of_Body_", new_var_names)
new_var_names <- sub( "tGravity", "Time_of_Gravity_", new_var_names)
new_var_names <- sub( "fBody", "Frequency_of_Body_", new_var_names)

new_var_names <- gsub( "Mag", "_Magnitude", new_var_names)
new_var_names <- gsub( "Jerk", "_Jerk", new_var_names)
new_var_names <- gsub( "BodyAcc", "Body_Acc", new_var_names)

new_var_names <- gsub( "BodyGyro", "Body_Gyro", new_var_names)
new_var_names <- gsub( "meanFreq", "mean_frequency", new_var_names)
new_var_names <- gsub( "Acc", "Acceleration", new_var_names)
new_var_names <- gsub( "Gyro", "Gyroscope", new_var_names)

# Renaming the names of the columns in the main dataset "mean_and_std_XTrainTest".
colnames(mean_and_std_XTrainTest)[1:79] <- new_var_names

#################################################################################
### 5.From the data set in step 4, creates a second, independent tidy data set###
###   with the average of each variable for each activity and each subject.######
#################################################################################

tidy_data_set <- aggregate(mean_and_std_XTrainTest[1:79], by=list(mean_and_std_XTrainTest$Subject_ID, mean_and_std_XTrainTest$Activity_Names), FUN="mean")
                             
# Renaming the first 2 columns in the tidy data set.
colnames(tidy_data_set)[1:2] <- c("Activity", "Subject_ID")

# Writing the tidy_data_set into a text file.
write.table(tidy_data_set, file="tidy_data_set.txt", row.names=FALSE)

###DONE!###
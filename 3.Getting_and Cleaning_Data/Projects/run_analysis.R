# Getting and Cleaning Data Project John Hopkins Coursera
#Getting and Cleaning data according to following five step
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Load package and data (The data was already downloaded onto local directory.)
library(dplyr)
library(data.table)
library(reshape2)

x_train <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/train/Y_train.txt")
SubjectTrain <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/test/Y_test.txt")
SubjectTest <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/test/subject_test.txt")

features <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/features.txt", col.names = c("index", "FeatureName"))
ActivityLabels <- read.table("3.Getting_and Cleaning_Data/Projects/UCI HAR Dataset/activity_labels.txt", col.names = c("classindex", "ActivityName"))


#1.Merges the training and the test sets to create one data set.
train <- cbind(SubjectTrain, y_train, x_train)
test <- cbind(SubjectTest, y_test, x_test)
MergedDataset <- rbind(train, test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
names <- rbind(c(0, 'subject'),c(0, "Activity") , sapply(features, as.character))
names <- as.data.frame(names)
FeatureNames <- names$FeatureName
names(MergedDataset) <- FeatureNames

MergedDataset[["Activity"]] <- factor(MergedDataset[,"Activity"],
                                      levels = ActivityLabels[["classindex"]],
                                      labels = ActivityLabels[["ActivityName"]])

#Chouse column by keyword 
extractDataset <- MergedDataset[,grepl("subject|Activity|mean\\(\\)|std\\(\\)", colnames(MergedDataset))]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
average_measurements <- colnames(extractDataset[,3:68])
#rechape data according to subject and Activity
meltData <- melt(extractDataset, id=c("subject", "Activity"),measure.vars = average_measurements)
#calcurate average of each variable for each activity and each subject
TidyData <- dcast(meltData, formula=subject + Activity ~variable, fun.aggregate=mean)

#output data file
data.table::fwrite(x =TidyData, file = "TidyData.txt", row.names=FALSE)

# Getting and Cleaning Data Project

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.

# Load Packages and get the Data
packages <- "data.table"
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# read activity_labels file and features file and give them corresponding labels
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
# find the columns that contains mean or std in feature Names(use \\(\\) to speficy the single word)
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames]) #?\\(\\)
# extract the columns by the index return from above statement
measurements <- features[featuresWanted, featureNames]
# gsub(old, new, data): substract the data from () to ""(no space between)
measurements <- gsub('[()]', '', measurements)

# read train datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))
# extract the train datasets associating with the index returned by featuresWanted
train = train[, featuresWanted, with = FALSE] #?with
# setnames(data, old, new): change variable names from V-number to names returned by measurements
setnames(train, colnames(train), measurements)
# test: head(train)
# read train dataset and set corresponding column names
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
# merge three data tables(they should have the same amount of row)
train <- cbind(trainSubjects, trainActivities, train)

# read test datasets
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))
# extract the test datasets associatind with the index returned by featuresWanted
test = test[, featuresWanted, with = FALSE]
# setnames(data, old, new): change variable names from V-number to names returned by measurements
setnames(test, colnames(test), measurements)
# read test dataset and set corresponding column names
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
# merge three data tables
test <- cbind(testSubjects, testActivities, test)

# merge two datasets(train and test)
combined <- rbind(train, test)

# Convert classLabels to activityName basically. More explicit. 
# the old combined[["Activity"]] is a vector of numbers from 1 to 6, 
# according to the level and labels in activity Labels data table,
# it maps to the number to a specific activity label
# so we can assign the new activity labels to the combined activity columns
combined[["Activity"]] <- factor(combined[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])
# convert from an integer vector to a factor using as.factor method
combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])
# melt and dcast the combined data table
combined <- melt(combined, id = c("SubjectNum", "Activity"))
combined <- dcast(combined, SubjectNum + Activity ~ variable, fun.aggregate = mean) #?fun.aggregage
# write the tidy dataset to a new file
fwrite(combined, file = "Les3project_tidyData.txt")

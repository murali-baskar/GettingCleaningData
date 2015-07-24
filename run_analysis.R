#########################################################
# Load the reshape library to create the tidy dataset at the end
library(reshape2)
#########################################################

filename <- "getdata_dataset.zip"
## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

file.rename("UCI HAR Dataset","UCI_HAR_Dataset")

#########################################################
# Data extraction step for the features and activity data
#########################################################
## Extract the features data
features <- read.table("UCI_HAR_Dataset/features.txt")

## Convert the data frame to character type
features[,2] <- as.character(features[,2])

## Since the requirement is to find the mean of only the mean and 
## standard deviation variables find all the column names that 
## contain the words mean or std
featuresStdMean <- grep(".*mean.*|.*std.*", features[,2])
featuresStdMean.names <- features[featuresStdMean,2]

## Format the column names to a readable form
featuresStdMean.names = gsub('-mean', 'Mean', featuresStdMean.names)
featuresStdMean.names = gsub('-std', 'Std.Deviation', featuresStdMean.names)
featuresStdMean.names <- gsub('[-()]', '', featuresStdMean.names)

## Extract the activity labels
activityLabels <- read.table("UCI_HAR_Dataset/activity_labels.txt")

## Convert the data frame to character type
activityLabels[,2] <- as.character(activityLabels[,2])

#########################################################
# Data extraction step for the test data
#########################################################
# Creating the data frame as suggested in the Discussion pages of the coursera for this assignment
testData <- read.table("UCI_HAR_Dataset/test/X_test.txt", nrows=5)
testData.classes <- lapply(testData, class)
testData <- read.table("UCI_HAR_Dataset/test/X_test.txt", colClasses=testData.classes)

# Select only the mean and standard deviation variables
testData <- testData[,featuresStdMean]

# Combine the subjects, activities and the test data
testActivities <- read.table("UCI_HAR_Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, testData)

#########################################################
# Data extraction step for the train data
#########################################################

# Creating the data frame as suggested in the Discussion pages of the coursera for this assignment
trainData <- read.table("UCI_HAR_Dataset/train/X_train.txt", nrows=5)
trainData.classes <- lapply(trainData, class)
trainData <- read.table("UCI_HAR_Dataset/train/X_train.txt", colClasses=trainData.classes)

# Select only the mean and standard deviation variables
trainData <- trainData[,featuresStdMean]

# Combine the subjects, activities and the train data
trainActivities <- read.table("UCI_HAR_Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, trainData)

#########################################################
# Merge the data sets test and train
#########################################################
mergedData <- rbind(test,train)

# Add labels to the merged datasets
colnames(mergedData) <- c("subject", "activity", featuresStdMean.names)


#########################################################
# Convert activities and subjects to factors
# Step 5: Tidy data set
#########################################################
mergedData$activity <- factor(mergedData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
mergedData$subject <- as.factor(mergedData$subject)

mergedData.melted <- melt(mergedData, id = c("subject", "activity"))
mergedData.mean <- dcast(mergedData.melted, subject + activity ~ variable, mean)

write.table(mergedData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

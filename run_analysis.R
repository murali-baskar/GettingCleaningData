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
## Step 1a: 
## Extract the test data using read.table
## Add the subject and activity labels to the test data
#########################################################
testData <- read.table("UCI_HAR_Dataset/test/X_test.txt", nrows=5)
testData.classes <- lapply(testData, class)
testData <- read.table("UCI_HAR_Dataset/test/X_test.txt", colClasses=testData.classes)
testActivities <- read.table("UCI_HAR_Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, testData)

#########################################################
## Step 1b: 
## Extract the train data using read.table
## Add the subject and activity labels to the train data
#########################################################
trainData <- read.table("UCI_HAR_Dataset/train/X_train.txt", nrows=5)
trainData.classes <- lapply(trainData, class)
trainData <- read.table("UCI_HAR_Dataset/train/X_train.txt", colClasses=trainData.classes)
trainActivities <- read.table("UCI_HAR_Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, trainData)

mergedData <- rbind(test,train)

#########################################################
## Step 2 and 3:
## Load the activity labels and features
## Extract only the required features (mean and standard deviation)
## Descriptive activity names to the activities
#########################################################
activityLabels <- read.table("UCI_HAR_Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("UCI_HAR_Dataset/features.txt")
features[,2] <- as.character(features[,2])

#########################################################
# Step 4: Label with descriptive variable names
# Extract only the data on mean and standard deviation
#########################################################
featuresStdMean <- grep(".*mean.*|.*std.*", features[,2])
featuresStdMean.names <- features[featuresStdMean,2]
featuresStdMean.names = gsub('-mean', 'Mean', featuresStdMean.names)
featuresStdMean.names = gsub('-std', 'Std.Deviation', featuresStdMean.names)
featuresStdMean.names <- gsub('[-()]', '', featuresStdMean.names)

# merge datasets and add labels
allData <- mergedData[,featuresStdMean]
colnames(allData) <- c("subject", "activity", featuresStdMean.names)


#########################################################
# Convert activities and subjects to factors
# Step 5: Tidy data set
#########################################################
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

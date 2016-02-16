//Function to convert raw data to tidy data
GetDataPorj <- function()
{

  ## Download and unzip the dataset:
  if (!file.exists("raw_dataset.zip")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, "raw_dataset.zip")
  }  
  if (!file.exists("UCI HAR Dataset")) { 
    unzip("raw_dataset.zip") 
  }
  
  # Load activity labels + features
  activityLabels <- read.table("UCI HAR Dataset\\activity_labels.txt")
  activityLabels[,2] <- as.character(activityLabels[,2])
  

   features <- read.table("UCI HAR Dataset\\features.txt")
   features[,2] <- as.character(features[,2])
   
   
  # 
  # # Extract only the data on mean and standard deviation
   featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
   featuresWanted.names <- features[featuresWanted,2]
   
  featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
  featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
  featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)
   
   
  
  # # Load the train datasets
   train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
   trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
   trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
   train <- cbind(trainSubjects, trainActivities, train)

  # Load the test datasets
  test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
  testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  #Bind the y_test and subject_test data
  test <- cbind(testSubjects, testActivities, test)
  # 
  
  # # merge datasets and add labels
   TidyData <- rbind(train, test)
   colnames(TidyData) <- c("subject", "activity", featuresWanted.names)
   
   
  # # turn activities & subjects into factors
   TidyData$activity <- factor(TidyData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
   TidyData$subject <- as.factor(TidyData$subject)
  # 
   TidyData.melted <- melt(TidyData, id = c("subject", "activity"))
   TidyData.mean <- dcast(TidyData.melted, subject + activity ~ variable, mean)
  # 
   write.table(TidyData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
}

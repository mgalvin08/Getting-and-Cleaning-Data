##################################################################################################
#                                           GET DATA                                             #
##################################################################################################

# Downloading data
setwd("~/Coursera Data Science/Getting and Cleaning")
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./Dataset.zip", method = "curl")

#Unzipping file
unzip(zipfile = "./Dataset.zip", exdir = "./data")

##################################################################################################
#                                            READ DATA                                           #
##################################################################################################
# Getting the lists of unzipped files
setwd("./data") 

list.files("UCI HAR Dataset")

#Reading data files into variables from the "UCI HAR Dataset" directory
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

#Listing files in the "test" directory
list.files("UCI HAR Dataset/test")

#Reading data files into variables from the "test" directory
subjectTest <-read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <-read.table("UCI HAR Dataset/test/X_test.txt")
y_test <-read.table("UCI HAR Dataset/test/y_test.txt")

#Listing files in the "train" directory
list.files("UCI HAR Dataset/train")

#Reading data files into variables from the "train" directory
subjectTrain <-read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <-read.table("UCI HAR Dataset/train/X_train.txt")
y_train <-read.table("UCI HAR Dataset/train/y_train.txt")

##################################################################################################
#                                            ADD HEADERS                                         #
##################################################################################################
# Creating column names

colnames(subjectTest) <- "subjectId"
colnames(X_test) <- features[,2] 
colnames(y_test) <-"activityId"

colnames(subjectTrain) <- "subjectId"
colnames(X_train) <- features[,2] 
colnames(y_train) <- "activityId"

colnames(activityLabels) <- c('activityId', 'activityType')
##################################################################################################
# 1. Merges the training and the test sets to create one data set.                               #
##################################################################################################
# Merging columns to get data from all data
combine_train <- cbind(y_train, subjectTrain, X_train)
combine_test <- cbind(y_test, subjectTest, X_test)
Data <- rbind(combine_train, combine_test)


##################################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.     #
##################################################################################################
# Read column names
colNames <- colnames(Data)

#Mean and standard deviation
mean_std <- (grepl("activityId" , colNames) | 
                  grepl("subjectId" , colNames) | 
                  grepl("mean.." , colNames) | 
                  grepl("std.." , colNames) 
)
# Making nessesary subset from Data
setMeanAndStd <- Data[ , mean_std == TRUE]

##################################################################################################
# 3. Uses descriptive activity names to name the activities in the data set                      #
##################################################################################################

setActivityNames <- merge(activityLabels, setMeanAndStd,
                              by.x = 'activityId', by.y = 'activityId')


##################################################################################################
# 4. From the data set in step 4, creates a second, independent tidy data set with the average   #
#    of each variable for each activity and each subject.                                        #
##################################################################################################
#Creating second tidy set
tidySet2 <- aggregate(. ~subjectId + activityId + activityType, setActivityNames, mean)
tidySet2 <- tidySet2[order(tidySet2$subjectId, tidySet2$activityId),]

#Writing second tidy data set in txt file
write.table(tidySet2, "tidySet2.txt", row.name=FALSE)

